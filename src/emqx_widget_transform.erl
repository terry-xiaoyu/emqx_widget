-module(emqx_widget_transform).

-include_lib("syntax_tools/include/merl.hrl").

-export([parse_transform/2]).

parse_transform(Forms, _Opts) ->
    ModName = hd([Mod || {attribute, _, module, Mod} <- Forms]),
    AST = trans(ModName, proplists:delete(eof, Forms)),
    debug_print(ModName, AST),
    AST.

-ifdef(WIDGET_DEBUG).

debug_print(ModName, Ts) ->
    {ok, Io} = file:open("./" ++ atom_to_list(ModName) ++ ".trans.erl", [write]),
    do_debug_print(Io, Ts),
    file:close(Io).

do_debug_print(Io, Ts) when is_list(Ts) ->
    lists:foreach(fun(T) -> do_debug_print(Io, T) end, Ts);
do_debug_print(Io, T) ->
    io:put_chars(Io, erl_prettypr:format(merl:tree(T))),
    io:nl(Io).
-else.
debug_print(_ModName, _AST) ->
    ok.
-endif.

trans(ModName, Forms) ->
    forms(ModName, Forms) ++ [erl_syntax:revert(erl_syntax:eof_marker())].

forms(ModName, [F0 | Fs0]) ->
    case form(ModName, F0) of
        {CurrForm, AppendedForms} ->
            CurrForm ++ forms(ModName, Fs0) ++ AppendedForms;
        {AHeadForms, CurrForm, AppendedForms} ->
            AHeadForms ++ CurrForm ++ forms(ModName, Fs0) ++ AppendedForms
    end;
forms(_, []) -> [].

form(ModName, Form) ->
    case Form of
        ?Q("-emqx_widget_api_path('@Path').") ->
            {fix_spec_attrs() ++ fix_api_attrs(erl_syntax:concrete(Path)) ++ fix_api_exports(),
             [],
             fix_spec_funcs(ModName) ++ fix_api_funcs()};
        _ ->
            %io:format("---other form: ~p~n", [Form]),
            {[], [Form], []}
    end.

fix_spec_attrs() ->
    [ ?Q("-export([emqx_widget_schema/0]).")
    ].
fix_spec_funcs(ModName) ->
    SchemaModName = list_to_atom(atom_to_list(ModName) ++ "_schema"),
    [ erl_syntax:revert(?Q("emqx_widget_schema() -> '@SchemaModName@'."))
    ].

fix_api_attrs(Path0) ->
    BaseName = filename:basename(Path0),
    Path = "/" ++ BaseName,
    [erl_syntax:revert(
        erl_syntax:attribute(?Q("rest_api"), [
            erl_syntax:abstract(#{
                name => list_to_atom(Name ++ "_log_tracers"),
                method => Method,
                path => mk_path(Path, WithId),
                func => Func,
                descr => Name ++ " the " ++ BaseName})]))
       || {Name, Method, WithId, Func} <- [
            {"list", 'GET', noid, api_get_all},
            {"get", 'GET', id, api_get},
            {"update", 'PUT', id, api_put},
            {"delete", 'DELETE', id, api_delete}]].

fix_api_exports() ->
    [?Q("-export([api_get_all/2, api_get/2, api_put/2, api_delete/2]).")].

fix_api_funcs() ->
    [?Q("api_get_all(_Binding, _Params) ->
            {200, #{code => 0, data =>
                [format_data(Data) || Data <- emqx_widget:list_instances_verbose()]}}."),
     ?Q("api_get(#{id := Id}, _Params) ->
            case emqx_widget:get_instance(Id) of
                {ok, Data} ->
                    {200, #{code => 0, data => format_data(Data)}};
                {error, not_found} ->
                    {404, #{code => 102, message => not_found}}
            end."),
     ?Q("api_put(#{id := Id}, Params) ->
            JsonStr = jsx:encode([{<<\"id\">>, list_to_binary(Id)} | Params]),
            case emqx_widget:parse_config(JsonStr) of
                {ok, InstId, WidgetType, Config} ->
                    case emqx_widget:update(InstId, WidgetType, Config) of
                        {ok, Data} ->
                            {200, #{code => 0, data => format_data(Data)}};
                        {error, Reason} ->
                            {500, #{code => 102, message =>
                                iolist_to_binary(io_lib:format(\"~p\", [Reason]))
                            }}
                    end;
                {error, Reason} ->
                    {400, #{code => 108, message =>
                        iolist_to_binary(io_lib:format(\"~p\", [Reason]))}}
            end."),
     ?Q("api_delete(#{id := Id}, _Params) ->
            case emqx_widget:remove(Id) of
                ok -> {200, #{code => 0, data => #{}}};
                {error, Reason} ->
                    {500, #{code => 102, message =>
                        iolist_to_binary(io_lib:format(\"~p\", [Reason]))}}
            end.")
    ].

mk_path(Path, id) -> Path ++ "/:id";
mk_path(Path, noid) -> Path.
