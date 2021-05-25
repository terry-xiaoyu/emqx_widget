-module(emqx_widget_transform).

-include_lib("syntax_tools/include/merl.hrl").

-export([parse_transform/2]).

parse_transform(Forms, _Opts) ->
    ModName = hd([Mod || {attribute, _, module, Mod} <- Forms]),
    io:format("----------- ModName: ~p~n", [ModName]),
    AST = trans(proplists:delete(eof, Forms)),
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

trans(Forms) ->
    forms(Forms) ++ [erl_syntax:revert(erl_syntax:eof_marker())].

forms([F0 | Fs0]) ->
    case form(F0) of
        {CurrForm, AppendedForms} ->
            CurrForm ++ forms(Fs0) ++ AppendedForms;
        {AHeadForms, CurrForm, AppendedForms} ->
            AHeadForms ++ CurrForm ++ forms(Fs0) ++ AppendedForms
    end;
forms([]) -> [].

form(Form) ->
    case Form of
        ?Q("-emqx_widget_spec('@File').") ->
            %io:format("----widget attr: ~p, form: ~p~n", [File, Form]),
            {ok, Widget} = hocon:load(erl_syntax:concrete(File), #{format => map}),
            SpecFun = erl_syntax:revert(?Q("emqx_widget_spec() -> _@Widget@.")),
            SchemaBody = to_schema("config", maps:get(<<"fields">>, Widget)),
            SchemaFunc =
                erl_syntax:revert(?Q(
                    "fields(\"config\") -> _@SchemaBody;"
                    "fields(\"id\") -> fix_str_fields(\"id\");"
                    "fields(\"widget_type\") -> fix_str_fields(\"widget_type\").")),
            %merl:print(SchemaFunc),
            {fix_spec_attrs(),
             [],
             fix_spec_funcs() ++ [SpecFun, SchemaFunc]};
        ?Q("-emqx_widget_api_path('@Path').") ->
            {fix_api_attrs(erl_syntax:concrete(Path)) ++ fix_api_exports(),
             [],
             fix_api_funcs()};
        _ ->
            %io:format("---other form: ~p~n", [Form]),
            {[], [Form], []}
    end.

fix_spec_attrs() ->
    [ ?Q("-export([emqx_widget_spec/0]).")
    , ?Q("-export([structs/0, fields/1, translations/0, translation/1]).")
    , ?Q("-behaviour(hocon_schema).")
    ].
fix_spec_funcs() ->
    [ ?Q("structs() -> [\"id\", \"widget_type\", \"config\"].")
    , ?Q("translations() -> [\"config\"].")
    , ?Q("translation(\"config\") -> log_tracer:config_transform(\"config\").")
    , ?Q("fix_str_fields(Str) ->"
         "  [fun"
         "      (mapping) -> Str;"
         "      (type) -> typerefl:string();"
         "      (_) -> undefined"
         "   end].")
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
                    {400, #{code => 108, message => iolist_to_binary(io_lib:format(\"~p\", [Reason]))}}
            end."),
     ?Q("api_delete(#{id := Id}, _Params) ->
            case emqx_widget:remove(Id) of
                ok -> {200, #{code => 0, data => #{}}};
                {error, Reason} ->
                    {500, #{code => 102, message => iolist_to_binary(io_lib:format(\"~p\", [Reason]))}}
            end.")
    ].

mk_path(Path, id) -> Path ++ "/:id";
mk_path(Path, noid) -> Path.

to_schema(Path, Spec) ->
    erl_syntax:list([erl_syntax:tuple([erl_syntax:string(str(Key)),
                                       fields(path_join(Path, str(Key)), SubSpec)])
                     || {Key, SubSpec} <- maps:to_list(Spec)]).

fields(Path, Spec) ->
    SubFields = type(Spec),
    Default = default(Spec),
    Validators = validators(Spec),
    ?Q("fun(mapping) -> _@Path@;"
       "   (type) -> _@SubFields;"
       "   (default) -> _@Default;"
       "   (validator) -> _@Validators;"
       "   (_) -> undefined"
       " end").

type(#{<<"type">> := <<"string">>, <<"pattern">> := RE}) ->
    ?Q("typerefl:regexp_binary(_@RE@)");
type(#{<<"type">> := <<"string">>}) ->
    ?Q("typerefl:string()");
type(#{<<"type">> := <<"long_string">>} = Spec) ->
    type(Spec#{<<"type">> => <<"string">>});
type(#{<<"type">> := <<"password">>} = Spec) ->
    type(Spec#{<<"type">> => <<"string">>});
type(#{<<"type">> := <<"file">>} = Spec) ->
    type(Spec#{<<"type">> => <<"string">>});
type(#{<<"type">> := <<"size">>} = Spec) ->
    type(Spec#{<<"type">> => <<"string">>, <<"pattern">> => "^[. 0-9]+(B|KB|MB|GB)$"});
type(#{<<"type">> := <<"number">>}) ->
    ?Q("typerefl:union([typerefl:integer(), typerefl:float()])");
type(#{<<"type">> := <<"integer">>}) ->
    ?Q("typerefl:integer()");
type(#{<<"type">> := <<"float">>}) ->
    ?Q("typerefl:float()");
type(#{<<"type">> := <<"boolean">>}) ->
    ?Q("typerefl:boolean()");
type(#{<<"type">> := <<"enum">>}) ->
    %% an enum can only consist of atoms (converted from string) or numbers
    ?Q("typerefl:union([typerefl:atom(), typerefl:number()])");
type(#{<<"type">> := <<"array">>, <<"items">> := Items}) ->
    EItems = erl_syntax:list([type(I) || I <- Items]),
    ?Q("typerefl:union(_@EItems)");
type(#{<<"type">> := Object, <<"fields">> := Fields})
        when Object == <<"object">>; Object == <<"table_object">> ->
    EFields = erl_syntax:list([begin
        TV = type(V),
        RTVs = ?Q("[{strict, value, _@TV}, {fuzzy, typerefl:atom(), typerefl:term()}]"),
        RichMapTV = ?Q("typerefl:map(_@RTVs)"),
        ?Q("{strict, _@K@, _@RichMapTV}")
    end || {K, V} <- maps:to_list(Fields)]),
    ?Q("typerefl:map([{fuzzy, typerefl:atom(), typerefl:term()} | _@EFields])").

validators(#{<<"type">> := DataType} = Spec)
        when DataType =:= <<"string">>;
             DataType =:= <<"array">>;
             DataType =:= <<"integer">>;
             DataType =:= <<"float">>;
             DataType =:= <<"number">> ->
    Type = binary_to_atom(DataType, latin1),
    case {maps:find(max_field(Type), Spec), maps:find(min_field(Type), Spec)} of
        {error, error} -> ?Q("[]");
        {error, {ok, Min}} ->
            ?Q("[emqx_widget_validator:min(_@Type@,_@Min@)]");
        {{ok, Max}, error} ->
            ?Q("[emqx_widget_validator:max(_@Type@,_@Max@)]");
        {{ok, Max}, {ok, Min}} ->
            ?Q("[emqx_widget_validator:max(_@Type@,_@Max@),"
               " emqx_widget_validator:min(_@Type@,_@Min@)]")
    end;
validators(#{<<"type">> := <<"enum">>, <<"enum">> := Enum0}) ->
    Enum = [enum_item(E) || E <- Enum0],
    ?Q("[emqx_widget_validator:enum(_@Enum@)]");
validators(_Spec) ->
    ?Q("[]").

default(#{<<"default">> := Default}) ->
    ?Q("_@Default@");
default(_Spec) ->
    ?Q("undefined").

enum_item(E) when is_binary(E) ->
    binary_to_atom(E, latin1);
enum_item(E) when is_list(E) ->
    list_to_atom(E);
enum_item(E) when is_number(E) ->
    E.

max_field(Type) when Type =:= array; Type =:= string ->
    <<"max_len">>;
max_field(_) ->
    <<"max">>.
min_field(Type) when Type =:= array; Type =:= string ->
    <<"min_len">>;
min_field(_) ->
    <<"min">>.

path_join(A, B) ->
    A ++ "." ++ B.

str(B) when is_binary(B) -> binary_to_list(B);
str(S) when is_list(S) -> S.
