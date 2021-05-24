-module(emqx_widget_transform).

-include_lib("syntax_tools/include/merl.hrl").

-export([parse_transform/2]).

parse_transform(Forms, _Options) ->
    AST = trans(proplists:delete(eof, Forms)),
    debug_print(AST),
    AST.

-ifdef(WIDGET_DEBUG).
debug_print(AST) ->
    merl:print(AST).
-else.
debug_print(_AST) ->
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
            {fix_attrs(),
             [],
             fix_funcs() ++ [SpecFun, SchemaFunc]};
        _ ->
            %io:format("---other form: ~p~n", [Form]),
            {[], [Form], []}
    end.


fix_attrs() ->
    [ ?Q("-export([emqx_widget_spec/0]).")
    , ?Q("-export([structs/0, fields/1, translations/0, translation/1]).")
    , ?Q("-behaviour(hocon_schema).")
    ].
fix_funcs() ->
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
