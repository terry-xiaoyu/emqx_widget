-module(emqx_widget_transform).

-include_lib("syntax_tools/include/merl.hrl").

-export([parse_transform/2]).

parse_transform(Forms, _Options) ->
    trans(proplists:delete(eof, Forms)).
    %io:format("the result form: ~p~n", [A]).

trans(Forms) ->
    forms(Forms) ++ [erl_syntax:revert(erl_syntax:eof_marker())].

forms([F0|Fs0]) ->
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
            Fun = ?Q("emqx_widget_spec() -> _@Widget@."),
            RFun = erl_syntax:revert(Fun),
            {[?Q("-export([emqx_widget_spec/0]).")],
             [],
             [RFun]};
        _ ->
            %io:format("---other form: ~p~n", [Form]),
            {[], [Form], []}
    end.


% load_spec_file(File) ->
%     case hocon:load(File, #{format => map, convert => [duration, bytesize, percent]}) of
%         {ok, Widget0} ->
%             case emqx_widget_validator:validate_spec(Widget0) of
%                 ok -> Widget0;
%                 {error, Reason} ->
%                     logger:error("validate widget ~p failed: ~p", [File, Reason])
%             end;
%         {error, Reason} ->
%             logger:error("load widget from ~p failed: ~p", [File, Reason])
%     end.