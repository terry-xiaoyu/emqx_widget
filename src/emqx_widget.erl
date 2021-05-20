%%--------------------------------------------------------------------
%% Copyright (c) 2020-2021 EMQ Technologies Co., Ltd. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------

-module(emqx_widget).

-include("emqx_widget.hrl").
-include("emqx_widget_utils.hrl").

-compile({no_auto_import,
          [ get/1
          ]}).

-export([ get/1
        , list/0
        ]).

-export([ discover_widget_mods/0
        , is_widget_mod/1
        ]).

-export([ query_success/1
        , query_failed/1
        ]).

-define(EXT, "*.wgt").

%% when calling emqx_widget_instance:start/1
-callback on_start(instance_id(), widget_config()) ->
    {ok, widget_state()} | {error, Reason :: term()}.

%% when calling emqx_widget_instance:stop/1
-callback on_stop(instance_id(), widget_state()) -> term().

%% when calling emqx_widget_instance:query/3
-callback on_query(instance_id(), Request :: term(), after_query(), widget_state()) -> term().

%% when calling emqx_widget_instance:health_check/2
-callback on_health_check(instance_id(), widget_state()) ->
    {ok, widget_state()} | {error, Reason:: term(), widget_state()}.

%% load specs and return the loaded widgets this time.
-spec list() -> list(widget_spec()).
list() ->
    [get_spec(Mod) || Mod <- discover_widget_mods()].

-spec get(module()) -> {ok, widget_spec()} | {error, not_found}.
get(Mod) ->
    case is_widget_mod(Mod) of
        true -> {ok, get_spec(Mod)};
        false -> {error, not_found}
    end.

-spec get_spec(module()) -> widget_spec().
get_spec(Mod) ->
    maps:put(<<"widget_type">>, Mod, Mod:emqx_widget_spec()).

-spec discover_widget_mods() -> list(module()).
discover_widget_mods() ->
    [Mod || {Mod, _} <- code:all_loaded(), is_widget_mod(Mod)].

-spec is_widget_mod(module()) -> boolean().
is_widget_mod(Mod) ->
    erlang:function_exported(Mod, emqx_widget_spec, 0).

-spec query_success(after_query()) -> ok.
query_success(undefined) -> ok;
query_success({{OnSucc, Args}, _}) ->
    safe_apply(OnSucc, Args).

-spec query_failed(after_query()) -> ok.
query_failed(undefined) -> ok;
query_failed({_, {OnFailed, Args}}) ->
    safe_apply(OnFailed, Args).

%% =================================================================================

safe_apply(Func, Args) ->
    ?SAFE_CALL(erlang:apply(Func, Args)).

