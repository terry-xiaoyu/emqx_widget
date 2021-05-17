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

-compile({no_auto_import,
          [ get/1
          ]}).

-export([ get/1
        , list/0
        , discover_widget_mods/0
        , is_widget_mod/1
        ]).

-define(EXT, "*.wgt").

-type after_called() :: {OnSuccess :: after_called_fun(), OnFailed :: after_called_fun()} |
    undefined.

%% when calling emqx_widget_instance:start/1
-callback on_start(instance_id(), widget_config()) ->
    {ok, widget_state()} | {error, Reason :: term()}.

%% when calling emqx_widget_instance:stop/1
-callback on_stop(instance_id(), widget_state()) -> term().

%% when calling emqx_widget_instance:call/1,2
-callback on_call(instance_id(), widget_state()) -> term().

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
    Mod:emqx_widget_spec().

-spec discover_widget_mods() -> list(module()).
discover_widget_mods() ->
    [Mod || {Mod, _} <- code:all_loaded(), is_widget_mod(Mod)].

-spec is_widget_mod(module()) -> boolean().
is_widget_mod(Mod) ->
    erlang:function_exported(Mod, emqx_widget_spec, 0).

%% =================================================================================

load_spec_file(File) ->
    case hocon:load(File, #{format => map, [duration, bytesize, percent]}) of
        {ok, Widget0} ->
            case emqx_widget_validator:validate_spec(Widget0) of
                ok -> Widget0;
                {error, Reason} ->
                    logger:error("validate widget ~p failed: ~p", [File, Reason])
            end;
        {error, Reason} ->
            logger:error("load widget from ~p failed: ~p", [File, Reason])
    end.

