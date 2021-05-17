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

-export([ add_search_paths/1
        , get_search_paths/0
        , load/0
        , load/1
        , unload/0
        , unload/1
        , get/1
        , list/0
        ]).

-define(T_RETRY, 60000).

-type args() :: term().
-type widget_name() :: binary().
-type instance_id() :: binary().
-type widget_config() :: map().
-type widget_state() :: term().

-type after_called_fun() :: {fun((widget_state(), args()) -> ok), args()}.

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

-spec add_search_paths(list(string())) -> ok.
add_search_paths(PathList) ->
    application:set_env(emqx_widget, search_paths, get_search_paths() ++ PathList).

-spec get_search_paths() -> list(string()).
get_search_paths() ->
    application:get_env(emqx_widget, search_paths, []).

%% load specs and return the loaded widgets this time.
-spec load() -> {ok, lists(widget())} | {error, Reason :: term()}.
load() ->
    LoadedWidgets =
        lists:foldl(fun(Path, Loaded) ->
            Loaded ++ load_spec_files(filelib:wildcard(filename:join([Path, "*.wgt"]))),
        end, [], get_search_paths()),
    ets:insert(?WIDGET_TAB, [{Name, Widget} || #{name := Name} = Widget <- LoadedWidgets]).

load_spec_files(Files) ->
    list:foldr(fun(File, Loaded) ->
            case load_spec_file(File) of
                {ok, Widget0} ->
                    case emqx_widget_validator:validate_spec(Widget0) of
                        ok -> [Widget | Loaded];
                        {error, Reason} ->
                            logger:error("validate widget ~p failed: ~p", [File, Reason])
                    end;
                {error, Reason} ->
                    logger:error("load widget from ~p failed: ~p", [File, Reason])
            end
        end, [], Files).

load_spec_file(File) ->
    hocon:load(File, #{format => map, [duration, bytesize, percent]}).
