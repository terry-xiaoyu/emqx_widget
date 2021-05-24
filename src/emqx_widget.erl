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

%% APIs for widget types

-export([ get_type/1
        , list_types/0
        , list_types_verbose/0
        ]).

-export([ discover_widget_mods/0
        , is_widget_mod/1
        , call_instance/2
        ]).

-export([ query_success/1
        , query_failed/1
        ]).

%% APIs for instances

%% Sync widget instances and files
%% provisional solution: rpc:multical to all the nodes for creating/updating/removing
%% todo: replicate operations
-export([ create/3 %% store the config and start the instance
        , create_dry_run/3 %% run start/2, health_check/2 and stop/1 sequentially
        , update/2 %% update the config, stop the old instance and start the new one
        , remove/1 %% remove the config and stop the instance
        ]).

%% Calls to the callback module with current widget state
%% They also save the state after the call finished (except query/2,3).
-export([ restart/1  %% restart the instance.
        , health_check/1 %% verify if the widget is working normally
        , stop/1   %% stop the instance
        , query/2  %% query the instance
        , query/3  %% query the instance with after_query()
        ]).

%% Direct calls to the callback module
-export([ call_start/3  %% start the instance
        , call_health_check/3 %% verify if the widget is working normally
        , call_stop/3   %% stop the instance
        ]).

-export([ list_instances/0 %% list all the instances, id only.
        , list_instances_verbose/0 %% list all the instances
        , get_instance/1 %% return the data of the instance
        , get_instance_by_type/1 %% return all the instances of the same widget type
        , load_instances/1 %% load instances from config files
        % , dependents/1
        % , inc_counter/2 %% increment the counter of the instance
        % , inc_counter/3 %% increment the counter by a given integer
        ]).

-define(EXT, "*.wgt").

%% when calling emqx_widget:start/1
-callback on_start(instance_id(), widget_config()) ->
    {ok, widget_state()} | {error, Reason :: term()}.

%% when calling emqx_widget:stop/1
-callback on_stop(instance_id(), widget_state()) -> term().

%% when calling emqx_widget:query/3
-callback on_query(instance_id(), Request :: term(), after_query(), widget_state()) -> term().

%% when calling emqx_widget:health_check/2
-callback on_health_check(instance_id(), widget_state()) ->
    {ok, widget_state()} | {error, Reason:: term(), widget_state()}.

%% load specs and return the loaded widgets this time.
-spec list_types_verbose() -> [widget_spec()].
list_types_verbose() ->
    [get_spec(Mod) || Mod <- list_types()].

-spec list_types() -> [module()].
list_types() ->
    discover_widget_mods().

-spec get_type(module()) -> {ok, widget_spec()} | {error, not_found}.
get_type(Mod) ->
    case is_widget_mod(Mod) of
        true -> {ok, get_spec(Mod)};
        false -> {error, not_found}
    end.

-spec get_spec(module()) -> widget_spec().
get_spec(Mod) ->
    maps:put(<<"widget_type">>, Mod, Mod:emqx_widget_spec()).

-spec discover_widget_mods() -> [module()].
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
%% APIs for widget instances
%% =================================================================================
-spec create(instance_id(), widget_type(), widget_config()) -> ok | {error, Reason :: term()}.
create(InstId, WidgetType, Config) ->
    ?CLUSTER_CALL(call_instance, [InstId, {create, InstId, WidgetType, Config}]).

-spec create_dry_run(instance_id(), widget_type(), widget_config()) ->
    ok | {error, Reason :: term()}.
create_dry_run(InstId, WidgetType, Config) ->
    ?CLUSTER_CALL(call_instance, [InstId, {create_dry_run, InstId, WidgetType, Config}]).

-spec update(instance_id(), widget_config()) -> ok | {error, Reason :: term()}.
update(InstId, Config) ->
    ?CLUSTER_CALL(call_instance, [InstId, {update, InstId, Config}]).

-spec remove(instance_id()) -> ok | {error, Reason :: term()}.
remove(InstId) ->
    ?CLUSTER_CALL(call_instance, [InstId, {remove, InstId}]).

-spec query(instance_id(), Request :: term()) -> Result :: term().
query(InstId, Request) ->
    query(InstId, Request, undefined).

%% same to above, also defines what to do when the Module:on_query success or failed
%% it is the duty of the Moudle to apply the `after_query()` functions.
-spec query(instance_id(), Request :: term(), after_query()) -> Result :: term().
query(InstId, Request, AfterQuery) ->
    case get_instance(InstId) of
        {ok, #{mod := Mod, state := WidgetState}} ->
            %% the widget state is readonly to Moudle:on_query/4
            %% and the `after_query()` functions should be thread safe
            Mod:on_query(InstId, Request, AfterQuery, WidgetState);
        {error, Reason} ->
            error({get_instance, {InstId, Reason}})
    end.

-spec restart(instance_id()) -> ok | {error, Reason :: term()}.
restart(InstId) ->
    call_instance(InstId, {restart, InstId}).

-spec stop(instance_id()) -> ok | {error, Reason :: term()}.
stop(InstId) ->
    call_instance(InstId, {stop, InstId}).

-spec health_check(instance_id()) -> ok | {error, Reason :: term()}.
health_check(InstId) ->
    call_instance(InstId, {health_check, InstId}).

-spec get_instance(instance_id()) -> {ok, widget_data()} | {error, Reason :: term()}.
get_instance(InstId) ->
    emqx_widget_instance:lookup(InstId).

-spec list_instances() -> [instance_id()].
list_instances() ->
    [Id || {Id, _} <- list_instances_verbose()].

-spec list_instances_verbose() -> [{instance_id(), widget_data()}].
list_instances_verbose() ->
    emqx_widget_instance:list_all().

-spec get_instance_by_type(module()) -> [widget_data()].
get_instance_by_type(WidgetType) ->
    emqx_widget_instance:lookup_by_type(WidgetType).

-spec load_instances(Dir :: string()) -> ok.
load_instances(Dir) ->
    emqx_widget_instance:load(Dir).


-spec call_start(instance_id(), module(), widget_config()) ->
    {ok, widget_state()} | {error, Reason :: term()}.
call_start(InstId, Mod, Config) ->
    ?SAFE_CALL(Mod:on_start(InstId, Config)).

-spec call_health_check(instance_id(), module(), widget_state()) ->
    {ok, widget_state()} | {error, Reason:: term(), widget_state()}.
call_health_check(InstId, Mod, WidgetState) ->
    ?SAFE_CALL(Mod:on_health_check(InstId, WidgetState)).

-spec call_stop(instance_id(), module(), widget_state()) -> term().
call_stop(InstId, Mod, WidgetState) ->
    ?SAFE_CALL(Mod:on_stop(InstId, WidgetState)).

%% =================================================================================

call_instance(InstId, Query) ->
    emqx_widget_instance:hash_call(InstId, Query).

safe_apply(Func, Args) ->
    ?SAFE_CALL(erlang:apply(Func, Args)).

