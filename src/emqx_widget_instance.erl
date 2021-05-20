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

-module(emqx_widget_instance).

-behaviour(gen_server).

-include("emqx_widget.hrl").
-include("emqx_widget_utils.hrl").

-export([start_link/2]).

-compile({no_auto_import,
          [ get/1
          ]}).

%% load widget instances from *.conf files
-export([load/1]).

%% Sync widget instances and files
%% provisional solution: rpc:multical to all the nodes for creating/updating/removing
%% todo: replicate operations
-export([ create/1 %% store the configs and start the instance
        , create_dry_run/1 %% run start/2, health_check/2 and stop/1 sequentially
        , update/1 %% update the configs, stop the old instance and start the new one
        , remove/1 %% remove the configs and stop the instance
        ]).

%% invoke the callbacks of a instance
-export([ query/2  %% query the instance
        , query/3  %% query the instance with after_query()
        , restart/1  %% restart the instance
        , health_check/1 %% verify if the widget is working normally
        , stop/1   %% stop the instance
        ]).

-export([ get/1 %% return the data of the instance
        , get_by_type/1 %% return all the instances of the same widget type
        % , dependents/1
        % , inc_counter/2 %% increment the counter of the instance
        % , inc_counter/3 %% increment the counter by a given integer
        ]).

%% for debug purposes
-export([dump/0]).

-export([ hash_call/2
        , hash_call/3
        ]).

%% gen_server Callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

dump() ->
    io:format("Instances: ~p~n", [ets:tab2list(emqx_widget_instance)]).

%%------------------------------------------------------------------------------
%% Start the registry
%%------------------------------------------------------------------------------

start_link(Pool, Id) ->
    gen_server:start_link({local, proc_name(?MODULE, Id)},
                          ?MODULE, [Pool, Id], []).

%% call the worker by the hash of widget-instance-id, to make sure we always handle
%% operations on the same instance in the same worker.
hash_call(InstId, Request) ->
    hash_call(InstId, Request, infinity).

hash_call(InstId, Request, Timeout) ->
    gen_server:call(pick(InstId), Request, Timeout).

-spec load(Path :: string()) -> ok.
load(Path) ->
    lists:foreach(fun load_file/1, filelib:wildcard(filename:join([Path, "*.conf"]))).

load_file(File) ->
    case hocon:load(File, #{format => map}) of
         {ok, Conf} ->
            case ?SAFE_CALL(do_create(Conf)) of
                ok -> ok;
                {error, Reason} ->
                    logger:error("load widget instance from ~p failed: ~p", [File, Reason])
            end;
         {error, Reason} ->
             logger:error("load widget from ~p failed: ~p", [File, Reason])
     end.

-spec create(widget_config()) -> ok | {error, Reason :: term()}.
create(Config) ->
    create(create, Config).

-spec create_dry_run(widget_config()) -> ok | {error, Reason :: term()}.
create_dry_run(Config) ->
    create(create_dry_run, Config).
create(CreateType, #{<<"id">> := InstId, <<"widget_type">> := _} = Config) ->
    ?CLUSTER_CALL(hash_call, [InstId, {CreateType, Config}]).

-spec update(widget_config()) -> ok | {error, Reason :: term()}.
update(#{<<"id">> := InstId} = Config) ->
    ?CLUSTER_CALL(hash_call, [InstId, {update, Config}]).

-spec remove(instance_id()) -> ok | {error, Reason :: term()}.
remove(InstId) ->
    ?CLUSTER_CALL(hash_call, [InstId, {remove, InstId}]).

-spec query(instance_id(), Request :: term()) -> Result :: term().
query(InstId, Request) ->
    query(InstId, Request, undefined).

%% same to above, also defines what to do when the Module:on_query success or failed
%% it is the duty of the Moudle to apply the `after_query()` functions.
-spec query(instance_id(), Request :: term(), after_query()) -> Result :: term().
query(InstId, Request, AfterQuery) ->
    case get(InstId) of
        {ok, #{mod := Mod, state := WidgetState}} ->
            %% the widget state is readonly to Moudle:on_query/4
            %% and the `after_query()` functions should be thread safe
            Mod:on_query(InstId, Request, AfterQuery, WidgetState);
        {error, Reason} ->
            error(get_instance, Reason)
    end.

%% call the Module:on_start/2
-spec restart(instance_id()) -> ok | {error, Reason :: term()}.
restart(InstId) ->
    hash_call(InstId, {restart, InstId}).

-spec stop(instance_id()) -> ok | {error, Reason :: term()}.
stop(InstId) ->
    hash_call(InstId, {stop, InstId}).

-spec health_check(instance_id()) -> ok | {error, Reason :: term()}.
health_check(InstId) ->
    hash_call(InstId, {health_check, InstId}).

-spec get(instance_id()) -> {ok, widget_data()} | {error, Reason :: term()}.
get(InstId) ->
    case ets:lookup(emqx_widget_instance, InstId) of
        [] -> {error, not_found};
        [{_, Data}] -> {ok, Data}
    end.

-spec get_by_type(module()) -> [widget_data()].
get_by_type(Mod) ->
    [Data || #{mod := Mod0} = Data <- ets:tab2list(emqx_widget_instance), Mod0 =:= Mod].

%%------------------------------------------------------------------------------
%% gen_server callbacks
%%------------------------------------------------------------------------------

init([Pool, Id]) ->
    true = gproc_pool:connect_worker(Pool, {Pool, Id}),
    {ok, #{pool => Pool, id => Id}}.

handle_call({create, Config}, _From, State) ->
    {reply, do_create(Config), State};

handle_call({create_dry_run, Config}, _From, State) ->
    {reply, do_create_dry_run(Config), State};

handle_call({update, Config}, _From,
        State) ->
    {reply, do_update(Config), State};

handle_call({remove, InstId}, _From, State) ->
    {reply, do_remove(InstId), State};

handle_call({restart, InstId}, _From, State) ->
    {reply, do_restart(InstId), State};

handle_call({stop, InstId}, _From, State) ->
    {reply, do_stop(InstId), State};

handle_call({health_check, InstId}, _From, State) ->
    {reply, do_health_check(InstId), State};

handle_call(Req, _From, State) ->
    logger:error("Received unexpected call: ~p", [Req]),
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #{pool := Pool, id := Id}) ->
    gproc_pool:disconnect_worker(Pool, {Pool, Id}).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%------------------------------------------------------------------------------
start_instance(#{<<"id">> := InstId, <<"widget_type">> := WidgetType} = Config) ->
    case get_cbk_mod_of_widget(WidgetType) of
        {ok, Mod} ->
            case mod_start(Mod, InstId, Config) of
                {ok, WidgetState} -> {ok, Mod, InstId, WidgetState};
                {error, Reason} -> {error, Reason}
            end;
        Error -> Error
    end.

do_create(#{<<"id">> := InstId, <<"widget_type">> := WidgetType} = Config) ->
    case get(InstId) of
        {ok, _} -> {error, already_created};
        _ ->
            case start_instance(Config) of
                {ok, Mod, InstId, WidgetState} ->
                    ets:insert(emqx_widget_instance, {InstId, #{mod => Mod, config => Config,
                        state => WidgetState, status => started}}),
                    ok;
                {error, Reason} ->
                    logger:error("start ~s widget ~s failed: ~p", [WidgetType, InstId, Reason]),
                    {error, Reason}
            end
    end.

do_create_dry_run(Config) ->
    case start_instance(Config) of
        {ok, Mod, InstId, WidgetState0} ->
            case mod_health_check(Mod, InstId, WidgetState0) of
                {ok, WidgetState1} ->
                    _ = mod_stop(Mod, InstId, WidgetState1),
                    ok;
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

do_remove(InstId) ->
    case get(InstId) of
        {ok, #{mod := Mod, state := WidgetState}} ->
            do_remove(Mod, InstId, WidgetState);
        Error ->
            Error
    end.

do_remove(Mod, InstId, WidgetState) ->
    _ = mod_stop(Mod, InstId, WidgetState),
    ets:delete(emqx_widget_instance, InstId),
    ok.

do_update(#{<<"Id">> := InstId} = NewConfig) ->
    case get(InstId) of
        {ok, #{mod := Mod, state := WidgetState, config := OldConfig}} ->
            Config = maps:merge(OldConfig, NewConfig),
            case do_create_dry_run(Config) of
                ok ->
                    do_remove(Mod, InstId, WidgetState),
                    do_create(Config);
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

do_restart(InstId) ->
    case get(InstId) of
        {ok, #{mod := Mod, state := WidgetState, config := Config} = Data} ->
            _ = mod_stop(Mod, InstId, WidgetState),
            case mod_start(Mod, InstId, Config) of
                {ok, WidgetState} ->
                    ets:insert(emqx_widget_instance, {InstId, Data#{state => WidgetState, status => started}}),
                    ok;
                {error, Reason} ->
                    ets:insert(emqx_widget_instance, {InstId, Data#{status => stopped}}),
                    {error, Reason}
            end;
        Error ->
            Error
    end.

do_stop(InstId) ->
    case get(InstId) of
        {ok, #{mod := Mod, state := WidgetState} = Data} ->
            _ = mod_stop(Mod, InstId, WidgetState),
            ets:insert(emqx_widget_instance, {InstId, Data#{status => stopped}}),
            ok;
        Error ->
            Error
    end.

do_health_check(InstId) ->
    case get(InstId) of
        {ok, #{mod := Mod, state := WidgetState0} = Data} ->
            case mod_health_check(Mod, InstId, WidgetState0) of
                {ok, WidgetState1} ->
                    ets:insert(emqx_widget_instance, {InstId, Data#{status => started, state => WidgetState1}}),
                    ok;
                {error, Reason} ->
                    logger:error("health check for ~p failed: ~p", [InstId, Reason]),
                    ets:insert(emqx_widget_instance, {InstId, Data#{status => stopped}}),
                    {error, Reason}
            end;
        Error ->
            Error
    end.

mod_start(Mod, InstId, Config) ->
    ?SAFE_CALL(Mod:on_start(InstId, Config)).

mod_health_check(Mod, InstId, WidgetState) ->
    ?SAFE_CALL(Mod:on_health_check(InstId, WidgetState)).

mod_stop(Mod, InstId, WidgetState) ->
    ?SAFE_CALL(Mod:on_stop(InstId, WidgetState)).

%%------------------------------------------------------------------------------
%% internal functions
%%------------------------------------------------------------------------------

proc_name(Mod, Id) ->
    list_to_atom(lists:concat([Mod, "_", Id])).

get_cbk_mod_of_widget(WidgetType) ->
    try Mod = binary_to_existing_atom(WidgetType, latin1),
        case emqx_widget:is_widget_mod(Mod) of
            true -> {ok, Mod};
            false -> {error, {invalid_widget, Mod}}
        end
    catch error:badarg ->
        {error, {not_found, WidgetType}}
    end.

pick(InstId) ->
    gproc_pool:pick_worker(emqx_widget_instance, InstId).
