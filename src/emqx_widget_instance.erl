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

%% load widget instances from *.conf files
-export([ load/1
        , lookup/1
        , list_all/0
        , lookup_by_type/1
        ]).

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

-record(state, {worker_pool, worker_id}).

-type state() :: #state{}.

%%------------------------------------------------------------------------------
%% Start the registry
%%------------------------------------------------------------------------------

start_link(Pool, Id) ->
    gen_server:start_link({local, proc_name(?MODULE, Id)},
                          ?MODULE, {Pool, Id}, []).

%% call the worker by the hash of widget-instance-id, to make sure we always handle
%% operations on the same instance in the same worker.
hash_call(InstId, Request) ->
    hash_call(InstId, Request, infinity).

hash_call(InstId, Request, Timeout) ->
    gen_server:call(pick(InstId), Request, Timeout).

-spec lookup(instance_id()) -> {ok, widget_data()} | {error, Reason :: term()}.
lookup(InstId) ->
    case ets:lookup(emqx_widget_instance, InstId) of
        [] -> {error, not_found};
        [{_, Data}] -> {ok, Data#{id => InstId}}
    end.

force_lookup(InstId) ->
    {ok, Data} = lookup(InstId),
    Data.

-spec list_all() -> [widget_data()].
list_all() ->
    [Data#{id => Id} || {Id, Data} <- ets:tab2list(emqx_widget_instance)].

-spec lookup_by_type(module()) -> [widget_data()].
lookup_by_type(WidgetType) ->
    [Data || #{mod := Mod} = Data <- list_all()
             , Mod =:= WidgetType].

-spec load(Dir :: string()) -> ok.
load(Dir) ->
    lists:foreach(fun load_file/1, filelib:wildcard(filename:join([Dir, "*.conf"]))).

load_file(File) ->
    case ?SAFE_CALL(hocon_token:read(File)) of
        {error, Reason} ->
            logger:error("load widget from ~p failed: ~p", [File, Reason]);
        RawConfig ->
            case emqx_widget:parse_config(RawConfig) of
                {error, Reason} ->
                    logger:error("load widget instance from ~p failed: ~p", [File, Reason]);
                {ok, InstId, WidgetType, InstConf} ->
                    create_instance_local(InstId, WidgetType, InstConf)
            end
    end.

create_instance_local(InstId, WidgetType, InstConf) ->
    case do_create(InstId, WidgetType, InstConf) of
        {ok, Data} ->
            logger:debug("created ~p widget instance: ~p from config: ~p, Data: ~p",
                [WidgetType, InstId, InstConf, Data]);
        {error, Reason} ->
            logger:error("create ~p widget instance: ~p failed: ~p, config: ~p",
                [WidgetType, InstId, Reason, InstConf])
    end.

%%------------------------------------------------------------------------------
%% gen_server callbacks
%%------------------------------------------------------------------------------

-spec init({atom(), integer()}) ->
    {ok, State :: state()} | {ok, State :: state(), timeout() | hibernate | {continue, term()}} |
    {stop, Reason :: term()} | ignore.
init({Pool, Id}) ->
    true = gproc_pool:connect_worker(Pool, {Pool, Id}),
    {ok, #state{worker_pool = Pool, worker_id = Id}}.

handle_call({create, InstId, WidgetType, Config}, _From, State) ->
    {reply, do_create(InstId, WidgetType, Config), State};

handle_call({create_dry_run, InstId, WidgetType, Config}, _From, State) ->
    {reply, do_create_dry_run(InstId, WidgetType, Config), State};

handle_call({update, InstId, WidgetType, Config}, _From, State) ->
    {reply, do_update(InstId, WidgetType, Config), State};

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

terminate(_Reason, #state{worker_pool = Pool, worker_id = Id}) ->
    gproc_pool:disconnect_worker(Pool, {Pool, Id}).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%------------------------------------------------------------------------------

do_update(InstId, WidgetType, NewConfig) when is_map(NewConfig) ->
    case lookup(InstId) of
        {ok, #{mod := WidgetType, state := WidgetState, config := OldConfig}} ->
            Config = maps:merge(OldConfig, NewConfig),
            case do_create_dry_run(InstId, WidgetType, Config) of
                ok ->
                    do_remove(WidgetType, InstId, WidgetState),
                    do_create(InstId, WidgetType, Config);
                Error ->
                    Error
            end;
        {ok, #{mod := Mod}} when Mod =/= WidgetType ->
            {error, updating_to_incorrect_widget_type};
        {error, not_found} ->
            do_create(InstId, WidgetType, NewConfig)
    end.

do_create(InstId, WidgetType, Config) when is_map(Config) ->
    case lookup(InstId) of
        {ok, _} -> {error, already_created};
        _ ->
            case emqx_widget:call_start(InstId, WidgetType, Config) of
                {ok, WidgetState} ->
                    ets:insert(emqx_widget_instance, {InstId,
                        #{mod => WidgetType, config => Config,
                          state => WidgetState, status => stopped}}),
                    _ = do_health_check(InstId),
                    {ok, force_lookup(InstId)};
                {error, Reason} ->
                    logger:error("start ~s widget ~s failed: ~p", [WidgetType, InstId, Reason]),
                    {error, Reason}
            end
    end.

do_create_dry_run(InstId, WidgetType, Config) when is_map(Config) ->
    case emqx_widget:call_start(InstId, WidgetType, Config) of
        {ok, WidgetState0} ->
            Return = case emqx_widget:call_health_check(InstId, WidgetType, WidgetState0) of
                {ok, WidgetState1} -> ok;
                {error, Reason, WidgetState1} ->
                    {error, Reason}
            end,
            _ = emqx_widget:call_stop(InstId, WidgetType, WidgetState1),
            Return;
        {error, Reason} ->
            {error, Reason}
    end.

do_remove(InstId) ->
    case lookup(InstId) of
        {ok, #{mod := Mod, state := WidgetState}} ->
            do_remove(Mod, InstId, WidgetState);
        Error ->
            Error
    end.

do_remove(Mod, InstId, WidgetState) ->
    _ = emqx_widget:call_stop(InstId, Mod, WidgetState),
    ets:delete(emqx_widget_instance, InstId),
    ok.

do_restart(InstId) ->
    case lookup(InstId) of
        {ok, #{mod := Mod, state := WidgetState, config := Config} = Data} ->
            _ = emqx_widget:call_stop(InstId, Mod, WidgetState),
            case emqx_widget:call_start(InstId, Mod, Config) of
                {ok, WidgetState} ->
                    ets:insert(emqx_widget_instance,
                        {InstId, Data#{state => WidgetState, status => started}}),
                    ok;
                {error, Reason} ->
                    ets:insert(emqx_widget_instance, {InstId, Data#{status => stopped}}),
                    {error, Reason}
            end;
        Error ->
            Error
    end.

do_stop(InstId) ->
    case lookup(InstId) of
        {ok, #{mod := Mod, state := WidgetState} = Data} ->
            _ = emqx_widget:call_stop(InstId, Mod, WidgetState),
            ets:insert(emqx_widget_instance, {InstId, Data#{status => stopped}}),
            ok;
        Error ->
            Error
    end.

do_health_check(InstId) ->
    case lookup(InstId) of
        {ok, #{mod := Mod, state := WidgetState0} = Data} ->
            case emqx_widget:call_health_check(InstId, Mod, WidgetState0) of
                {ok, WidgetState1} ->
                    ets:insert(emqx_widget_instance,
                        {InstId, Data#{status => started, state => WidgetState1}}),
                    ok;
                {error, Reason, WidgetState1} ->
                    logger:error("health check for ~p failed: ~p", [InstId, Reason]),
                    ets:insert(emqx_widget_instance,
                        {InstId, Data#{status => stopped, state => WidgetState1}}),
                    {error, Reason}
            end;
        Error ->
            Error
    end.

%%------------------------------------------------------------------------------
%% internal functions
%%------------------------------------------------------------------------------

proc_name(Mod, Id) ->
    list_to_atom(lists:concat([Mod, "_", Id])).

pick(InstId) ->
    gproc_pool:pick_worker(emqx_widget_instance, InstId).
