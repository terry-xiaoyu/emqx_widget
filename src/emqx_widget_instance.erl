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
        [{_, Data}] -> {ok, Data}
    end.

-spec load(Dir :: string()) -> ok.
load(Dir) ->
    lists:foreach(fun load_file/1, filelib:wildcard(filename:join([Dir, "*.conf"]))).

load_file(File) ->
    case hocon:load(File, #{format => richmap}) of
        {ok, Config} ->
            load_widget_instance(Config);
        {error, Reason} ->
            logger:error("load widget from ~p failed: ~p", [File, Reason])
     end.

%% Config for list of widget instances
load_widget_instance(Config) when is_list(Config) ->
    lists:foreach(fun load_widget_instance/1, Config);

%% Config for a widget
load_widget_instance(Config) ->
    logger:debug("loading widget instance from config: ~p", [Config]),
    WidgetType = hocon_schema:deep_get("widget_type", Config, value),
    case get_cbk_mod_of_widget(WidgetType) of
        {ok, CbkMod} ->
            do_load_widget_instance(CbkMod, Config);
        Error ->
            logger:error("load widget instance failed: ~p, config: ~p", [Error, Config])
    end.

do_load_widget_instance(WidgetType, Config) ->
    case ?SAFE_CALL(hocon_schema:generate(WidgetType, Config)) of
        {error, Reason} ->
            logger:error("load widget instance for type ~p failed: ~p, config: ~p",
                [WidgetType, Reason, Config]);
        Config0 ->
            InstId = proplists:get_value(id, Config0),
            InstConf = proplists:get_value(config, Config0),
            case ?SAFE_CALL(do_create(InstId, WidgetType, InstConf)) of
                ok -> ok;
                {error, Reason} ->
                    logger:error("create widget instance for type ~p failed: ~p, config: ~p",
                        [WidgetType, Reason, Config0])
            end
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

handle_call({update, InstId, Config}, _From, State) ->
    {reply, do_update(InstId, Config), State};

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

do_create(InstId, WidgetType, Config) ->
    case lookup(InstId) of
        {ok, _} -> {error, already_created};
        _ ->
            case mod_start(InstId, WidgetType, Config) of
                {ok, WidgetState} ->
                    ets:insert(emqx_widget_instance, {InstId, #{mod => WidgetType, config => Config,
                        state => WidgetState, status => started}}),
                    ok;
                {error, Reason} ->
                    logger:error("start ~s widget ~s failed: ~p", [WidgetType, InstId, Reason]),
                    {error, Reason}
            end
    end.

do_create_dry_run(InstId, WidgetType, Config) ->
    case mod_start(InstId, WidgetType, Config) of
        {ok, WidgetState0} ->
            case mod_health_check(InstId, WidgetType, WidgetState0) of
                {ok, WidgetState1} ->
                    _ = mod_stop(InstId, WidgetType, WidgetState1),
                    ok;
                {error, Reason} ->
                    {error, Reason}
            end;
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
    _ = mod_stop(InstId, Mod, WidgetState),
    ets:delete(emqx_widget_instance, InstId),
    ok.

do_update(InstId, NewConfig) ->
    case lookup(InstId) of
        {ok, #{mod := Mod, state := WidgetState, config := OldConfig}} ->
            Config = maps:merge(OldConfig, NewConfig),
            case do_create_dry_run(InstId, Mod, Config) of
                ok ->
                    do_remove(Mod, InstId, WidgetState),
                    do_create(InstId, Mod, Config);
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

do_restart(InstId) ->
    case lookup(InstId) of
        {ok, #{mod := Mod, state := WidgetState, config := Config} = Data} ->
            _ = mod_stop(InstId, Mod, WidgetState),
            case mod_start(InstId, Mod, Config) of
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
    case lookup(InstId) of
        {ok, #{mod := Mod, state := WidgetState} = Data} ->
            _ = mod_stop(InstId, Mod, WidgetState),
            ets:insert(emqx_widget_instance, {InstId, Data#{status => stopped}}),
            ok;
        Error ->
            Error
    end.

do_health_check(InstId) ->
    case lookup(InstId) of
        {ok, #{mod := Mod, state := WidgetState0} = Data} ->
            case mod_health_check(InstId, Mod, WidgetState0) of
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

mod_start(InstId, Mod, Config) ->
    ?SAFE_CALL(Mod:on_start(InstId, Config)).

mod_health_check(InstId, Mod, WidgetState) ->
    ?SAFE_CALL(Mod:on_health_check(InstId, WidgetState)).

mod_stop(InstId, Mod, WidgetState) ->
    ?SAFE_CALL(Mod:on_stop(InstId, WidgetState)).

%%------------------------------------------------------------------------------
%% internal functions
%%------------------------------------------------------------------------------

proc_name(Mod, Id) ->
    list_to_atom(lists:concat([Mod, "_", Id])).

get_cbk_mod_of_widget(WidgetType) ->
    try Mod = list_to_existing_atom(str(WidgetType)),
        case emqx_widget:is_widget_mod(Mod) of
            true -> {ok, Mod};
            false -> {error, {invalid_widget, Mod}}
        end
    catch error:badarg ->
        {error, {not_found, WidgetType}}
    end.

pick(InstId) ->
    gproc_pool:pick_worker(emqx_widget_instance, InstId).

str(S) when is_binary(S) -> binary_to_list(S);
str(S) when is_list(S) -> S.
