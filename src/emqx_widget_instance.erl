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

-export([start_link/0]).

-compile({no_auto_import,
          [ get/1
          ]}).

%% Sync widget instances and files
%% provisional solution: rpc:multical to all the nodes for creating/updating/removing
%% todo: replicate operations
-export([ create/1 %% store the configs and start the instance
        , create_dry_run/1 %% run start/2, health_check/2 and stop/1 sequentially
        , update/1 %% update the configs, stop the old instance and start the new one
        , remove/1 %% remove the configs and stop the instance
        ]).

%% invoke the callbacks of a instance
-export([ call/2   %% call the instance
        , start/2  %% start the instance
        , start/3  %% start the instance with after_called()
        , health_check/2 %% verify if the widget is working normally
        , stop/1   %% stop the instance
        ]).

-export([ lookup/1 %% return the configs and the state of the instance
        , dependents/1
        , inc_counter/2 %% increment the counter of the instance
        , inc_counter/3 %% increment the counter by a given integer
        ]).

%% for debug purposes
-export([dump/0]).

%% gen_server Callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

dump() ->
    io:format("Instances: ~p~n", [ets:tab2list(?MODULE)]).

%%------------------------------------------------------------------------------
%% Start the registry
%%------------------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%------------------------------------------------------------------------------
%% gen_server callbacks
%%------------------------------------------------------------------------------

init([]) ->
    {ok, #{}}.

handle_call(Req, _From, State) ->
    {reply, ignored, State}.

handle_cast(Msg, State) ->
    {noreply, State}.

handle_info(Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%------------------------------------------------------------------------------
%% APIs
%%------------------------------------------------------------------------------

%% call the Module:on_start/2
-spec start(widget_name(), widget_config()) -> {ok, widget_state()} | {error, Reason :: term()}.
start(WidgetName, Config) ->
    start(WidgetName, Config, undefined).

%% same to above, also defines what to do when the Module:on_call success or failed
-spec start(widget_name(), widget_config(), after_called()) ->
    {ok, instance_id()} | {error, Reason :: term()}.
start(WidgetName, Config, AfterCalled) ->
    Mod = binary_to_existing_atom(WidgetName, latin1),
    Mod:on_start(gen_inst_id(maps:get(id, Config, gen_id())), Config).

-spec call(instance_id(), Request :: term()) -> Result :: term().
call(InstId, Request) ->
    Mod = cbk_mod(InstId),
    Mod:on_call(InstId, Request, get_state(InstId)).

-spec stop(instance_id()) -> Result :: term().
stop(InstId) ->
    Mod = cbk_mod(InstId),
    Mod:on_stop(InstId, get_state(InstId)).

cbk_mod(InstId) ->
    [WidgetName, _Id] = string:split(InstId, ":"),
    binary_to_existing_atom(WidgetName, latin1).

create_rule(Params = #{rawsql := Sql, actions := ActArgs}) ->
    case emqx_rule_sqlparser:parse_select(Sql) of
        {ok, Select} ->
            RuleId = maps:get(id, Params, gen_inst_id()),
            Enabled = maps:get(enabled, Params, true),
            try prepare_actions(ActArgs, Enabled) of
                Actions ->
                    Rule = aa,
                    ok = emqx_rule_registry:add_rule(Rule),
                    ok = emqx_rule_metrics:create_rule_metrics(RuleId),
                    {ok, Rule}
            catch
                throw:{action_not_found, ActionName} ->
                    {error, {action_not_found, ActionName}};
                throw:Reason ->
                    {error, Reason}
            end;
        Reason -> {error, Reason}
    end.

-spec(update_rule(#{id := binary(), _=>_}) -> {ok, rule()} | {error, {not_found, gen_inst_id()}}).
update_rule(Params = #{id := RuleId}) ->
    case emqx_rule_registry:get_rule(RuleId) of
        {ok, Rule0} ->
            try may_update_rule_params(Rule0, Params) of
                Rule ->
                    ok = emqx_rule_registry:add_rule(Rule),
                    {ok, Rule}
            catch
                throw:Reason ->
                    {error, Reason}
            end;
        not_found ->
            {error, {not_found, RuleId}}
    end.

-spec(delete_rule(RuleId :: gen_inst_id()) -> ok).
delete_rule(RuleId) ->
    case emqx_rule_registry:get_rule(RuleId) of
        {ok, Rule = #rule{actions = Actions}} ->
            try
                _ = ?CLUSTER_CALL(clear_rule, [Rule]),
                ok = emqx_rule_registry:remove_rule(Rule)
            catch
                Error:Reason:ST ->
                    ?LOG(error, "clear_rule ~p failed: ~p", [RuleId, {Error, Reason, ST}]),
                    refresh_actions(Actions)
            end;
        not_found ->
            ok
    end.

gen_inst_id(WidgetName) ->
    gen_id(WidgetName, fun emqx_widget_registry:get_widget_inst/1).

gen_id(Prefix, TestFun) ->
    Id = iolist_to_binary([Prefix, emqx_rule_id:gen()]),
    case TestFun(Id) of
        not_found -> Id;
        _Res -> gen_id(Prefix, TestFun)
    end.

