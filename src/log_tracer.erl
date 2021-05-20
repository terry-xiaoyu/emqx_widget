-module(log_tracer).

-include_lib("emqx_widget/include/emqx_widget_mod.hrl").

-emqx_widget_spec("src/log_tracer.wgt").

-export([ on_start/2
        , on_stop/2
        , on_query/4
        , on_health_check/2
        ]).

on_start(InstId, Config) ->
    logger:warning("== the demo log tracer ~p started, config: ~p", [InstId, Config]),
    {ok, #{logger_handler_id => abc, health_checked => 0}}.

on_stop(InstId, State) ->
    logger:warning("== the demo log tracer ~p stopped, state: ~p", [InstId, State]),
    ok.

on_query(InstId, Request, AfterQuery, State) ->
    logger:warning("== the demo log tracer ~p received request: ~p, state: ~p",
        [InstId, Request, State]),
    emqx_widget:query_success(AfterQuery),
    "this is a demo log messages...".

on_health_check(InstId, State = #{health_checked := Checked}) ->
    logger:warning("== the demo log tracer ~p is working well, state: ~p", [InstId, State]),
    {ok, State#{health_checked => Checked + 1}}.