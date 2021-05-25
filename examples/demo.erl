-module(demo).

-export([start/0]).

start() ->
    code:load_file(log_tracer),
    {ok, _} = application:ensure_all_started(minirest),
    {ok, _} = application:ensure_all_started(emqx_widget),
    emqx_widget:load_instances("./_build/default/lib/emqx_widget/examples"),
    Handlers = [{"/", minirest:handler(#{modules => [log_tracer]})}],
    Dispatch = [{"/[...]", minirest, Handlers}],
    minirest:start_http(?MODULE, #{socket_opts => [inet, {port, 9900}]}, Dispatch).
