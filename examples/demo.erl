-module(demo).

-export([start/0]).

start() ->
    code:load_file(log_tracer),
    {ok, _} = application:ensure_all_started(emqx_widget),
    emqx_widget_instance:load("./_build/default/lib/emqx_widget/examples").