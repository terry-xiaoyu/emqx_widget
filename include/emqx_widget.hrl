-type args() :: term().
-type widget_type() :: binary().
-type instance_id() :: binary().
-type widget_config() :: map().
-type widget_spec() :: map().
-type widget_state() :: term().
-type widget_data() :: #{
    mod => module(),
    config => widget_config(),
    state => widget_state(),
    status => started | stopped
}.

-type after_query_fun() :: {fun((widget_state(), args()) -> ok), args()}.
-type after_query() :: {OnSuccess :: after_query_fun(), OnFailed :: after_query_fun()} |
    undefined.

-define(WIDGET_INST_TAB, emqx_widget_instance).
