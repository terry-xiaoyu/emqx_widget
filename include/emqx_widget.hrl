-type widget_type() :: module().
-type instance_id() :: string().
-type widget_config() :: map().
-type widget_spec() :: map().
-type widget_state() :: term().
-type widget_data() :: #{
    mod => module(),
    config => widget_config(),
    state => widget_state(),
    status => started | stopped
}.

-type after_query() :: {OnSuccess :: after_query_fun(), OnFailed :: after_query_fun()} |
    undefined.

%% the `after_query_fun()` is mainly for callbacks that increment counters or do some fallback
%% actions upon query failure
-type after_query_fun() :: {fun((...) -> ok), Args :: [term()]}.
