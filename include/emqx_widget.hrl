-define(?WIDGET_INST_TAB, emqx_widget_instance).

-type args() :: term().
-type widget_name() :: binary().
-type instance_id() :: binary().
-type widget_config() :: map().
-type widget_spec() :: map().
-type widget_state() :: term().
-type after_called_fun() :: {fun((widget_state(), args()) -> ok), args()}.