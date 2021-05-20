emqx_widget
=====

The `emqx_widget` is an application that manages configuration specs and runtime states
for components that need to be configured and manipulated from the emqx-dashboard.

It is intended to be used by resources, actions, acl, auth, backend_logics and more.

It reads the configuration spec from *.wgt (in HOCON format) and provide APIs for
creating, updating and destroying widget instances among all nodes in the cluster.

It handles the problem like storing the configs and runtime states for both widget
and widget instances, and how porting them between different emqx_widget versions.

It may maintain the config and data in JSON or HOCON files in data/ dir.

After restarting the emqx_widget, it re-creates all the widget instances.

There can be foreign references between widget instances via widget-id.
So they may find each other via this Id.

Try it out
----------

    $ ./demo.sh
    Eshell V11.1.8  (abort with ^G)
    1> == the demo log tracer <<"log_tracer_clientid_shawn">> started.
    config: #{<<"configs">> =>
                #{<<"bulk">> => <<"10KB">>,<<"cache_log_dir">> => <<"/tmp">>,
                    <<"condition">> => #{<<"clientid">> => <<"abc">>},
                    <<"level">> => <<"debug">>},
            <<"id">> => <<"log_tracer_clientid_shawn">>,
            <<"widget_type">> => <<"log_tracer">>}
    1> emqx_widget_instance:health_check(<<"log_tracer_clientid_shawn">>).
    == the demo log tracer <<"log_tracer_clientid_shawn">> is working well
    state: #{health_checked => 1,logger_handler_id => abc}
    ok

    2> emqx_widget_instance:health_check(<<"log_tracer_clientid_shawn">>).
    == the demo log tracer <<"log_tracer_clientid_shawn">> is working well
    state: #{health_checked => 2,logger_handler_id => abc}
    ok

    3> emqx_widget_instance:query(<<"log_tracer_clientid_shawn">>, get_log).
    == the demo log tracer <<"log_tracer_clientid_shawn">> received request: get_log
    state: #{health_checked => 2,logger_handler_id => abc}
    "this is a demo log messages..."

    4> emqx_widget_instance:remove(<<"log_tracer_clientid_shawn">>).
    == the demo log tracer <<"log_tracer_clientid_shawn">> stopped.
    state: #{health_checked => 0,logger_handler_id => abc}
    ok

    5> emqx_widget_instance:query(<<"log_tracer_clientid_shawn">>, get_log).
    ** exception error: {get_instance,{<<"log_tracer_clientid_shawn">>,not_found}}

