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

Build
-----

    $ rebar3 compile
