#!/bin/sh
set -e

rebar3 compile

erl -sname abc -pa _build/default/lib/emqx_widget/ebin _build/default/lib/getopt/ebin _build/default/lib/gproc/ebin _build/default/lib/hocon/ebin _build/default/lib/typerefl/ebin _build/default/lib/emqx_widget/examples -s demo

