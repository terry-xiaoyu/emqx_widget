-module(log_tracer_schema).

-include_lib("typerefl/include/types.hrl").

-behaviour(hocon_schema).

-export([structs/0, fields/1, translations/0, translation/1]).

-reflect_type([t_level/0, t_cache_logs_in/0]).

-type t_level() :: debug | info | notice | warning | error | critical | alert | emergency.

-type t_cache_logs_in() :: memory | file.

structs() -> [id, widget_type, configs].

translations() -> ["configs"].

translation("configs") -> log_tracer:config_transform("configs").

fields(id) ->
    [fun
        (mapping) -> "id";
        (type) -> string();
        (_) -> undefined
     end];

fields(widget_type) ->
    [fun
        (mapping) -> "widget_type";
        (type) -> string();
        (_) -> undefined
     end];

fields("app0") ->
    [{"app", fun
        (mapping) -> "configs.condition.app";
        (type) -> string();
        (_) -> undefined
      end}];

fields(configs) ->
    [ {condition, fun condition/1}
    , {level, fun level/1}
    , {enable_cache, fun enable_cache/1}
    , {cache_logs_in, fun cache_logs_in/1}
    , {cache_log_dir, fun cache_log_dir/1}
    , {bulk, fun bulk/1}
    ].

condition(mapping) -> "configs.condition";
condition(type) ->
    {ref, fields("app0")};
condition(_) -> undefined.

level(mapping) -> "configs.level";
level(type) -> t_level();
level(_) -> undefined.

enable_cache(mapping) -> "configs.enable_cache";
enable_cache(type) -> boolean();
enable_cache(_) -> undefined.

cache_logs_in(mapping) -> "configs.cache_logs_in";
cache_logs_in(type) -> t_cache_logs_in();
cache_logs_in(_) -> undefined.

cache_log_dir(mapping) -> "configs.cache_log_dir";
cache_log_dir(type) -> typerefl:regexp_string("^(.*)$");
cache_log_dir(_) -> undefined.

bulk(mapping) -> "configs.bulk";
bulk(type) -> typerefl:regexp_string("^[. 0-9]+(B|KB|MB|GB)$");
bulk(_) -> undefined.
