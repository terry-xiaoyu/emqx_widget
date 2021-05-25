-module(log_tracer_api).

-emqx_widget_api_path("/log_tracer").

-rest_api(#{name   => list_log_tracers,
            method => 'GET',
            path   => "/log_tracer/",
            func   => api_get_all,
            descr  => "List log tracers"}).

-rest_api(#{name   => lookup_log_tracer,
            method => 'GET',
            path   => "/log_tracer/:id",
            func   => api_get,
            descr  => "Get book by Id"}).

-rest_api(#{name   => update_log_tracer,
            method => 'PUT',
            path   => "/log_tracer/:id",
            func   => api_put,
            descr  => "Update the log tracer by an existing tracer Id and a new config,\n"
                      "create a new log tracer if not found"}).

-rest_api(#{name   => delete_log_tracer,
            method => 'DELETE',
            path   => "/log_tracer/:id",
            func   => api_delete,
            descr  => "Delete the log tracer by an existing tracer Id"}).

-export([ api_get_all/2
        , api_get/2
        , api_put/2
        , api_delete/2
        ]).

jsonify(Reason) ->
    iolist_to_binary(io_lib:format("~p", [Reason])).

api_get_all(_Binding, _Params) ->
    {200, #{code => 0, data =>
        [format_data(Data) || Data <- emqx_widget:list_instances_verbose()]}}.

api_get(#{id := Id}, _Params) ->
    case emqx_widget:get_instance(Id) of
        {ok, Data} ->
            {200, #{code => 0, data => format_data(Data)}};
        {error, not_found} ->
            {404, #{code => 102, message => not_found}}
    end.

api_put(#{id := Id}, Params) ->
    JsonStr = jsx:encode([{<<"id">>, list_to_binary(Id)} | Params]),
    case emqx_widget:parse_config(JsonStr) of
        {ok, InstId, WidgetType, Config} ->
            case emqx_widget:update(InstId, WidgetType, Config) of
                {ok, Data} ->
                    %% TODO: provide API: emqx_widget_data:state(Data),
                    {200, #{code => 0, data => format_data(Data)}};
                {error, Reason} ->
                    {500, #{code => 102, message => jsonify(Reason)}}
            end;
        {error, Reason} ->
            {400, #{code => 108, message => jsonify(Reason)}}
    end.

api_delete(#{id := Id}, _Params) ->
    case emqx_widget:remove(Id) of
        ok -> {200, #{code => 0, data => #{}}};
        {error, Reason} ->
            {500, #{code => 102, message => jsonify(Reason)}}
    end.

format_data(#{id := Id, status := Status, state := #{health_checked := NChecked}}) ->
    #{id => list_to_binary(Id), status => Status, checked_count => NChecked}.
