-define(CLUSTER_CALL(Func, Args), ?CLUSTER_CALL(Func, Args, ok)).

-define(CLUSTER_CALL(Func, Args, ResParttern),
%% ekka_mnesia:running_nodes()
    fun() -> case rpc:multicall([node()|nodes()], ?MODULE, Func, Args, 5000) of
        {ResL, []} ->
            case lists:filter(fun(ResParttern) -> false; (_) -> true end, ResL) of
                [] -> ResL;
                ErrL ->
                    logger:error("cluster_call error found, ResL: ~p", [ResL]),
                    throw({Func, ErrL})
            end;
        {ResL, BadNodes} ->
            logger:error("cluster_call bad nodes found: ~p, ResL: ~p", [BadNodes, ResL]),
            throw({Func, {failed_on_nodes, BadNodes}})
    end end()).

-define(SAFE_CALL(_EXP_),
        ?SAFE_CALL(_EXP_, _ = do_nothing)).

-define(SAFE_CALL(_EXP_, _EXP_ON_FAIL_),
        fun() ->
            try (_EXP_)
            catch _EXCLASS_:_EXCPTION_:_ST_ ->
                _EXP_ON_FAIL_,
                {error, {_EXCLASS_, _EXCPTION_, _ST_}}
            end
        end()).