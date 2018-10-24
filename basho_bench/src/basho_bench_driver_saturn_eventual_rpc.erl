-module(basho_bench_driver_saturn_eventual_rpc).

-export([new/1,
         run/4]).

-include("basho_bench.hrl").

-record(state, { nodes,
                 clock,
                 bucket
               }).


%% ====================================================================
%% API
%% ====================================================================

new(Id) ->
    DCs = basho_bench_config:get(saturn_dcs_nodes),
    TargetDC = lists:nth((Id rem length(DCs)+1), DCs),
    Bucket = basho_bench_config:get(saturn_bucket),
    MyNode = basho_bench_config:get(saturn_mynode),

    case net_kernel:start(MyNode) of
        {ok, _} ->
            ?INFO("Net kernel started as ~p\n", [node()]);
        {error, {already_started, _}} ->
            ?INFO("Net kernel already started as ~p\n", [node()]),
            ok;
        {error, Reason} ->
            ?FAIL_MSG("Failed to start net_kernel for ~p: ~p\n", [?MODULE, Reason])
    end,

    Cookie = basho_bench_config:get(saturn_cookie),
    true = erlang:set_cookie(node(), Cookie),
    {ok, #state{nodes=TargetDC, clock=0, bucket=Bucket}}.

run(read, KeyGen, _ValueGen, #state{nodes=Nodes, bucket=Bucket}=S0) ->
    Key = KeyGen(),
    BKey = {Bucket, Key},
    Node = lists:nth(random:uniform(length(Nodes)), Nodes), 
    Result = rpc:call(Node, saturn_leaf, read, [BKey, clock]),
    case Result of
        {ok, _Value} ->
            {ok, S0};
        Else ->
            {error, Else}
    end;

run(update, KeyGen, _ValueGen, #state{nodes=Nodes, bucket=Bucket}=S0) ->
    Key = KeyGen(),
    BKey = {Bucket, Key},
    Node = lists:nth(random:uniform(length(Nodes)), Nodes), 
    Result = rpc:call(Node, saturn_leaf, update, [BKey, value, clock]),
    case Result of
        ok ->
            {ok, S0};
        Else ->
            {error, Else}
    end.
