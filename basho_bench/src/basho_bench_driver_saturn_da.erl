-module(basho_bench_driver_saturn_da).

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
    %% Make sure the path is setup such that we can get at saturn_leaf
    case code:which(saturn_leaf) of
        non_existing ->
            ?FAIL_MSG("~s requires saturn_leaf module to be available on code path.\n",
                      [?MODULE]);
        _ ->
            ok
    end,

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

    ping_each(TargetDC),

    {ok, #state{nodes=TargetDC, clock=0, bucket=Bucket}}.

run(read, KeyGen, _ValueGen, #state{nodes=Nodes, clock=Clock0, bucket=Bucket}=S0) ->
    Key = KeyGen(),
    BKey = {Bucket, Key},
    Node = lists:nth(random:uniform(length(Nodes)), Nodes), 
    spawn(Node, saturn_leaf, spawn_wrapper, [saturn_leaf, read, self(), [BKey, Clock0]]),
    receive
        {ok, {_Value, TimeStamp}} ->
            Clock1 = max(TimeStamp, Clock0),
            {ok, S0#state{clock=Clock1}};
        Else ->
            {error, Else}
    end;

run(update, KeyGen, ValueGen, #state{nodes=Nodes, clock=Clock0, bucket=Bucket}=S0) ->
    Key = KeyGen(),
    BKey = {Bucket, Key},
    Node = lists:nth(random:uniform(length(Nodes)), Nodes), 
    spawn(Node, saturn_leaf, spawn_wrapper, [saturn_leaf, update, self(), [BKey, ValueGen, Clock0]]),
    receive
        {ok, Clock1} ->
            {ok, S0#state{clock=Clock1}};
        Else ->
            {error, Else}
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

ping_each([]) ->
    ok;
ping_each([Node | Rest]) ->
    case net_adm:ping(Node) of
        pong ->
            ping_each(Rest);
        pang ->
            ?FAIL_MSG("Failed to ping node ~p\n", [Node])
    end.
