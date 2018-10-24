-module(reconfig_manager).

-export([new/1,
         run/4]).

-include("basho_bench.hrl").

%% Constants
-define(DATACENTER_ID, 0).
-define(DELAY_SECONDS, 20). %% Time is in seconds
-define(DELAY, 1000*?DELAY_SECONDS). %% Time is in milliseconds.

-record(state, {nodes,
                total_dcs,
                id,
                buckets_map,
                total_buckets,
                start_time}).

%% ====================================================================
%% API
%% ====================================================================

new(Id) ->
    StartTime = get_ts(),
    Nodes = basho_bench_config:get(saturn_dc_nodes),
    NumberDcs = length(Nodes),
    BucketsFileName = basho_bench_config:get(saturn_buckets_file),
    MyNode = basho_bench_config:get(saturn_mynode),    

    %% Store the buckets from all replicas
    {ok, BucketsFile} = file:open(BucketsFileName, [read]),
    Name = list_to_atom(integer_to_list(Id) ++ atom_to_list(buckets)),
    BucketsMap = ets:new(Name, [set, named_table]),
    {ok, TotalBuckets} = get_buckets_from_file(BucketsFile, BucketsMap, 0),
    file:close(BucketsFile),

    %% Enables a node to communicate with others
    case net_kernel:start(MyNode) of
        {ok, _} ->
            ?INFO("Net kernel started as ~p\n", [node()]);
        {error, {already_started, _}} ->
            ?INFO("Net kernel already started as ~p\n", [node()]),
            ok;
        {error, Reason} ->
            ?FAIL_MSG("Failed to start net_kernel for ~p: ~p\n", [?MODULE, Reason])
    end,

    %% Set the cookie for communicating
    Cookie = basho_bench_config:get(saturn_cookie),
    true = erlang:set_cookie(node(), Cookie),
    
    {ok, #state{nodes=Nodes,
                total_dcs=NumberDcs,
                id=Id,
                buckets_map=BucketsMap,
                total_buckets=TotalBuckets,
                start_time=StartTime}}.

get_buckets_from_file(Device, Map, TotalBuckets) ->
    case file:read_line(Device) of
        eof ->
            {ok, TotalBuckets};
        {error, Reason} ->
            lager:error("Problem reading ~p file, reason: ~p", [friends_file, Reason]),
            {error, Reason};
        {ok, Line} ->
            [BucketString|ReplicasString] = string:tokens(hd(string:tokens(Line,"\n")), ","),
            {Bucket, []} = string:to_integer(BucketString),
            Replicas = lists:foldl(fun(Replica, Acc) ->
                                    {Int, []} = string:to_integer(Replica),
                                    [Int|Acc]
                                   end, [], ReplicasString),
            true = ets:insert(Map, {Bucket, lists:sort(Replicas)}),
            get_buckets_from_file(Device, Map, TotalBuckets + 1)
    end.

run(leave_join, KeyGen, _ValueGen, S0=#state{nodes=Nodes,
                                             total_dcs=NumberDcs,
                                             start_time=StartTime}) ->
    %% Choose a cloudlet
    Id = get_random_dc_id(NumberDcs),
    Node = get_node_from_id(Id, NumberDcs, Nodes),

    %% Wait for all clients to be working
    timer:sleep(?DELAY),

    %% Leave
    TimeLeave = get_ts() - StartTime,
    log_relative_time(TimeLeave, leave),
    leave(Id, Node),

    %% Leave an interval for the change to have an impact
    timer:sleep(?DELAY),

    %% Join
    TimeJoin = get_ts() - StartTime,
    log_relative_time(TimeJoin, join),
    join(Id, Node),

    {ok, S0};

run(subscribe_bucket, KeyGen, _ValueGen, S0=#state{nodes=Nodes,
                                                   total_dcs=NumberDcs,
                                                   start_time=StartTime,
                                                   buckets_map=BucketsMap,
                                                   total_buckets=TotalBuckets}) ->
    %% Choose a cloudlet
    Id = get_random_dc_id(NumberDcs),
    Node = get_node_from_id(Id, NumberDcs, Nodes),

    %% Choose a bucket that the cloudlet does not replicate
    Bucket = get_new_cloudlet_bucket(Id, BucketsMap, TotalBuckets),

    %% Wait for all clients to be working
    timer:sleep(?DELAY),

    %% Subscribe to the bucket
    TimeSub = get_ts() - StartTime,
    log_relative_time(TimeSub, subscribe_bucket),
    subscribe_buckets(Id, Node, [Bucket]),

    {ok, S0}.

%% TODO: Add other sequences below!

%% ====================================================================
%% Internal functions
%% ====================================================================

%%%%%%%%%%%%%%%%%%%%%%%%%
%% Reconfiguration API %%
%%%%%%%%%%%%%%%%%%%%%%%%%
leave(Id, Node) ->
    Data = gen_server:call(server_name(Node), {leave, Id}, infinity),
    io:format("Result: ~p", [Data]).

join(Id, Node) ->
    Data = gen_server:call(server_name(Node), {join, Id}, infinity),
    io:format("Result: ~p", [Data]).

fail(Node) ->
    Data = gen_server:call(server_name(Node), fail, infinity),
    io:format("Result: ~p", [Data]).

subscribe_buckets(Id, Node, Buckets) ->
    Data = gen_server:call(server_name(Node), {subscribe_buckets, Id, Buckets}, infinity),
    io:format("Result: ~p", [Data]).

%% The wildcard is processed in the receiving cloudlet
subscribe_objects(Id, Node, Objects) ->
    Data = gen_server:call(server_name(Node), {subscribe_objects, Id, Objects}, infinity),
    io:format("Result: ~p", [Data]).

%%%%%%%%%%%%%%%%%%%%%%
%% Helper functions %%
%%%%%%%%%%%%%%%%%%%%%%
%% Taken from saturn_leaf:saturn_utilities (gives time in microseconds)
get_ts()->
    %% Not very efficient. os:timestamp() faster but non monotonic. Test!
    {MegaSecs, Secs, MicroSecs} = os:timestamp(),
    (MegaSecs * 1000000 + Secs) * 1000000 + MicroSecs.

%% TODO: Check if it is better to output to a file!
log_relative_time(Time, Op) ->
    %% Leave a visible separation between each log
    lager:info("\n\n\n\n\n\n\n\n\n\n", []),
    lager:info("~p - Running: ~p", [Time, Op]),
    lager:info("\n\n\n\n\n\n\n\n\n\n", []).

server_name(Node)->
    {saturn_client_receiver, Node}.

get_node_from_id(Id, NumberDcs, Nodes) ->
    lists:nth((Id rem NumberDcs+1), Nodes).

%% Start of get_different_id/2
get_different_id(Id, NumberDcs) ->
    get_different_id_tail(Id, NumberDcs, ?DATACENTER_ID + 1).

get_different_id_tail(Id, NumberDcs, Index) ->
    case Index < NumberDcs of
        true ->
            case Index == Id of
                true ->
                    get_different_id_tail(Id, NumberDcs, Index + 1);
                false ->
                    {ok, Index}
            end;
        false ->
            {error, "Impossible to get a different Id."}
    end.
%% End of get_different_id/2

get_random_dc_id(NumberDcs) ->
    case (random:uniform(NumberDcs) - 1) of
        ?DATACENTER_ID ->
            get_random_dc_id(NumberDcs);
        Else ->
            Else
    end.

get_new_cloudlet_bucket(Id, BucketsMap, TotalBuckets) ->
    ProposedBucket = random:uniform(TotalBuckets) - 1,
    [{ProposedBucket, Ids}] = ets:lookup(BucketsMap, ProposedBucket),
    case lists:member(Id, Ids) of
        true ->
            get_new_cloudlet_bucket(Id, BucketsMap, TotalBuckets);
        false ->
            true = ets:insert(BucketsMap, {ProposedBucket, Ids ++ [Id]}),
            ProposedBucket
    end.
