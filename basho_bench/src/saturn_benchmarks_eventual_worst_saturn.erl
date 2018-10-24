-module(saturn_benchmarks_eventual_worst_saturn).

-export([new/1,
         run/4]).

-include("basho_bench.hrl").

-record(state, {node,
                nodes,
                clock,
                mydc,
                id,
                correlation,
                local_buckets,
                remote_buckets,
                ordered_latencies,
                total_dcs,
                remote_tx,
                key_tx,
                buckets_map,
                closest_replicas,
                original_replica}).

%% ====================================================================
%% API
%% ====================================================================

new(Id) ->
    Nodes = basho_bench_config:get(saturn_dc_nodes),
    Correlation = basho_bench_config:get(saturn_correlation),
    MyNode = basho_bench_config:get(saturn_mynode),
    MyDc = basho_bench_config:get(saturn_dc_id),
    BucketsFileName = basho_bench_config:get(saturn_buckets_file),
    TreeFileName = basho_bench_config:get(saturn_tree_file),
    ClosestReplicasFileName = basho_bench_config:get(saturn_worst),
    RemoteTx = basho_bench_config:get(saturntx_remote_percentage),
    KeyTx = basho_bench_config:get(saturntx_n_key),
    
    %% Force the same sequence for both migration clients
    random:seed(),

    {ok, BucketsFile} = file:open(BucketsFileName, [read]),
    Name = list_to_atom(integer_to_list(Id) ++ atom_to_list(buckets)),
    BucketsMap = ets:new(Name, [set, named_table]),
    {ok, {LocalBuckets, RemoteBuckets}} = get_buckets_from_file(BucketsFile, MyDc, [], [], BucketsMap),
    file:close(BucketsFile),

    {ok, TreeFile} = file:open(TreeFileName, [read]),
    {ok, {LatenciesOrdered, _NumberLines}} = get_tree_from_file(TreeFile, MyDc, 0, []),
    %% Quick fix for allowing explicit migration
    NumberDcs = length(Nodes),
    file:close(TreeFile),

    %% This is the new file that dictates the replica that clients migrate to
    {ok, ClosestReplicasFile} = file:open(ClosestReplicasFileName, [read]),
    {ok, ClosestReplicasDict} = get_closest_replicas_from_file(ClosestReplicasFile),
    file:close(ClosestReplicasFile),

    case net_kernel:start(MyNode) of
        {ok, _} ->
            ?INFO("Net kernel started as ~p\n", [node()]);
        {error, {already_started, _}} ->
            ?INFO("Net kernel already started as ~p\n", [node()]),
            ok;
        {error, Reason} ->
            ?FAIL_MSG("Failed to start net_kernel for ~p: ~p\n", [?MODULE, Reason])
    end,
    Node = lists:nth((MyDc rem length(Nodes)+1), Nodes),
    Cookie = basho_bench_config:get(saturn_cookie),
    true = erlang:set_cookie(node(), Cookie),

    ok = ping_each(Nodes),

    case Id of
        1 ->
            ok = rpc:call(Node, saturn_leaf, clean, [MyDc]),
            timer:sleep(5000);
        _ ->
            noop
    end,
    
    State = #state{node=Node,
                   nodes=Nodes,
                   mydc=MyDc,
                   remote_tx=RemoteTx,
                   key_tx=KeyTx,
                   correlation=Correlation,
                   local_buckets=LocalBuckets,
                   remote_buckets=RemoteBuckets,
                   ordered_latencies=LatenciesOrdered,
                   total_dcs=NumberDcs,
                   buckets_map=BucketsMap,
                   id=Id,
                   closest_replicas=ClosestReplicasDict,
                   original_replica=MyDc},
    {ok, State}.

get_closest_replicas_from_file(Device) ->
    get_closest_replicas_from_file(Device, dict:new()).

get_closest_replicas_from_file(Device, DictRes) ->
    case file:read_line(Device) of
        eof ->
            case dict:size(DictRes) of
                0 ->
                    {error, not_enough};
                _ ->
                    {ok, DictRes}
            end;

        {error, Reason} ->
            lager:error("Problem reading ~p file, reason: ~p", [closest_replicas, Reason]),
            {error, Reason};

        {ok, Line} ->
            [OriginString|DestinationList] = string:tokens(hd(string:tokens(Line,"\n")), ","),
            {IdOrigin, []} = string:to_integer(OriginString),
            {IdDestination, []} = string:to_integer(hd(DestinationList)),

            get_closest_replicas_from_file(Device, dict:store(IdOrigin, IdDestination, DictRes))
    end.

get_tree_from_file(Device, MyDc, Counter, OrderedList) ->
    case file:read_line(Device) of
        eof ->
            case (Counter > MyDc) of
                true ->
                    {ok, {OrderedList, Counter}};
                false ->
                    {error, not_enough}
            end;
        {error, Reason} ->
            lager:error("Problem reading ~p file, reason: ~p", [friends_file, Reason]),
            {error, Reason};
        {ok, Line} ->
            case Counter of
                MyDc ->
                    %lager:info("my row: ~p", [Line]),
                    ListString = string:tokens(hd(string:tokens(Line,"\n")), ","),
                    {List, _} = lists:foldl(fun(LatencyString, {Acc, DcId}) ->
                                                {Latency, []} = string:to_integer(LatencyString),
                                                {orddict:store(Latency, DcId, Acc), DcId+1}
                                            end, {orddict:new(), 0}, ListString),
                    get_tree_from_file(Device, MyDc, Counter+1, List);
                _ ->
                    get_tree_from_file(Device, MyDc, Counter+1, OrderedList)
            end
    end.

get_buckets_from_file(Device, MyDc, Local, Remote, Map) ->
    case file:read_line(Device) of
        eof ->
            {ok, {Local, Remote}};
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
            %lager:info("Inserted to ets: ~p, ~p", [lists:sort(Replicas), Bucket]),
            true = ets:insert(Map, {lists:sort(Replicas), Bucket}),
            case lists:member(MyDc, Replicas) of
                true ->
                    get_buckets_from_file(Device, MyDc, [Bucket|Local], Remote, Map);
                false ->
                    get_buckets_from_file(Device, MyDc, Local, [Bucket|Remote], Map)
            end
    end.

pick_local_bucket(proportional, [_MySelf|LatenciesOrderedDcs], MyDc, NumberDcs, BucketsMap) ->
    Group = get_bucket_proportional(LatenciesOrderedDcs, NumberDcs),
    [{_, Bucket}] = ets:lookup(BucketsMap, lists:sort([MyDc|Group])),
    {ok, Bucket};

pick_local_bucket(exponential, [_MySelf|LatenciesOrderedDcs], MyDc, NumberDcs, BucketsMap) ->
    Group = get_bucket_exponential(LatenciesOrderedDcs, NumberDcs),
    [{_, Bucket}] = ets:lookup(BucketsMap, lists:sort([MyDc|Group])),
    {ok, Bucket}.

pick_local_bucket(uniform, MyBuckets) ->
    Pos = random:uniform(length(MyBuckets)),
    Bucket = lists:nth(Pos, MyBuckets),
    {ok, Bucket}.

pick_remote_bucket(proportional, [_MySelf|LatenciesOrderedDcs]=Latencies, NumberDcs, BucketsMap) ->
    case get_bucket_proportional(LatenciesOrderedDcs, NumberDcs) of
        [] ->
            pick_remote_bucket(proportional, Latencies, NumberDcs, BucketsMap);
        Group ->
            [{_, Bucket}] = ets:lookup(BucketsMap, lists:sort(Group)),
            {ok, Bucket}
    end;

pick_remote_bucket(exponential, [_MySelf|LatenciesOrderedDcs]=Latencies, NumberDcs, BucketsMap) ->
    case get_bucket_exponential(LatenciesOrderedDcs, NumberDcs) of
        [] ->
            pick_remote_bucket(exponential, Latencies, NumberDcs, BucketsMap);
        Group ->
            [{_, Bucket}] = ets:lookup(BucketsMap, lists:sort(Group)),
            {ok, Bucket}
    end.

pick_remote_bucket(uniform, RemoteBuckets) ->
    Pos = random:uniform(length(RemoteBuckets)),
    Bucket = lists:nth(Pos, RemoteBuckets),
    {ok, Bucket}.

get_bucket_proportional(LatenciesOrderedDcs, NumberDcs) ->    
    {_, DCs} = lists:foldl(fun({_Latency, DC}, {Counter, List}) ->
                            Portion = trunc(100/NumberDcs),
                            Prob = (NumberDcs-Counter)*Portion,
                            case random:uniform(100) =< Prob of
                                true ->
                                    {Counter + 1, [DC|List]};
                                false ->
                                    {Counter + 1, List}
                            end
                           end, {1, []}, LatenciesOrderedDcs),
    DCs.

get_bucket_exponential(LatenciesOrderedDcs, NumberDcs) ->    
    {_ ,DCs} = lists:foldl(fun({_Latency, DC}, {Counter, List}) ->
                            Max = math:pow(2, NumberDcs),
                            Upper = math:pow(2, (NumberDcs - Counter)) * 100,
                            Prob = trunc(Upper/Max),
                            case random:uniform(100) =< Prob of
                                true ->
                                    {Counter + 1, [DC|List]};
                                false ->
                                    {Counter + 1, List}
                            end
                           end, {1, []}, LatenciesOrderedDcs),
    DCs.

get_bkeys(0, _KeyGen, BKeys, _S0) ->
    BKeys;

get_bkeys(Rest, KeyGen, BKeys, S0=#state{remote_tx=PercentageRemote,
                                 correlation=Correlation,
                                 local_buckets=LocalBuckets,
                                 remote_buckets=RemoteBuckets,
                                 ordered_latencies=OrderedLatencies,
                                 buckets_map=BucketsMap,
                                 mydc=MyDc,
                                 total_dcs=NumberDcs}) ->
    Type = case (PercentageRemote>random:uniform(100)) of
        true -> remote;
        false -> local
    end,
    {ok, Bucket} = case {Type, Correlation} of
        {local, uniform} ->
            pick_local_bucket(uniform, LocalBuckets);
        {remote, uniform} ->
            pick_remote_bucket(uniform, RemoteBuckets);
        {local, _} ->
            pick_local_bucket(Correlation, OrderedLatencies, MyDc, NumberDcs, BucketsMap);
        {remote, _} ->
            pick_remote_bucket(Correlation, OrderedLatencies, NumberDcs, BucketsMap);
        {_, full} ->
            {ok, trunc(math:pow(2,NumberDcs) - 2)}
    end,
    Key = generate_key(KeyGen, Bucket, BKeys),
    get_bkeys(Rest-1, KeyGen, [{Bucket, Key}|BKeys], S0).

generate_key(KeyGen, Bucket, BKeys) -> 
    Key = KeyGen(),
    case lists:member({Bucket, Key}, BKeys) of
        true ->
            generate_key(KeyGen, Bucket, BKeys);
        false ->
            Key
    end.

run(read, KeyGen, _ValueGen, S0) ->
    read(KeyGen, S0);

run(read_tx, KeyGen, _ValueGen, #state{node=Node,
                                        key_tx=NKeys,
                                        clock=Clock0}=S0) ->
    BKeys = get_bkeys(NKeys, KeyGen, [], S0),
    Result = gen_server:call(server_name(Node), {read_tx, BKeys, Clock0}, infinity),
    case Result of
        {ok, {_Value, TimeStamp}} ->
            Clock1 = max(TimeStamp, Clock0),
            {ok, S0#state{clock=Clock1}};
        Else ->
            {error, Else}
    end;

%% It goes directly to the second replica, without enforcing causality
run(remote_read, KeyGen, ValueGen, S0=#state{local_buckets=LocalBuckets}) ->
    %% Choose an object that is on both replicas
    [Bucket | _] = LocalBuckets,
    BKey = {Bucket, KeyGen()},

    %% Update object
    {ok, S1} = update_bkey(BKey, ValueGen, S0),
    %% Migrate
    {ok, S2} = migration(KeyGen, S1),
    %% Read object
    read_bkey(BKey, S2);

%% To be used for testing the transition from a cloudlet to another cloudlet
run(double_migration_different, KeyGen, _ValueGen, #state{node=Node,
                                            nodes=Nodes,
                                            mydc=MyDc,
                                            clock=Clock0,
                                            correlation=Correlation,
                                            remote_buckets=RemoteBuckets,
                                            ordered_latencies=OrderedLatencies,
                                            buckets_map=BucketsMap,
                                            total_dcs=NumberDcs}=S0) ->
    %% The DC will be identified by 0
    Id = 0,
    BKey = {Id, KeyGen()},

    %%%%%%%%%%%%%%%%%%%%%
    %% First migration %%
    %%%%%%%%%%%%%%%%%%%%%
    Result1 = gen_server:call(server_name(Node), {read, BKey, Clock0}, infinity),
    case Result1 of
        {ok, {Id, TimeStamp1}} ->
            Node1 = get_node_from_id(Id, NumberDcs, Nodes),
            Result2 = gen_server:call(server_name(Node1), {read, BKey, MyDc, TimeStamp1, Id}, infinity),
            case Result2 of
                {ok, {_Value, TimeStamp2}} ->
                    Clock1 = max(TimeStamp2, Clock0),
                    {ok, IdFinal} = get_different_id(MyDc, Id, NumberDcs),
                    
                    %% Each DC has an unique bucket, which corresponds to its Id
                    BKey1 = {IdFinal, KeyGen()},
                    
                    %%%%%%%%%%%%%%%%%%%%%%
                    %% Second migration %%
                    %%%%%%%%%%%%%%%%%%%%%%
                    Result3 = gen_server:call(server_name(Node1), {read, BKey1, Clock1}, infinity),
                    case Result3 of
                        {ok, {IdFinal, TimeStamp3}} ->
                            Node2 = get_node_from_id(IdFinal, NumberDcs, Nodes),
                            Result4 = gen_server:call(server_name(Node2), {read, BKey1, Id, TimeStamp3, IdFinal}, infinity),
                            case Result4 of
                                {ok, {_Value2, TimeStamp4}} ->
                                    Clock2 = max(TimeStamp4, Clock1),
                                    {LocalBuckets1, RemoteBuckets1, LatenciesOrdered} = update_local_dc(IdFinal, BucketsMap),
                                    {ok, S0#state{clock=Clock2,
                                                node=Node2,
                                                mydc=IdFinal,
                                                local_buckets=LocalBuckets1,
                                                remote_buckets=RemoteBuckets1,
                                                ordered_latencies=LatenciesOrdered}};
                                Else ->
                                    {error, Else}
                            end;
                        Else ->
                            {error, Else}
                    end;
                Else ->
                    {error, Else}
            end;
        Else ->
            {error, Else}
    end;

%% To be used for testing the transition from a cloudlet to the DC and coming back
run(double_migration_same, KeyGen, _ValueGen, #state{node=Node,
                                            nodes=Nodes,
                                            mydc=MyDc,
                                            clock=Clock0,
                                            correlation=Correlation,
                                            remote_buckets=RemoteBuckets,
                                            ordered_latencies=OrderedLatencies,
                                            buckets_map=BucketsMap,
                                            total_dcs=NumberDcs}=S0) ->
    %% The DC will be identified by 0
    Id = 0,
    BKey = {Id, KeyGen()},

    %%%%%%%%%%%%%%%%%%%%%
    %% First migration %%
    %%%%%%%%%%%%%%%%%%%%%
    Result1 = gen_server:call(server_name(Node), {read, BKey, Clock0}, infinity),
    case Result1 of
        {ok, {Id, TimeStamp1}} ->
            Node1 = get_node_from_id(Id, NumberDcs, Nodes),
            Result2 = gen_server:call(server_name(Node1), {read, BKey, MyDc, TimeStamp1, Id}, infinity),
            case Result2 of
                {ok, {_Value, TimeStamp2}} ->
                    Clock1 = max(TimeStamp2, Clock0),                    
                    %% Each DC has an unique bucket, which corresponds to its Id
                    BKey1 = {MyDc, KeyGen()},
                    
                    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                    %% Second migration: Return to the original replica %%
                    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                    Result3 = gen_server:call(server_name(Node1), {read, BKey1, Clock1}, infinity),
                    case Result3 of
                        {ok, {MyDc, TimeStamp3}} ->
                            Result4 = gen_server:call(server_name(Node), {read, BKey1, Id, TimeStamp3, MyDc}, infinity),
                            case Result4 of
                                {ok, {_Value2, TimeStamp4}} ->
                                    Clock2 = max(TimeStamp4, Clock1),
                                    {ok, S0#state{clock=Clock2}};
                                Else ->
                                    {error, Else}
                            end;
                        Else ->
                            {error, Else}
                    end;
                Else ->
                    {error, Else}
            end;
        Else ->
            {error, Else}
    end;

run(write_tx, KeyGen, ValueGen, #state{node=Node,
                                        key_tx=NKeys,
                                        clock=Clock0}=S0) ->
    BKeys = get_bkeys(NKeys, KeyGen, [], S0),
    Pairs = [{BKey, ValueGen()} || BKey <- BKeys],
    Result = gen_server:call(server_name(Node), {write_tx, Pairs, Clock0}, infinity),
    case Result of
        {ok, Clock1} ->
            {ok, S0#state{clock=Clock1}};
        Else ->
            {error, Else}
    end;

run(update, KeyGen, ValueGen, S0) ->
    update(KeyGen, ValueGen, S0);

run(remote_update, KeyGen, ValueGen, #state{node=Node,
                                             clock=Clock0,
                                             correlation=Correlation,
                                             remote_buckets=RemoteBuckets,
                                             ordered_latencies=OrderedLatencies,
                                             buckets_map=BucketsMap,
                                             total_dcs=NumberDcs}=S0) ->
    {ok, Bucket} = case Correlation of
        uniform ->
            pick_remote_bucket(uniform, RemoteBuckets);
        full ->
            {ok, trunc(math:pow(2, NumberDcs) - 2)};
        _ ->
            pick_remote_bucket(Correlation, OrderedLatencies, NumberDcs, BucketsMap)
    end,
    BKey = {Bucket, KeyGen()},
    Result = gen_server:call(server_name(Node), {update, BKey, ValueGen(), Clock0}, infinity),
    %Result = rpc:call(Node, saturn_leaf, update, [BKey, value, Clock0]),
    case Result of
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

server_name(Node)->
    {saturn_client_receiver, Node}.
    %{global, list_to_atom(atom_to_list(Node) ++ atom_to_list(saturn_client_receiver))}.

get_node_from_id(Id, NumberDcs, Nodes) ->
    lists:nth((Id rem NumberDcs+1), Nodes).

update_local_dc(Id, BucketsMap) ->
    %% Update local and remote buckets
    BucketsFileName = basho_bench_config:get(saturn_buckets_file),
    {ok, BucketsFile} = file:open(BucketsFileName, [read]),
    ets:delete_all_objects(BucketsMap),
    {ok, {LocalBuckets, RemoteBuckets}} = get_buckets_from_file(BucketsFile, Id, [], [], BucketsMap),
    file:close(BucketsFile),
    
    %% Update ordered latencies
    TreeFileName = basho_bench_config:get(saturn_tree_file),
    {ok, TreeFile} = file:open(TreeFileName, [read]),
    {ok, {LatenciesOrdered, _NumberLines}} = get_tree_from_file(TreeFile, Id, 0, []),
    file:close(TreeFile),

    {LocalBuckets, RemoteBuckets, LatenciesOrdered}.

%% Start of get_different_id/3
get_different_id(MyDc, Id, NumberDcs) ->
    get_different_id_tail(MyDc, Id, NumberDcs, 0).

get_different_id_tail(MyDc, Id, NumberDcs, Index) ->
    case Index < NumberDcs of
        true ->
            case (Index == MyDc) or (Index == Id) of
                true ->
                    get_different_id_tail(MyDc, Id, NumberDcs, Index + 1);
                false ->
                    {ok, Index}
            end;
        false ->
            {error, "Impossible to get a different Id."}
    end.
%% End of get_different_id/3

%% Start of get_random_dc_id/2
get_random_dc_id(MyDc, NumberDcs) ->
    get_random_dc_id_tail(MyDc, NumberDcs, random:uniform(NumberDcs) - 1).

get_random_dc_id_tail(MyDc, NumberDcs, NewDc) ->
    case MyDc == NewDc of
        true ->
            get_random_dc_id_tail(MyDc, NumberDcs, random:uniform(NumberDcs) - 1);
        false ->
            NewDc
    end.
%% End of get_random_dc_id/2

log_state(State) ->
    lager:info("Worker ~p state:~nNode:~p~nNodes:~p~nClock:~p~nMyDC:~p~nRemote_TX:~p~nKey_TX:~p~nCorrelation:~p~nLocal_Buckets:~p~nRemote_Buckets:~p~nOrdered_Latencies:~p~nTotalDCs:~p~nBuckets_Map:~p",
    [State#state.id,
     State#state.node,
     State#state.nodes,
     State#state.clock,
     State#state.mydc,
     State#state.remote_tx,
     State#state.key_tx,
     State#state.correlation,
     State#state.local_buckets,
     State#state.remote_buckets,
     State#state.ordered_latencies,
     State#state.total_dcs,
     State#state.buckets_map]).


%% Operations
read(KeyGen, S0=#state{correlation=Correlation,
                       local_buckets=LocalBuckets,
                       ordered_latencies=OrderedLatencies,
                       buckets_map=BucketsMap,
                       mydc=MyDc,
                       total_dcs=NumberDcs}) ->

    %% Choose the object partition
    {ok, Bucket} = case Correlation of
        uniform ->
            pick_local_bucket(uniform, LocalBuckets);
        full ->
            {ok, trunc(math:pow(2, NumberDcs) - 2)};
        _ ->
            pick_local_bucket(Correlation, OrderedLatencies, MyDc, NumberDcs, BucketsMap)
    end,
    BKey = {Bucket, KeyGen()},

    %% Make the request
    read_bkey(BKey, S0).

read_bkey(BKey, S0=#state{node=Node}) ->
    Result = gen_server:call(server_name(Node), {read, BKey}, infinity),
    case Result of
        {ok, _Value} ->
            {ok, S0};
        Else ->
            {error, Else}
    end.

update(KeyGen, ValueGen, S0=#state{correlation=Correlation,
                                   ordered_latencies=OrderedLatencies,
                                   buckets_map=BucketsMap,
                                   mydc=MyDc,
                                   total_dcs=NumberDcs,
                                   local_buckets=LocalBuckets}) ->

    %% Choose the key
    {ok, Bucket} = case Correlation of
        uniform ->
            pick_local_bucket(uniform, LocalBuckets);
        full ->
            {ok, trunc(math:pow(2, NumberDcs) - 2)};
        _ ->
            pick_local_bucket(Correlation, OrderedLatencies, MyDc, NumberDcs, BucketsMap)
    end,
    BKey = {Bucket, KeyGen()},

    %% Make the request
    update_bkey(BKey, ValueGen, S0).

update_bkey(BKey, ValueGen, S0=#state{node=Node}) ->
    Result = gen_server:call(server_name(Node), {update, BKey, ValueGen()}, infinity),
    case Result of
        ok ->
            {ok, S0};
        Else ->
            {error, Else}
    end.

migration(KeyGen, S0=#state{nodes=Nodes,
                           buckets_map=BucketsMap,
                           total_dcs=NumberDcs,
                           mydc=MyDc,
                           closest_replicas=ClosestReplicas,
                           original_replica=IdOriginalReplica}) ->

    %% Choose new replica (from closest_replicas)
    Id = case MyDc of
        IdOriginalReplica ->
            dict:fetch(MyDc, ClosestReplicas);
        _ ->
            IdOriginalReplica
    end,
    Node = get_node_from_id(Id, NumberDcs, Nodes),
    BKey = {Id, KeyGen()},
    Result = gen_server:call(server_name(Node), {read, BKey}, infinity),
    case Result of
        {ok, _Value} ->
            {LocalBuckets, RemoteBuckets, LatenciesOrdered} = update_local_dc(Id, BucketsMap),
            {ok, S0#state{node=Node,
                          mydc=Id,
                          local_buckets=LocalBuckets,
                          remote_buckets=RemoteBuckets,
                          ordered_latencies=LatenciesOrdered}};
        Else ->
            {error, Else}
    end.