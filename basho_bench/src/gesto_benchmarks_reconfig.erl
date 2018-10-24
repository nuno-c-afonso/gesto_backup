-module(gesto_benchmarks_reconfig).

-export([new/1,
         run/4]).

-include("basho_bench.hrl").

%% Constants
-define(TIMEOUT, 150). %% Time is in milliseconds.
-define(DATACENTER_ID, 0).

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
                local_label,
                remote_label,
                left_nodes}).

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
    RemoteTx = basho_bench_config:get(saturntx_remote_percentage),
    KeyTx = basho_bench_config:get(saturntx_n_key),
    

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
                   clock=0,
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
                   local_label=empty,
                   remote_label=empty,
                   left_nodes=sets:new()},
    %lager:info("Worker ~p state: ~p", [Id, State]),
    %log_state(State),
    %lager:info("Worker ~p latencies: ~p", [Id, LatenciesOrdered]),
    {ok, State}.

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

run(read, KeyGen, _ValueGen, #state{node=Node,
                                     clock=Clock0,
                                     correlation=Correlation,
                                     local_buckets=LocalBuckets,
                                     ordered_latencies=OrderedLatencies,
                                     buckets_map=BucketsMap,
                                     mydc=MyDc,
                                     total_dcs=NumberDcs,
                                     local_label=LocalLabel0,
                                     remote_label=RemoteLabel0,
                                     left_nodes=LeftNodes0}=S0) ->
    {ok, Bucket} = case Correlation of
        uniform ->
            pick_local_bucket(uniform, LocalBuckets);
        full ->
            {ok, trunc(math:pow(2, NumberDcs) - 2)};
        _ ->
            pick_local_bucket(Correlation, OrderedLatencies, MyDc, NumberDcs, BucketsMap)
    end,
    
    %% It is needed, because of the reconfiguration
    BKey = {Bucket, KeyGen()},
    try gen_server:call(server_name(Node), {read, BKey, Clock0}, ?TIMEOUT) of
        {ok, {_Value, TimeStamp}} ->
            Clock1 = max(TimeStamp, Clock0),
            case LocalLabel0 of
                empty ->
                    {ok, S0#state{clock=Clock1, local_label={MyDc, Bucket, TimeStamp}}};
                {_, _, Timestamp0} ->
                    case TimeStamp > Timestamp0 of
                        true ->
                            {ok, S0#state{clock=Clock1, local_label={MyDc, Bucket, TimeStamp}}};
                        false ->
                            {ok, S0#state{clock=Clock1}}
                    end
            end;
        {ok, {_Value, TimeStamp, Sender, SerializerClock}} ->
            Clock1 = max(TimeStamp, Clock0),
            case RemoteLabel0 of
                empty ->
                    {ok, S0#state{clock=Clock1, remote_label={Sender, Bucket, SerializerClock}}};
                {_, _, SerializerClock0} ->
                    case SerializerClock > SerializerClock0 of
                        true ->
                            {ok, S0#state{clock=Clock1, remote_label={Sender, Bucket, SerializerClock}}};
                        false ->
                            {ok, S0#state{clock=Clock1}}
                    end
            end;
        %% Machine is leaving -> client must attach to the DC
        {error, down} ->
            migrate_to(?DATACENTER_ID, S0#state{left_nodes=sets:add_element(MyDc, LeftNodes0)});
        Else ->
            {error, Else}
    catch
        %% Cloudlet can be reached, but is blocked
        exit:{timeout, _} ->
            migrate_to(?DATACENTER_ID, S0#state{left_nodes=sets:add_element(MyDc, LeftNodes0)});
        %% Cloudlet cannot be reached
        exit:{noproc, _} ->
            migrate_to(?DATACENTER_ID, S0#state{left_nodes=sets:add_element(MyDc, LeftNodes0)});
        %% Same situation as the previous clause
        exit:{{nodedown, _}, _} ->
            migrate_to(?DATACENTER_ID, S0#state{left_nodes=sets:add_element(MyDc, LeftNodes0)});
        %% Unexpected exception
        Type:Exception ->
            %% It makes it easier to recover the exception
            lager:info("Caught error: Type: ~p\nException: ~p", [Type, Exception]),
            migrate_to(?DATACENTER_ID, S0#state{left_nodes=sets:add_element(MyDc, LeftNodes0)})
    end;

run(remote_read, KeyGen, _ValueGen, S0=#state{total_dcs=NumberDcs,
                                              mydc=MyDc,
                                              left_nodes=LeftNodes}) ->
    %% Choose new replica
    Id = get_random_dc_id_migration(MyDc, NumberDcs, LeftNodes),
    migrate_to(Id, S0);

run(update, KeyGen, ValueGen, #state{node=Node,
                                      clock=Clock0,
                                      correlation=Correlation,
                                      ordered_latencies=OrderedLatencies,
                                      buckets_map=BucketsMap,
                                      mydc=MyDc,
                                      total_dcs=NumberDcs,
                                      local_buckets=LocalBuckets,
                                      left_nodes=LeftNodes0}=S0) ->
    {ok, Bucket} = case Correlation of
        uniform ->
            pick_local_bucket(uniform, LocalBuckets);
        full ->
            {ok, trunc(math:pow(2, NumberDcs) - 2)};
        _ ->
            pick_local_bucket(Correlation, OrderedLatencies, MyDc, NumberDcs, BucketsMap)
    end,

    %% It is needed, because of the reconfiguration
    BKey = {Bucket, KeyGen()},
    try gen_server:call(server_name(Node), {update, BKey, ValueGen(), Clock0}, ?TIMEOUT) of
        {ok, Clock1} ->
            {ok, S0#state{clock=Clock1, local_label={MyDc, Bucket, Clock1}}};
        %% Machine is leaving -> client must attach to the DC
        {error, down} ->
            migrate_to(?DATACENTER_ID, S0#state{left_nodes=sets:add_element(MyDc, LeftNodes0)});
        Else ->
            {error, Else}
    catch
        %% Cloudlet can be reached, but is blocked
        exit:{timeout, _} ->
            migrate_to(?DATACENTER_ID, S0#state{left_nodes=sets:add_element(MyDc, LeftNodes0)});
        %% Cloudlet cannot be reached
        exit:{noproc, _} ->
            migrate_to(?DATACENTER_ID, S0#state{left_nodes=sets:add_element(MyDc, LeftNodes0)});
        %% Same situation as the previous clause
        exit:{{nodedown, _}, _} ->
            migrate_to(?DATACENTER_ID, S0#state{left_nodes=sets:add_element(MyDc, LeftNodes0)});
        %% Unexpected exception
        Type:Exception ->
            %% It makes it easier to recover the exception
            lager:info("Caught error: Type: ~p\nException: ~p", [Type, Exception]),
            migrate_to(?DATACENTER_ID, S0#state{left_nodes=sets:add_element(MyDc, LeftNodes0)})
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

%% Start of get_random_dc_id_migration/3
get_random_dc_id_migration(MyDc, NumberDcs, LeftNodes) ->
    get_random_dc_id_migration_tail(MyDc, NumberDcs, LeftNodes, random:uniform(NumberDcs) - 1).

get_random_dc_id_migration_tail(MyDc, NumberDcs, LeftNodes, NewDc) ->
    case (MyDc == NewDc) or sets:is_element(NewDc, LeftNodes) of
        true ->
            get_random_dc_id_migration_tail(MyDc, NumberDcs, LeftNodes, random:uniform(NumberDcs) - 1);
        false ->
            NewDc
    end.
%% End of get_random_dc_id_migration/3

%% Method to be used for executing the forced migrations
migrate_to(Id, S0=#state{nodes=Nodes,
                         node=MyNode,
                         buckets_map=BucketsMap,
                         total_dcs=NumberDcs,
                         mydc=MyDc,
                         local_label=LocalLabel0,
                         remote_label=RemoteLabel0,
                         left_nodes=LeftNodes0}) ->

    Node = get_node_from_id(Id, NumberDcs, Nodes),
    {LocalBuckets1, RemoteBuckets1, LatenciesOrdered} = update_local_dc(Id, BucketsMap),

    %% Create the migration label in parallel
    gen_server:cast(server_name(MyNode), {migration_partial, MyDc, Id}),

    %% Contact the destination replica
    LocalLabel1 = case LocalLabel0 of
        empty ->
            empty;
        {MyDc, _, Timestamp} ->
            {MyDc, Timestamp}
    end,
    RemoteLabel1 = case RemoteLabel0 of
        empty ->
            empty;
        {Sender, _, Counter} ->
            {Sender, Counter}
    end,

    %% It is needed, because of the reconfiguration
    try gen_server:call(server_name(Node), {migration, Id, LocalLabel1, RemoteLabel1}, ?TIMEOUT) of
        {ok, {SerializerClock, NewLeftNodes}} ->
            {ok, S0#state{node=Node,
                            mydc=Id,
                            local_buckets=LocalBuckets1,
                            remote_buckets=RemoteBuckets1,
                            ordered_latencies=LatenciesOrdered,
                            local_label=empty,
                            remote_label=SerializerClock,
                            left_nodes=NewLeftNodes}};
        %% Machine is leaving -> client must attach to the DC
        {error, down} ->
            migrate_to(?DATACENTER_ID, S0#state{left_nodes=sets:add_element(Id, LeftNodes0)});
        Else ->
            {error, Else}
    catch
        %% Cloudlet can be reached, but is blocked
        exit:{timeout, _} ->
            migrate_to(?DATACENTER_ID, S0#state{left_nodes=sets:add_element(Id, LeftNodes0)});
        %% Cloudlet cannot be reached
        exit:{noproc, _} ->
            migrate_to(?DATACENTER_ID, S0#state{left_nodes=sets:add_element(Id, LeftNodes0)});
        %% Same situation as the previous clause
        exit:{{nodedown, _}, _} ->
            migrate_to(?DATACENTER_ID, S0#state{left_nodes=sets:add_element(Id, LeftNodes0)});
        %% Unexpected exception
        Type:Exception ->
            %% It makes it easier to recover the exception
            lager:info("Caught error: Type: ~p\nException: ~p", [Type, Exception]),
            migrate_to(?DATACENTER_ID, S0#state{left_nodes=sets:add_element(Id, LeftNodes0)})
    end.

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
