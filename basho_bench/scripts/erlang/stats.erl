-module(stats).
-export([dump_stats/1,
         dump_stats_parallel/1,
         cdf/1,
         cdf_converger/1,
         cdf_producer/1,
         average/1,
         cdf_eventual/1,
         cdf_internal/1,
         get_balancing/1,
         dump_stats_eventual/1,
         collect_leave_latencies/1,
         collect_object_subscription_latencies/1]).

-define(NR_STEPS, 1).
-define(LAST_STEP, 90).
-define(NR_DEPS_PERCENTILE, 95).
% -define(NR_DEPS_PERCENTILE, 90).

%% Old buggy version
% dump_stats(ListOfNodes) ->
%     lists:foldl(fun(Node, Counter) ->
%                     Id = integer_to_list(Counter),
%                     NodeName=list_to_atom("leafs"++Id++"@"++Node),
%                     io:format("rpc:call(~p, saturn_leaf, collect_stats, [~p]\n)", [NodeName, Counter-1]),
%                     {ok, Data} = rpc:call(NodeName, saturn_leaf, collect_stats, [Counter-1, updates]),
%                     %{ok, Data} = rpc:call(NodeName, saturn_leaf_converger, dump_stats, [Counter-1, NLeaves]),
%                     lists:foreach(fun(Sender) ->
%                                     io:format("From ~p to ~p: ~p\n", [Sender, Counter-1, dict:fetch(Sender, Data)])
%                                   end, dict:fetch_keys(Data)),
%                     Counter+1
%                  end, 1, ListOfNodes).

% Ask for all nodes, but myself
% SEQUENCIAL VERSION
dump_stats(ListOfNodes) ->
    NrDcs = length(ListOfNodes),
    lists:foldl(fun(Node, Counter) ->
                    %% It must start with the initial counter value
                    dump_stats_internal_loop(Node, Counter, 1, NrDcs),
                    Counter+1
                 end, 1, ListOfNodes).

dump_stats_internal_loop(Node, Counter, Dc, NrDcs) ->
    case Dc =< NrDcs of
        true ->
            case Counter == Dc of
                true ->
                    noop;
                false->
                    Id = integer_to_list(Counter),
                    NodeName=list_to_atom("leafs"++Id++"@"++Node),
                    io:format("rpc:call(~p, saturn_leaf, collect_stats, [~p, updates])\n", [NodeName, Dc-1]),
                    {ok, Data} = rpc:call(NodeName, saturn_leaf, collect_stats, [Dc-1, updates]),

                    %% This is the original version and is the one that should be used in almost all tests
                    lists:foldl(fun(Value, Percentile) ->
                                    case ((Percentile rem ?NR_STEPS) == 0) or
                                    (Percentile >=  ?LAST_STEP) of
                                        true ->
                                            io:format("~p%: ~p ms\n", [Percentile, Value]);
                                        false ->
                                            noop
                                    end,
                                    Percentile+1
                                    end, 1, Data)

                    %% This is the version for the latency in respect to the amount of dependencies
                    % lists:foreach(fun({NrDeps, ListPercentiles}) ->
                    %     Value = lists:nth(?NR_DEPS_PERCENTILE, ListPercentiles),
                    %     io:format("~p deps: ~p ms (~p%)\n", [NrDeps, Value, ?NR_DEPS_PERCENTILE])
                    % end, Data)
            end,
            dump_stats_internal_loop(Node, Counter, Dc + 1, NrDcs);
        false ->
            noop
    end.

%% PARALLEL VERSION
dump_stats_parallel([ReceivingNode, ReceivingId | ListOfNodes]) ->
    %% Convert the args to the node name
    NodeName=list_to_atom("leafs"++ReceivingId++"@"++ReceivingNode),

    %% Request the latencies for this specific node
    lists:foldl(fun(SendingNode, IdDc) ->
                    case SendingNode of
                        ReceivingNode ->
                            noop;

                        %% Request the percentiles for the remote visibility latencies
                        _ ->
                            io:format("rpc:call(~p, saturn_leaf, collect_stats, [~p, updates])\n", [NodeName, IdDc]),
                            {ok, Data} = rpc:call(NodeName, saturn_leaf, collect_stats, [IdDc, updates]),

                            %% This is the original version and is the one that should be used in almost all tests
                            lists:foldl(fun(Value, Percentile) ->
                                            case ((Percentile rem ?NR_STEPS) == 0) or
                                            (Percentile >=  ?LAST_STEP) of
                                                true ->
                                                    io:format("~p%: ~p ms\n", [Percentile, Value]);
                                                false ->
                                                    noop
                                            end,
                                            Percentile+1
                                            end, 1, Data)
                    end,
                    IdDc+1
                 end, 0, ListOfNodes).

% get_id_from_node(MyNode, ListOfNodes) ->
%     get_id_from_node_tail(MyNode, ListOfNodes, 1).

% %% If this happens, there is a problem
% get_id_from_node_tail(_, [], Id) ->
%     Id;

% get_id_from_node_tail(MyNode, [ Node | Rem ], Id) ->
%     case Node of
%         MyNode ->
%             Id;
%         _ ->
%             get_id_from_node_tail(MyNode, Rem, Id + 1)
%     end.

% %% For allowing parallel requests of data
% dump_stats([ MyNode | ListOfNodes ]) ->
%     dump_stats_internal_loop(MyNode, ListOfNodes, get_id_from_node(MyNode, ListOfNodes), 0).

% dump_stats_internal_loop(_, [], _, _) ->
%     noop;

% dump_stats_internal_loop(MyNode, [ Node | Rem ], MyId, Dc) ->
%     case Node of
%         MyNode ->
%             noop;
%         _ ->
%             Id = integer_to_list(MyId),
%             NodeName=list_to_atom("leafs"++Id++"@"++MyNode),
%             io:format("rpc:call(~p, saturn_leaf, collect_stats, [~p, updates])\n", [NodeName, Dc]),
%             {ok, Data} = rpc:call(NodeName, saturn_leaf, collect_stats, [Dc, updates]),

%             lists:foldl(fun(Value, Percentile) ->
%                             case ((Percentile rem ?NR_STEPS) == 0) or
%                             (Percentile >=  ?LAST_STEP) of
%                                 true ->
%                                     io:format("~p%: ~p ms\n", [Percentile, Value]);
%                                 false ->
%                                     noop
%                             end,
%                             Percentile+1
%                             end, 1, Data)
%     end,
%     dump_stats_internal_loop(MyNode, Rem, MyId, Dc + 1).

cdf([LeafString, FromString, TypeString | Leafs]) ->
    Leaf = list_to_integer(LeafString),
    From = list_to_integer(FromString),
    Node = lists:nth(Leaf+1, Leafs),
    NodeName=list_to_atom("leafs"++integer_to_list(Leaf+1)++"@"++Node),
    {ok, Data} = rpc:call(NodeName, saturn_leaf, collect_stats, [Leaf, From, list_to_atom(TypeString)]),
    io:format("From ~p to ~p (~p): ~p\n", [From, Leaf, TypeString, Data]).

cdf_eventual([LeafString, FromString, TypeString | Leafs]) ->
    Leaf = list_to_integer(LeafString),
    From = list_to_integer(FromString),
    Node = lists:nth(Leaf+1, Leafs),
    NodeName=list_to_atom("leafs"++integer_to_list(Leaf+1)++"@"++Node),
    {ok, Data} = rpc:call(NodeName, saturn_leaf, collect_stats, [From, list_to_atom(TypeString)]),
    io:format("From ~p to ~p (~p): ~p\n", [From, Leaf, TypeString, Data]).

average(Leafs) ->
    {Sum, Total, _} = lists:foldl(fun(Node, {Sum0, Total0, Counter}) ->
                                    NodeName=list_to_atom("leafs"++integer_to_list(Counter)++"@"++Node),
                                    {ok, {Sum1, Total1}} = rpc:call(NodeName, saturn_leaf, staleness_average, []),
                                    {Sum0+Sum1, Total0+Total1, Counter+1}
                                  end, {0, 0, 1}, Leafs),
    io:format("Average staleness: ~p\n", [Sum/Total]),
    
    %% Added for allowing the calculation of the remote update throughput
    io:format("Total received remote updates: ~p\n", [Total]).

cdf_converger([LeafString, FromString, TypeString | Leafs]) ->
    Leaf = list_to_integer(LeafString),
    From = list_to_integer(FromString),
    Node = lists:nth(Leaf+1, Leafs),
    NodeName=list_to_atom("leafs"++integer_to_list(Leaf+1)++"@"++Node),
    {ok, Data} = rpc:call(NodeName, saturn_leaf, collect_stats_arrival, [Leaf, From, list_to_atom(TypeString)]),
    io:format("From ~p to ~p (~p): ~p\n", [From, Leaf, TypeString, Data]).

cdf_producer([LeafString, FromString, TypeString | Leafs]) ->
    Leaf = list_to_integer(LeafString),
    From = list_to_integer(FromString),
    Node = lists:nth(Leaf+1, Leafs),
    NodeName=list_to_atom("leafs"++integer_to_list(Leaf+1)++"@"++Node),
    {ok, Data} = rpc:call(NodeName, saturn_leaf, collect_stats_producer, [Leaf, From, list_to_atom(TypeString)]),
    io:format("From ~p to ~p (~p): ~p\n", [From, Leaf, TypeString, Data]).

cdf_internal([LeafString, InternalString, FromString, TypeString | Leafs]) ->
    Leaf = list_to_integer(LeafString),
    From = list_to_integer(FromString),
    Internal = list_to_integer(InternalString),
    Node = lists:nth(Leaf+1, Leafs),
    NodeName=list_to_atom("leafs"++integer_to_list(Leaf+1)++"@"++Node),
    {ok, Data} = rpc:call(NodeName, saturn_leaf, collect_stats_internal, [Internal, From, list_to_atom(TypeString)]),
    io:format("From ~p to ~p (~p): ~p\n", [From, Leaf, TypeString, Data]).

get_balancing([LeafString | Leafs]) ->
    Leaf = list_to_integer(LeafString),
    Node = lists:nth(Leaf+1, Leafs),
    NodeName=list_to_atom("leafs"++integer_to_list(Leaf+1)++"@"++Node),
    {ok, Data} = rpc:call(NodeName, saturn_leaf, get_total_ops, []),
    io:format("Total ops at ~p: ~p\n", [Leaf, Data]).
    

dump_stats_eventual(ListOfNodes) ->
    lists:foldl(fun(Node, Counter) ->
                    Id = integer_to_list(Counter),
                    NodeName=list_to_atom("leafs"++Id++"@"++Node),
                    io:format("rpc:call(~p, saturn_leaf, collect_stats, []\n)", [NodeName]),
                    {ok, Data} = rpc:call(NodeName, saturn_leaf, collect_stats, []),
                    %{ok, Data} = rpc:call(NodeName, saturn_leaf_converger, dump_stats, [Counter-1, NLeaves]),
                    lists:foreach(fun(Sender) ->
                                    io:format("From ~p to ~p: ~p\n", [Sender, Counter-1, dict:fetch(Sender, Data)])
                                  end, dict:fetch_keys(Data)),
                    Counter+1
                 end, 1, ListOfNodes).

%% Collect the latencies for the leave operation
collect_leave_latencies(ListOfNodes) ->
    NrDcs = length(ListOfNodes),
    lists:foldl(fun(Node, Counter) ->
                    %% It must start with the initial counter value
                    collect_leave_latencies_internal_loop(Node, Counter, 1, NrDcs),
                    Counter+1
                 end, 1, ListOfNodes).

collect_leave_latencies_internal_loop(Node, Counter, Dc, NrDcs) ->
    Id = Counter - 1,
    case Dc < NrDcs of
        true ->
            case Id == Dc of
                true ->
                    noop;
                false->
                    %% Get the results from the node
                    NodeId = integer_to_list(Counter),
                    NodeName = list_to_atom("leafs"++NodeId++"@"++Node),
                    io:format("rpc:call(~p, saturn_leaf, collect_leave_latencies, [~p, ~p])\n", [NodeName, Id, Dc]),
                    {ok, Data} = rpc:call(NodeName, saturn_leaf, collect_leave_latencies, [Id, Dc]),

                    %% Calculate the percentiles
                    get_percentiles(Data)
            end,
            collect_leave_latencies_internal_loop(Node, Counter, Dc + 1, NrDcs);
        false ->
            noop
    end.

%% Collect the latencies for the object subscription
collect_object_subscription_latencies(ListOfNodes) ->
    lists:foldl(fun(Node, Counter) ->
                    %% We just ignore the DC (it will not make any subscription)
                    case Counter > 1 of
                        true ->
                            %% Get the results from the node
                            Id = Counter - 1,
                            NodeId = integer_to_list(Counter),
                            NodeName = list_to_atom("leafs"++NodeId++"@"++Node),
                            io:format("rpc:call(~p, saturn_leaf, collect_object_subscription_latencies, [~p])\n", [NodeName, Id]),
                            {ok, Data} = rpc:call(NodeName, saturn_leaf, collect_object_subscription_latencies, [Id]),

                            %% Calculate the percentiles
                            get_percentiles(Data);
                        false ->
                            noop
                    end,
                    Counter+1
                 end, 1, ListOfNodes).

%% Outputs the percentiles
get_percentiles([]) ->
    noop;

get_percentiles(Data) ->
    get_percentiles(Data, length(Data), 1).

get_percentiles(Data, NrElements, 100) ->
    io:format("100%: ~p ms\n", [lists:nth(NrElements, Data)]);

get_percentiles(Data, 1, Percentile) ->
    io:format("~p%: ~p ms\n", [Percentile, lists:nth(1, Data)]),
    get_percentiles(Data, 1, Percentile + 1);

get_percentiles(Data, NrElements, Percentile) ->
    case ((Percentile rem ?NR_STEPS) == 0) or (Percentile >=  ?LAST_STEP) of
        true ->
            Index0 = Percentile / 100 * NrElements,
            Index0Round = round(Index0),
            Value = case Index0 == Index0Round of
                %% It is an integer
                true ->
                    (lists:nth(Index0Round, Data) + lists:nth(Index0Round + 1, Data)) / 2;
                %% It is not an integer
                false ->
                    Index1 = round(Index0 + 0.5), %% Acts as a ceiling
                    lists:nth(Index1, Data)
            end,

            %% Print the result
            io:format("~p%: ~p ms\n", [Percentile, Value]);
        false ->
            noop
    end,
    get_percentiles(Data, NrElements, Percentile + 1).
