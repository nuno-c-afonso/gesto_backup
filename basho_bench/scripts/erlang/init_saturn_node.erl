-module(init_saturn_node).
-export([init/1,
         init_all/1,
         init_all_txs/1, 
         init_all_p2p/1, 
         init_all_receivers/1,
         init_all_eventual/1,
         test1/1]).

init([NodeString, TypeString, IdString, NLeafsString | ListOfNodes]) ->
    NLeafs = list_to_integer(NLeafsString),
    NodeNames = generate_node_names(NLeafs, ListOfNodes),
    Node = list_to_atom(NodeString),
    Type = list_to_atom(TypeString),
    Id = list_to_integer(IdString),
    Port = 4042,
    case Type of
	    internals ->
	        {ok, _} = rpc:call(Node, saturn_leaf_sup, start_internal, [Port, Id]);
	    leafs ->
	        {ok, _} = rpc:call(Node, saturn_leaf_sup, start_leaf, [Port, Id]),
	        ok = rpc:call(Node, saturn_leaf_producer, check_ready, [Id])
    end,
    % This should eventually be done through a fiile.
    %Tree0 = dict:store(0, [-1, 300, 50], dict:new()).
    %Tree1 = dict:store(1, [300, -1, 70], Tree0). 
    %Tree2 = dict:store(2, [50, 70, -1], Tree1).
    %Groups0 = dict:store(1, [0, 1], dict:new()).
    %rpc:call(Node, groups_manager_serv, set_treedict, [Tree2, NLeafs]),
    %rpc:call(Node, groups_manager_serv, set_groupsdict,[Groups0]),
    ping_all(Node, NodeNames).

init_all([NLeafsString | ListOfNodes]) ->
    NLeafs = list_to_integer(NLeafsString),
    NodeNames = generate_node_names(NLeafs, ListOfNodes),
    Port = 4042,
    lists:foldl(fun(Node, Counter) ->
                    case (Counter > NLeafs) of
                        true ->
                            Id = integer_to_list(Counter - NLeafs),
                            NodeName=list_to_atom("internals"++Id++"@"++Node),
                            ping_all(NodeName, NodeNames),
                            {ok, _} = rpc:call(NodeName, saturn_leaf_sup, start_internal, [Port, Counter-1]),
                            %io:format("Node: ~p, id: ~p", [NodeName, Counter-1]),
                            Counter+1;
                        false ->
                            Id = integer_to_list(Counter),
                            NodeName=list_to_atom("leafs"++Id++"@"++Node),
                            ping_all(NodeName, NodeNames),
                            {ok, _} = rpc:call(NodeName, saturn_leaf_sup, start_leaf, [Port, Counter-1]),
                            ok = rpc:call(NodeName, saturn_leaf_producer, check_ready, [Counter-1]),
                            %io:format("Node: ~p, id: ~p", [NodeName, Counter-1]),
                            Counter+1
                    end
                 end, 1, ListOfNodes).

init_all_txs([NLeafsString | ListOfNodes]) ->
    NLeafs = list_to_integer(NLeafsString),
    NodeNames = generate_node_names(NLeafs, ListOfNodes),
    Port = 4042,
    lists:foldl(fun(Node, Counter) ->
                    case (Counter > NLeafs) of
                        true ->
                            Id = integer_to_list(Counter - NLeafs),
                            NodeName=list_to_atom("internals"++Id++"@"++Node),
                            ping_all(NodeName, NodeNames),
                            {ok, _} = rpc:call(NodeName, saturn_leaf_sup, start_internal, [Port, Counter-1]),
                            %io:format("Node: ~p, id: ~p", [NodeName, Counter-1]),
                            Counter+1;
                        false ->
                            Id = integer_to_list(Counter),
                            NodeName=list_to_atom("leafs"++Id++"@"++Node),
                            ping_all(NodeName, NodeNames),
                            {ok, _} = rpc:call(NodeName, saturn_leaf_sup, start_leaf, [Port, Counter-1]),
                            ok = rpc:call(NodeName, saturn_leaf_producer, check_ready, [Counter-1]),
                            %io:format("Node: ~p, id: ~p", [NodeName, Counter-1]),
                            Counter+1
                    end
                 end, 1, ListOfNodes),
    lists:foldl(fun(Node, Counter) ->
                    case (Counter > NLeafs) of
                        true ->
                            Counter+1;
                        false ->
                            Id = integer_to_list(Counter),
                            NodeName=list_to_atom("leafs"++Id++"@"++Node),
                            ok = rpc:call(NodeName, saturn_leaf_receiver, assign_convergers, [Counter-1, NLeafs]),
                            Counter+1
                    end
                 end, 1, ListOfNodes).

init_all_p2p([NLeafsString | ListOfNodes]) ->
    NLeafs = list_to_integer(NLeafsString),
    NodeNames = generate_node_names(NLeafs, ListOfNodes),
    Port = 4042,
    lists:foldl(fun(Node, Counter) ->
                    Id = integer_to_list(Counter),
                    NodeName=list_to_atom("leafs"++Id++"@"++Node),
                    ping_all(NodeName, NodeNames),
                    {ok, _} = rpc:call(NodeName, saturn_leaf_sup, start_leaf, [Port, Counter-1, NLeafs]),
                    ok = rpc:call(NodeName, saturn_leaf_producer, check_ready, [Counter-1]),
                    %io:format("Node: ~p, id: ~p", [NodeName, Counter-1]),
                    Counter+1
                end, 1, ListOfNodes),
    lists:foldl(fun(Node, Counter) ->
                    Id = integer_to_list(Counter),
                    NodeName=list_to_atom("leafs"++Id++"@"++Node),
                    ok = rpc:call(NodeName, saturn_leaf_receiver, assign_convergers, [Counter-1, NLeafs]),
                    Counter+1
                 end, 1, ListOfNodes).

init_all_receivers([NLeafsString | ListOfNodes]) ->
    NLeafs = list_to_integer(NLeafsString),
    NodeNames = generate_node_names_receivers(NLeafs, ListOfNodes),
    Port = 4042,
    lists:foldl(fun(Node, Counter) ->
                    case (Counter > 2*NLeafs) of
                        true ->
                            Id = integer_to_list(Counter - 2*NLeafs),
                            NodeName=list_to_atom("internals"++Id++"@"++Node),
                            ping_all(NodeName, NodeNames),
                            {ok, _} = rpc:call(NodeName, saturn_leaf_sup, start_internal, [Port, Counter-(1+NLeafs)]),
                            %io:format("Node: ~p, id: ~p", [NodeName, Counter-1]),
                            Counter+1;
                        false ->
                            case (Counter > NLeafs) of
                                false ->
                                    Id = integer_to_list(Counter),
                                    NodeName=list_to_atom("leafs"++Id++"@"++Node),
                                    ping_all(NodeName, NodeNames),
                                    {ok, _} = rpc:call(NodeName, saturn_leaf_sup, start_leaf, [Port, Counter-1]),
                                    ok = rpc:call(NodeName, saturn_leaf_producer, check_ready, [Counter-1]),
                                    %io:format("Node: ~p, id: ~p", [NodeName, Counter-1]),
                                    Counter+1;
                                true ->
                                    Id = integer_to_list(Counter-NLeafs),
                                    NodeName=list_to_atom("receivers"++Id++"@"++Node),
                                    ping_all(NodeName, NodeNames),
                                    ok = rpc:call(NodeName, saturn_leaf_sup, start_receiver, [Counter-(1+NLeafs)]),
                                    %io:format("Node: ~p, id: ~p", [NodeName, Counter-1]),
                                    Counter+1
                            end
                    end
                 end, 1, ListOfNodes),
    lists:foldl(fun(Node, Counter) ->
                    case (Counter > NLeafs) of
                        true ->
                            Counter+1;
                        false ->
                            Id = integer_to_list(Counter),
                            NodeName=list_to_atom("leafs"++Id++"@"++Node),
                            ok = rpc:call(NodeName, saturn_leaf_receiver, assign_convergers, [Counter-1, NLeafs]),
                            Counter+1
                    end
                 end, 1, ListOfNodes).

init_all_eventual([NLeafsString | ListOfNodes]) ->
    NLeafs = list_to_integer(NLeafsString),
    NodeNames = generate_node_names(NLeafs, ListOfNodes),
    Port = 4042,
    lists:foldl(fun(Node, Counter) ->
                    Id = integer_to_list(Counter),
                    NodeName=list_to_atom("leafs"++Id++"@"++Node),
                    ping_all(NodeName, NodeNames),
	                {ok, _} = rpc:call(NodeName, saturn_leaf_sup, start_leaf, [Port, Counter-1]),
                    %io:format("Node: ~p, id: ~p", [NodeName, Counter-1]),
                    Counter+1
                end, 1, ListOfNodes),
    lists:foldl(fun(Node, Counter) ->
                    Id = integer_to_list(Counter),
                    NodeName=list_to_atom("leafs"++Id++"@"++Node),
                    ok=rpc:call(NodeName, saturn_leaf_receiver, assign_convergers, [Counter-1, NLeafs]),
                    Counter+1
                end, 1, ListOfNodes).
    
test1([_Node, _Type, _Id, NLeafs | Nodes]) ->
    NodeNames = generate_node_names(list_to_integer(NLeafs), Nodes),
    io:format("nodelist ~p~n", [NodeNames]).

ping_all(_Node, []) ->
    ok;
	
ping_all(Node, [H|Rest]) ->
    case rpc:call(Node, net_adm, ping, [H]) of
        pong ->
            ping_all(Node, Rest);
        pang ->
            io:format("Error pang from ~p to ~p", [Node, H]),
            error
    end.

generate_node_names(NLeafs, Nodes) ->
    {_ , FinalList} = lists:foldl(fun(Node, {Counter, Acc}) ->
                                    case (Counter > NLeafs) of
                                        true ->
                                            Id = integer_to_list(Counter - NLeafs),
                                            {Counter + 1, Acc ++ [list_to_atom("internals"++Id++"@"++Node)]};
                                        false ->
                                            Id = integer_to_list(Counter),
                                            {Counter + 1, Acc ++ [list_to_atom("leafs"++Id++"@"++Node)]}
                                    end
                                  end, {1, []}, Nodes),
    FinalList.

generate_node_names_receivers(NLeafs, Nodes) ->
    {_ , FinalList} = lists:foldl(fun(Node, {Counter, Acc}) ->
                                    case (Counter > 2*NLeafs) of
                                        true ->
                                            Id = integer_to_list(Counter - 2*NLeafs),
                                            {Counter + 1, Acc ++ [list_to_atom("internals"++Id++"@"++Node)]};
                                        false ->
                                            case (Counter > NLeafs) of
                                                true ->
                                                    Id = integer_to_list(Counter - NLeafs),
                                                    {Counter + 1, Acc ++ [list_to_atom("receivers"++Id++"@"++Node)]};
                                                false ->
                                                    Id = integer_to_list(Counter),
                                                    {Counter + 1, Acc ++ [list_to_atom("leafs"++Id++"@"++Node)]}
                                            end
                                    end
                                  end, {1, []}, Nodes),
    FinalList.


                                    
