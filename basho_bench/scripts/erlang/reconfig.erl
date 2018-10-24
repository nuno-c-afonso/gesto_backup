-module(reconfig).
-export([dummy/1,
         leave/1,
         join/1,
         fail/1,
         subscribe_buckets/1,
         subscribe_objects/1]).

dummy(LstNode) ->
    [Node] = LstNode,
    io:format("Send request to: ~p", [Node]).

%% With RPC - works with saturn_leaf, but not saturn_client_receiver
% Data = rpc:call(Node, saturn_leaf, collect_stats, [5, updates]),
% io:format("Result: ~p", [Data]).

leave([Args]) ->
    %% Split the arguments
    [NodeStr, CloudletIdStr] = string:tokens(Args, " "),

    %% Make the request
    Node = list_to_atom(NodeStr),
    {CloudletId, _} = string:to_integer(CloudletIdStr),
    Data = gen_server:call(server_name(Node), {leave, CloudletId}, infinity),
    io:format("Result: ~p", [Data]).

join([Args]) ->
    %% Split the arguments
    [NodeStr, CloudletIdStr] = string:tokens(Args, " "),

    %% Make the request
    Node = list_to_atom(NodeStr),
    {CloudletId, _} = string:to_integer(CloudletIdStr),
    Data = gen_server:call(server_name(Node), {join, CloudletId}, infinity),
    io:format("Result: ~p", [Data]).

fail([NodeStr]) ->
    Node = list_to_atom(NodeStr),
    Data = gen_server:call(server_name(Node), fail, infinity),
    io:format("Result: ~p", [Data]).

subscribe_buckets([Args]) ->
    %% Split the arguments
    [NodeStr, CloudletIdStr | BucketsListStr] = string:tokens(Args, " "),

    %% Make the request
    Node = list_to_atom(NodeStr),
    {CloudletId, _} = string:to_integer(CloudletIdStr),
    Buckets = int_list_from_strs(BucketsListStr),
    Data = gen_server:call(server_name(Node), {subscribe_buckets, CloudletId, Buckets}, infinity),
    io:format("Result: ~p", [Data]).

subscribe_objects([Args]) ->
    %% Split the arguments
    [NodeStr, CloudletIdStr | ObjectsListStr] = string:tokens(Args, " "),

    %% Make the request
    Node = list_to_atom(NodeStr),
    {CloudletId, _} = string:to_integer(CloudletIdStr),
    Objects = bkeys_from_list(ObjectsListStr),
    Data = gen_server:call(server_name(Node), {subscribe_objects, CloudletId, Objects}, infinity),
    io:format("Result: ~p", [Data]).

%%%%%%%%%%%%%%%%%%%%%%%%
%% INTERNAL FUNCTIONS %%
%%%%%%%%%%%%%%%%%%%%%%%%

server_name(Node)->
    {saturn_client_receiver, Node}.

%% For buckets
int_list_from_strs(Lst) ->
    int_list_from_strs(Lst, []).

int_list_from_strs([], Res) ->
    Res;

int_list_from_strs([ Head | Rem ], Res) ->
    {Int, _} = string:to_integer(Head),
    int_list_from_strs(Rem, Res ++ [Int]).

%% For objects
bkeys_from_list(Lst) ->
    bkeys_from_list(Lst, dict:new()).

bkeys_from_list([], Res) ->
    Res;

bkeys_from_list([ Head | Rem ], Res) ->
    {Bucket, KeyStr} = string:to_integer(Head),
    KeyTrimmed = hd(string:tokens(KeyStr, ",")),
    {Key, _} = string:to_integer(KeyTrimmed),

    case dict:is_key(Bucket, Res) of
        true ->
            TempList = dict:fetch(Bucket, Res),

            %% Process the wildcard: -1
            case Key of
                -1 ->                
                    bkeys_from_list(Rem, dict:store(Bucket, [Key] ++ TempList, Res));
                _ ->
                    bkeys_from_list(Rem, dict:store(Bucket, TempList ++ [Key], Res))
            end;

        %% Just add the key
        false ->
            bkeys_from_list(Rem, dict:store(Bucket, [Key], Res))
    end.
