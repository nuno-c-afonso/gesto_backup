%% -------------------------------------------------------------------
%%
%% Copyright (c) 2015-2018 INESC-ID, Instituto Superior Tecnico,
%%                         Universidade de Lisboa, Portugal
%% Copyright (c) 2015-2016 Universite Catholique de Louvain, Belgium
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%  
%% -------------------------------------------------------------------
-module(saturn_leaf_converger).
-behaviour(gen_server).

-include("saturn_leaf.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).
-export([handle/2,
         clean_state/1,
         migrate/4]).

-record(state, {labels_queue :: queue(),
                myid,
                last_received,
                remote_replicated :: dict(),
                pending_clients}).
               
reg_name(MyId) ->  list_to_atom(integer_to_list(MyId) ++ atom_to_list(?MODULE)). 

start_link(MyId) ->
    gen_server:start({global, reg_name(MyId)}, ?MODULE, [MyId], []).

handle(MyId, Message) ->
    %lager:info("Message received: ~p", [Message]),
    gen_server:cast({global, reg_name(MyId)}, Message).

migrate(MyId, LocalLabel, RemoteLabel, Client) ->
    gen_server:cast({global, reg_name(MyId)}, {migrate, LocalLabel, RemoteLabel, Client}).

clean_state(MyId) ->
    gen_server:call({global, reg_name(MyId)}, clean_state, infinity).

init([MyId]) ->
    Name = list_to_atom(integer_to_list(MyId) ++ "converger_queue"),
    {ok, #state{labels_queue=ets_queue:new(Name),
                myid=MyId,
                last_received={empty, empty, 0},
                remote_replicated=dict:new(),
                pending_clients=[]}}.

handle_call(clean_state, _From, S0=#state{labels_queue=Labels}) ->
    {reply, ok, S0#state{labels_queue=ets_queue:clean(Labels)}}.

%% This will be called when saturn_proxy_vnode finishes applying the update
handle_cast(completed, S0=#state{labels_queue=Labels0, remote_replicated=RemoteReplicated0}) ->
    {{value, {_, {Label, SerializerClock}}}, Labels1} = ets_queue:out(Labels0),
    Sender = Label#label.sender,
    Clock = Label#label.timestamp,
    {Bucket, _} = Label#label.bkey,
    S1 = recheck_pending_migrations(S0#state{last_received={Sender, Bucket, SerializerClock}, remote_replicated=dict:store(Sender, Clock, RemoteReplicated0)}),
    {Labels2, S2} = handle_label(ets_queue:peek(Labels1), Labels1, S1),
    {noreply, S2#state{labels_queue=Labels2}};

%% Changed for getting the time that takes to apply the label when it reaches its destination
handle_cast({new_stream, Stream, _SenderId}, S0=#state{labels_queue=Labels0}) ->
    % lager:info("New stream received. Label: ~p", Stream),
    
    %% Arrival TS
    Now = saturn_utilities:now_microsec(),
    
    Empty = ets_queue:is_empty(Labels0),
    Labels1 = lists:foldl(fun(Label, Queue) ->
                            ets_queue:in({Now, Label}, Queue)
                          end, Labels0, Stream),
    case Empty of
        true ->
            {Labels2, S1} =  handle_label(ets_queue:peek(Labels1), Labels1, S0),
            {noreply, S1#state{labels_queue=Labels2}};
        false ->
            {noreply, S0#state{labels_queue=Labels1}}
    end;

%% Contacted by the client_receiver
handle_cast({migrate, LocalLabel, RemoteLabel, Client}, S0=#state{pending_clients=PendingClients, last_received=SerializerClock}) ->
    S1 = case can_complete(LocalLabel, RemoteLabel, S0) of
        true ->
            gen_server:reply(Client, {ok, SerializerClock}),
            S0;
        false ->
            S0#state{pending_clients=PendingClients ++ [{LocalLabel, RemoteLabel, Client}]}
    end,
    {noreply, S1};

handle_cast(_Info, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_label(empty, Queue, S) ->
    {Queue, S};

handle_label({value, {_, {Label, SerializerClock}}}, Queue, S0=#state{remote_replicated=RemoteReplicated0}) ->
    %% Debugging
    % Now = saturn_utilities:now_microsec(),
    % Diff = Now - ATS,
    % lager:info("\nTime to apply remote on destination: ~p", [Diff]),

    case Label#label.operation of
        remote_read ->
            Clock = Label#label.timestamp,
            Sender = Label#label.sender,
            {_, Queue1} = ets_queue:out(Queue),

            %% Update the state if the timestamp is higher
            RemoteReplicated1 = case dict:is_key(Sender, RemoteReplicated0) of
                true ->
                    ClockReplicated = dict:fetch(Sender, RemoteReplicated0),
                    case Clock > ClockReplicated of
                        true ->
                            dict:store(Sender, Clock, RemoteReplicated0);
                        false ->
                            RemoteReplicated0
                    end;
                false ->
                    dict:store(Sender, Clock, RemoteReplicated0)
            end,
            S1 = recheck_pending_migrations(S0#state{last_received={Sender, migration, SerializerClock}, remote_replicated=RemoteReplicated1}),
            handle_label(ets_queue:peek(Queue1), Queue1, S1);
        update ->
            BKey = Label#label.bkey,
            Clock = Label#label.timestamp,
            Node = Label#label.node,
            Sender = Label#label.sender,
            DocIdx = riak_core_util:chash_key(BKey),
            PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, ?PROXY_SERVICE),
            [{IndexNode, _Type}] = PrefList,
            saturn_proxy_vnode:propagate(IndexNode, Clock, Node, Sender, SerializerClock),
            
            %% Only update the state when the operation completes
            {Queue, S0}
    end.

can_complete(LocalLabel, RemoteLabel, #state{myid=MyId, last_received={_, _, SerializerClock}, remote_replicated=RemoteReplicated}) ->
    %% Check local label
    TestRemote = case LocalLabel of
        empty ->
            true;
        {SenderLocal, Timestamp} ->
            case dict:is_key(SenderLocal, RemoteReplicated) of
                true ->
                    ReceivedTimestamp = dict:fetch(SenderLocal, RemoteReplicated),
                    ReceivedTimestamp >= Timestamp;
                false ->
                    false
            end
    end,

    %% Check remote label
    case TestRemote of
        true ->
            case RemoteLabel of
                empty ->
                    true;
                {SenderRemote, SerializerClockRemote} ->
                    (SenderRemote == MyId) or (SerializerClock >= SerializerClockRemote)
            end;
        false ->
            false
    end.

recheck_pending_migrations(S0=#state{pending_clients=PendingClients0, last_received=SerializerClock}) ->
    PendingClients1 = lists:foldl(fun(Pend={LocalLabel, RemoteLabel, Client}, List) ->
                        case can_complete(LocalLabel, RemoteLabel, S0) of
                            true ->
                                gen_server:reply(Client, {ok, SerializerClock}),
                                List;
                            false ->
                                List ++ [Pend]
                        end
                    end, [], PendingClients0),
    S0#state{pending_clients=PendingClients1}.

-ifdef(TEST).

-endif.
