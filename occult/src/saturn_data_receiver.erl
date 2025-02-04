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
-module(saturn_data_receiver).
-behaviour(gen_server).

-include("saturn_leaf.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).
-export([data/4]).

               
start_link() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

data(Node, Label, BKey, Value) ->
    gen_server:cast(Node, {data, Label, BKey, Value}).

init([]) ->
    {ok, nostate}.

handle_call(Info, From, State) ->
    lager:error("Unhandled message ~p, from ~p", [Info, From]),
    {reply, ok, State}.

handle_cast({data, Label, BKey={Bucket, _}, Value}, S0) ->
    DocIdx = riak_core_util:chash_key({Bucket, none}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, ?PROXY_SERVICE),
    [{IndexNode, _Type}] = PrefList,
    saturn_proxy_vnode:data(IndexNode, Label, BKey, Value),
    {noreply, S0};

handle_cast(_Info, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-ifdef(TEST).

-endif.
