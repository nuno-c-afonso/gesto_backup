%% -------------------------------------------------------------------
%%
%% Copyright (c) 2015-2016 INESC-ID, Instituto Superior Tecnico,
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
-module(pending_operations).

-include("saturn_leaf.hrl").

-export([insert/5,
         get/2,
         remove/2,
         connect/1,
         clean/2]).

insert(ETS, Client, Current, Expected, Op) ->
    true =  ets:insert(ETS, {Client, { {Current, Expected}, Op }}),
    ETS.

get(ETS, Client) ->
    case ets:lookup(ETS, Client) of
        [] ->
            {ok, empty};
        [{Client, Value}] ->
            {ok, Value}
    end.

remove(ETS, Client) ->
    ets:delete(ETS, Client),
    ETS.

connect([Partition]) ->
    Name = integer_to_list(Partition) ++ "pending_ops",
    ets:new(list_to_atom(Name), [set, named_table, private]).

clean(ETS, Partition) ->
    true = ets:delete(ETS),
    connect([Partition]),
    ETS.
