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
-module(simple_overlapping_ets_backend_connector).

-include("saturn_leaf.hrl").

-export([update/2,
         read/2,
         local_update/2,
         connect/1,
         clean/2]).

update(ETS, Payload)->
    {BKey, Value, TimeStamp} = Payload,
    true =  ets:insert(ETS, {BKey, {Value, TimeStamp}}),
    {ok, ETS}.

read(ETS, Payload)->
    {BKey} = Payload,
    case ets:lookup(ETS, BKey) of
        [] ->
            {ok, {empty, 0}};
        [{BKey, Value}] ->
            {ok, Value}
    end.

local_update(ETS0, Payload0)->
    {BKey, Value} = Payload0,

    %% Read current version
    {ok, {_, TimeStamp0}} = read(ETS0, {BKey}),

    %% Increment the version by one
    TimeStamp1 = TimeStamp0 + 1,
    Payload1 = {BKey, Value, TimeStamp1},
    {ok, ETS1} = update(ETS0, Payload1),

    {ok, {ETS1, TimeStamp1}}.

connect([Partition]) ->
    Name = integer_to_list(Partition) ++ "kv",
    ets:new(list_to_atom(Name), [set, named_table, private]).

clean(ETS, Partition) ->
    true = ets:delete(ETS),
    connect([Partition]),
    ETS.
