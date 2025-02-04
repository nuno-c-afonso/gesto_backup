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
-module(simple_backend_connector).

-include("saturn_leaf.hrl").

-export([update/2,
         read/2,
         propagation/2,
         connect/1
        ]).

update(Connector, Payload)->
    {BKey, Value, TimeStamp} = Payload,
    IndexNode = get_indexnode(BKey),
    ok = saturn_simple_backend_vnode:update(IndexNode, BKey, {Value, TimeStamp}),
    {ok, Connector}.

read(_Connector, Payload)->
    {BKey} = Payload,
    IndexNode = get_indexnode(BKey),
    saturn_simple_backend_vnode:read(IndexNode, BKey).

propagation(Connector, Payload)->
    {BKey, Value, TimeStamp} = Payload,
    IndexNode = get_indexnode(BKey),
    ok = saturn_simple_backend_vnode:propagation(IndexNode, BKey, {Value, TimeStamp}),
    {ok, Connector}.

get_indexnode(BKey) ->
    {Bucket, _} = BKey,
    DocIdx = riak_core_util:chash_key({Bucket, none}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, ?SIMPLE_SERVICE),
    [{IndexNode, _Type}] = PrefList,
    IndexNode.

connect(_) ->
    ok.
