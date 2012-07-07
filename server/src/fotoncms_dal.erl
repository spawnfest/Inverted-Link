%% Copyright 2012 Alexander Tchitchigin
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%       http://www.apache.org/licenses/LICENSE-2.0
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and limitations under the License.

-module(fotoncms_dal).
-author('Alexander Tchitchigin <at@fosslabs.ru>').

-include_lib("mongodb/include/mongo_protocol.hrl").

-export([connect/0, disconnect/1, get_feed/3]).


-define(MONGODB_HOST, {localhost, 27017}).
-define(DBNAME, fotoncms).


connect() ->
    {ok, Connection} = mongo:connect(?MONGODB_HOST),
    Connection.

disconnect(Connection) ->
    mongo:disconnect(Connection).

get_feed(Connection, Account, Feed) ->
    mongo:do(safe, master, Connection, ?DBNAME,
	     fun() ->
		     Posts = mongo:rest( mongo:find(list_to_atom(Account),
						    {feed, list_to_binary(Feed)},
						    {}, 0, 10) ),
		     Posts
	     end).
