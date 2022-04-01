%% @author attack
%% @doc @todo Add description to chat_app.


-module(chat_app).

-behaviour(application).

-export([start/2, stop/1]).

-define(USER_TABLE, users).

-record(users, {name, node}).

start(normal, []) ->
	application:set_env(mnesia, dir, "db"),
	mnesia:delete_schema([node()]),
	mnesia:create_schema([node()]),
	application:start(mnesia),
	mnesia:create_table(?USER_TABLE,
						[{attributes, record_info(fields, users)},
						{disc_copies, [node()]}
						]),
	chat_sup:start_link();
start({takeover, _OtherNode}, []) ->
	application:set_env(mnesia, dir, "db"),
	mnesia:delete_schema([node()]),
	mnesia:create_schema([node()]),
	application:start(mnesia),
	mnesia:create_table(?USER_TABLE,
						[{attributes, record_info(fields, users)},
						{disc_copies, [node()]}
						]),
	chat_sup:start_link();
start({failover, _OtherNode}, []) ->
	application:stop(mnesia),
	application:set_env(mnesia, dir, "db"),
	mnesia:delete_schema([node()]),
	mnesia:create_schema([node()]),
	application:start(mnesia),
	mnesia:create_table(?USER_TABLE,
						[{attributes, record_info(fields, users)},
						{disc_copies, [node()]}
						]),
	chat_sup:start_link().

stop(_State) ->
	ok.
