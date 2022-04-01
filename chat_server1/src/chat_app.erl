%% @author attack
%% @doc @todo Add description to chat_app.


-module(chat_app).

-behaviour(application).

-export([start/2, stop/1]).

start(normal, []) ->
	chat_sup:start_link();
start({takeover, _OtherNode}, []) ->
	chat_sup:start_link().

stop(_State) ->
	ok.
