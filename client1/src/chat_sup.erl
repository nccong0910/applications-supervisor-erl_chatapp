%% @author attack
%% @doc @todo Add description to chat_sup.


-module(chat_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).
-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================
start_link() ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================
init([]) ->
	AChild = {chat_user1,{chat_user1, start_link, []},
	permanent, 2000, worker, [chat_user1]},
	{ok,{{one_for_all,1,1}, [AChild]}}.


