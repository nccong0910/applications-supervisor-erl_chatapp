%% @author attack
%% @doc @todo Add description to chatapp.


-module(chatapp).

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
	RestartStrategy = one_for_one,
	MaxRestarts = 1000,
	MaxSecondsBetweenRestarts = 4000,
	SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
	Restart = permanent,
	Shutdown = 3000,
	Type = worker,
	AChild = {'chat_server', {'chat_server', start_link, []}, Restart, Shutdown, Type, ['chat_server']},
	{ok, {SupFlags, [AChild]}}.

