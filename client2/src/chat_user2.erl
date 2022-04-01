%% @author attack
%% @doc @todo Add description to chat_user2.

-module(chat_user2).
-behaviour(gen_server).

-export([start_link/0, connect_server/1, chat_register/0, send_msg/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SV_NODE, 'sv@192.168.238.128').

-record(users, {name, node}).

start_link() ->
    Return = gen_server:start_link({local, ?MODULE}, ?MODULE, {}, []),
    % connect_server(?SV_NODE),
    io:format("start_link: ~p~n", [Return]),
    Return.

init({}) ->
    State = [],
    Return = {ok, State},
    io:format("init: ~p~n", [State]),
    chat_register(),
    Return.

% ===================================================================
connect_server(Node) ->
    Status = net_adm:ping(Node),
    io:format("Status: ~p~n", [Status]),
    case Status of
        pong -> {ok, connected};
        pang -> gen_server:cast(?MODULE, {noconnect, Node})
    end.

chat_register() ->
    io:format("Connecting...~n"),
    Return = {error, no_proc},
    case global:whereis_name(chat_server) of
        undefined ->    Return;
        PID ->          try
                            gen_server:call(PID, {connect, #users{name = ?MODULE, node = node()}}),
                            {ok, connected}
                        catch _:_ -> Return
                        end            
    end.

send_msg(ToClient, Msg) ->
    case global:whereis_name(chat_server) of
        undefined ->    io:format("No connection to the server");
        PID ->  try
                    gen_server:call(PID, {send_msg, #users{name = ToClient}, ?MODULE, Msg}),
                    {ok, connected}
                catch _:_ -> io:format("No connection to the server")
                end       
    end.

% ===================================================================
handle_call({message, From, Msg}, _From, State) ->
    io:format("~p: ~p~n", [From, Msg]),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    Return = {reply, Reply, State},
    io:format("handle_call: ~p~n", [Return]),
    Return.

handle_cast({noconnect, Node}, State) ->
    Return = {noreply, State},
    io:format("handle_cast: Cannot connect to ~p~n", [Node]),
    Return;

handle_cast({send_msg, Msg}, State) ->
    Return = {noreply, State},
    io:format("Message: ~p~n", [Msg]),
    Return;

handle_cast(_Msg, State) ->
    Return = {noreply, State},
    io:format("handle_cast: ~p~n", [Return]),
    Return.

handle_info(_Info, State) ->
    Return = {noreply, State},
    io:format("handle_info: ~p~n", [Return]),
    Return.

terminate(_Reason, _State) ->
    Return = ok,
    io:format("terminate: ~p~n", [Return]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    Return = {ok, State},
    io:format("code_change: ~p~n", [Return]),
    Return.

