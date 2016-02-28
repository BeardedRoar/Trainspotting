-module(server).
-export([handle/2, initial_state/1]).
-include_lib("./defs.hrl").

%% inititial_state/2 and handle/2 are used togetger with the genserver module,
%% explained in the lecture about Generic server.

% Produce initial state
initial_state(ServerName) ->
    #server_st{name = ServerName, clients = [], channels = []}.

%% ---------------------------------------------------------------------------

%% handle/2 handles requests from clients

%% All requests are processed by handle/2 receiving the request data (and the
%% current state), performing the needed actions, and returning a tuple
%% {reply, Reply, NewState}, where Reply is the reply to be sent to the client
%% and NewState is the new state of the server.

handle(St, {connect, _ClientId, _Nick}) ->
	io:fwrite("~p~n", [_Nick]),
	%%NewDict = dict:store(_Nick, _ClientId),
	X = St#server_st{clients = [{_ClientId, _Nick}|St#server_st.clients]},
	{reply, ok, X};
	
handle(St, {join, Nick, Channel}) ->
	io:fwrite("server received: ~p~n", [Nick]),
	io:fwrite("server received: ~p~n", [Channel]),
	X = St#server_st{channels = [{Nick}|St#server_st.clients]},
	io:fwrite("~p~n", [St#server_st.channels]),
	{reply, ok, X};
handle(St, Request) ->
    io:fwrite("Server received: ~p~n", [Request]),
    Response = "hi!",
    io:fwrite("Server is sending: ~p~n", [Response]),
    {reply, Response, St}.