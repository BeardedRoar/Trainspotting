% Top level module
-module(cchat).
-export([server/0,client/0,start/0,start2/0,send_job/3]).
-include_lib("./defs.hrl").

%% Start a server
server() ->
    Server = "shire",
    genserver:start(list_to_atom(Server), server:initial_state(Server), fun server:handle/2).

%% Start a client GUI
client() ->
    gui:start().

%% Start local server and one client
start() ->
    server(),
    client().

%% Start local server and two clients
start2() ->
    server(),
    client(),
    client().

%% Sends a job consisting of a function and input to all clients connected to the server.
send_job(Server, Function, Input) ->
	ServerAtom = list_to_atom(Server),
	case catch genserver:request(ServerAtom, {get_tasks, Input}, infinity) of
		{'EXIT', _Reason} ->
			{error, server_not_reached, "Could not reach server"};
		Response ->
			SPID = self(),
			F = fun() -> 
				SPID ! {work_done, [genserver:request(X,{work, Function, Y}, infinity) || {{_,X},Y} <- Response]}
			end,
			spawn(F),
			receive
				{work_done, Result} ->
					Result
			end
	end.
		
