% Top level module
-module(cchat).
-export([server/0,client/0,start/0,start2/0,send_job/3,send/1]).
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
	case catch genserver:request(ServerAtom, {get_users}, infinity) of
		{'EXIT', _Reason} ->
			{error, server_not_reached, "Could not reach server"};
		Response ->
			Tasks = assign_tasks(Response, Input, Function),
			SPID = self(),
			F = fun() -> 
				SPID ! {work_done, rpc:pmap({cchat, send}, [], Tasks)}
			end,
			spawn(F),
			receive
				{work_done, Result} ->
					Result
			end
	end.
	
send({{_,User}, Job, Function}) ->
	genserver:request(User,{work, Function, Job}, infinity).

%% Merges two lists into one where the all elements in the second list are distributed among the elements of the first.	
assign_tasks(Users, Tasks, Function) ->
	[  {lists:nth(((N-1) rem length(Users)) + 1, Users), Task, Function}
	|| {N,Task} <- lists:zip(lists:seq(1,length(Tasks)), Tasks) ].
	