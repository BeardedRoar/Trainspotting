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
	Connected = lists:keyfind(_Nick, 1, St#server_st.clients ),
	case Connected of 
		false ->
			X = St#server_st{clients = [{_Nick, _ClientId}|St#server_st.clients]},
			Result = ok;
		_else ->
			X = St,
			Result = {error, nick_taken}
	end,
	{reply, Result, X};
	
%%Called when a client wishes to disconnect from the server. 
handle(St, {disconnect, _ClientId, _Nick}) ->
	X = St#server_st{clients = St#server_st.clients -- [{_Nick, _ClientId}]},
	{reply, ok, X};

%%Called when a client wishes to join a Channel on the server. Creates a new Channel with the 
%%specified name if one does not exist and then adds the client to it. 
handle(St, {join, _Nick, _ClientId, _Channel}) ->
	Exists = lists:member(_Channel, St#server_st.channels),
	case Exists of
		%%channel does not exist.
		false ->
			genserver:start(_Channel, channel:initial_state(_Channel), fun channel:handle/2),
			X = St#server_st{channels = [_Channel|St#server_st.channels]};
		_else ->
			X = St
	end,
	genserver:request(_Channel, {join, _Nick, _ClientId}),
	{reply, ok, X};
	
handle(St, {job, Function, Input}) ->
	Jobs = assign_tasks(St#server_st.clients, Input),
	lists:foreach(fun(N) ->
		F = fun() -> genserver:request(element(2, element(1,N)),{work, Function, element(2,N)})
					end,
					%%Spawn a new process for every message to be sent.
					spawn(F)
				end, Jobs),
	{reply, ok, St};
	
%%Will always match, should never actually be called during execution of program. 
handle(St, Request) ->
    io:fwrite("Server received: ~p~n", [Request]),
    Response = "hi!",
    io:fwrite("Server is sending: ~p~n", [Response]),
    {reply, Response, St}.
	
	
assign_tasks(Users, Tasks) ->
	[  {lists:nth(((N-1) rem length(Users)) + 1, Users), Task}
	|| {N,Task} <- lists:zip(lists:seq(1,length(Tasks)), Tasks) ].
  
	
