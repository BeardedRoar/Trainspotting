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
			Result = {error, user_already_connected}
	end,
	{reply, Result, X};
	
%%Called when a client wishes to disconnect from the server. 
handle(St, {disconnect, _ClientId, _Nick}) ->
	X = St#server_st{clients = St#server_st.clients -- [{_Nick, _ClientId}]},
	{reply, ok, X};

%%Called when a client wishes to join a Channel on the server. Creates a new Channel with the 
%%specified name if one does not exist and then adds the client to it. 
handle(St, {join, _Nick, _ClientId, _Channel}) ->
	Channel = lists:keyfind(_Channel, #channel_st.name, St#server_st.channels),
	case Channel of
		%%channel does not exist.
		false ->
			NewChannel = #channel_st{name = _Channel, clients = [{_Nick, _ClientId}]},
			X = St#server_st{channels = [NewChannel|St#server_st.channels]};
		%%channel already exists.
		_else ->
			NewChannel = Channel#channel_st{clients = [{_Nick, _ClientId}|Channel#channel_st.clients]},
			NewChannelList = lists:keyreplace(_Channel, #channel_st.name, St#server_st.channels, NewChannel),
			X = St#server_st{channels = NewChannelList}
	end,
	{reply, ok, X};
		
%%Called when a client wishes to leave a Channel.
handle(St, {leave, _Nick, _ClientId, _Channel}) ->
	Channel = lists:keyfind(_Channel, #channel_st.name, St#server_st.channels),
	NewChannel = Channel#channel_st{clients = lists:keydelete(_Nick, 1, Channel#channel_st.clients )},
	NewChannelList = lists:keyreplace(_Channel, #channel_st.name, St#server_st.channels, NewChannel),
	X = St#server_st{channels = NewChannelList},
	{reply, ok, X};
	
%%Called whenever a message is sent from a Channel. Sends the message to all other clients who have joined
%%that channel. 	
handle(St, {msg_from_GUI, _Channel, _Nick, _Msg}) ->
	Channel = lists:keyfind(_Channel, #channel_st.name, St#server_st.channels),
	Receivers = lists:keydelete(_Nick, 1, Channel#channel_st.clients ),
	%%sends the message to everyone in the channel except for the sender.
	lists:foreach(fun(N) ->
		genserver:request(element(2,N),{incoming_msg, _Channel, _Nick, _Msg}),
							io:fwrite("N is: ~p~n", [element(2,N)])
				end, Receivers),
	{reply, ok, St};
	
&&Will always match, should never actually be called during execution of program. 
handle(St, Request) ->
    io:fwrite("Server received: ~p~n", [Request]),
    Response = "hi!",
    io:fwrite("Server is sending: ~p~n", [Response]),
    {reply, Response, St}.