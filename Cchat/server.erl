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

handle(St, {disconnect, _ClientId, _Nick}) ->
	X = St#server_st{clients = St#server_st.clients -- [{_ClientId, _Nick}]},
	{reply, ok, X};

handle(St, {join, Nick, Channel}) ->
	io:fwrite("server received: ~p~n", [Nick]),
	io:fwrite("server received: ~p~n", [Channel]),
	X = St#server_st{channels = [{Nick}|St#server_st.clients]},
	io:fwrite("~p~n", [St#server_st.channels]),
	{reply, ok, X};
	
handle(St, {join, _Nick, _ClientId, _Channel}) ->
	io:fwrite("channels in serverstate are: ~p~n", [St#server_st.channels]),
	io:fwrite("channel to match is: ~p~n", [_Channel]),
	Channel = lists:keyfind(_Channel, #channel_st.name, St#server_st.channels),
	case Channel of
		false ->
			NewChannel = #channel_st{name = _Channel, clients = [{_Nick, _ClientId}]},
			X = St#server_st{channels = [NewChannel|St#server_st.channels]};
		_else ->
			NewChannel = Channel#channel_st{clients = [{_Nick, _ClientId}|Channel#channel_st.clients]},
			NewChannelList = lists:keyreplace(_Channel, #channel_st.name, St#server_st.channels, NewChannel),
			X = St#server_st{channels = NewChannelList}
	end,
	io:fwrite("channels are: ~p~n", [X]),
	{reply, ok, X};
		
handle(St, {leave, _Nick, _ClientId, _Channel}) ->
	Channel = lists:keyfind(_Channel, #channel_st.name, St#server_st.channels),
	io:fwrite("Server received: ~p~n", [lists:keydelete(_Nick, 1, Channel#channel_st.clients )]),
	NewChannel = Channel#channel_st{clients = lists:keydelete(_Nick, 1, Channel#channel_st.clients )},
	NewChannelList = lists:keyreplace(_Channel, #channel_st.name, St#server_st.channels, NewChannel),
	X = St#server_st{channels = NewChannelList},
	io:fwrite("channels are: ~p~n", [NewChannelList]),
	{reply, ok, X};
handle(St, {msg_from_GUI, _Channel, _Nick, _Msg}) ->
	for each client in channel - client with nick Nick
	genserver:request(incoming message)
handle(St, Request) ->
    io:fwrite("Server received: ~p~n", [Request]),
    Response = "hi!",
    io:fwrite("Server is sending: ~p~n", [Response]),
    {reply, Response, St}.
