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
	Connected = lists:keyfind(_Nick, 2, St#server_st.clients ),
	case Connected of 
		false ->
			io:fwrite("~p~n", [St#server_st.clients]),
			X = St#server_st{clients = [{_ClientId, _Nick}|St#server_st.clients]},
			Result = ok;
		_else ->
			X = St,
			Result = {error, user_already_connected}
	end,
	{reply, Result, X};

handle(St, {disconnect, _ClientId, _Nick}) ->
	X = St#server_st{clients = St#server_st.clients -- [{_ClientId, _Nick}]},
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
	Channel = lists:keyfind(_Channel, #channel_st.name, St#server_st.channels),
	io:fwrite("after delete are: ~p~n", [lists:keydelete(_Nick, 1, Channel#channel_st.clients )]),
	Receivers = lists:keydelete(_Nick, 1, Channel#channel_st.clients ),
	io:fwrite("recievers are: ~p~n", [Receivers]),
	lists:foreach(fun(N) ->
		genserver:request(element(2,N),{incoming_msg, _Channel, _Nick, _Msg}),
							io:fwrite("N is: ~p~n", [element(2,N)])
				end, Receivers),
	%for each client in channel - client with nick Nick
	%genserver:request(incoming message)
	{reply, ok, St};
	
handle(St, Request) ->
    io:fwrite("Server received: ~p~n", [Request]),
    Response = "hi!",
    io:fwrite("Server is sending: ~p~n", [Response]),
    {reply, Response, St}.