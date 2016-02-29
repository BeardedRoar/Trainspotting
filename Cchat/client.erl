-module(client).
-export([handle/2, initial_state/2]).
-include_lib("./defs.hrl").
-incluse(stdlib).

%% inititial_state/2 and handle/2 are used togetger with the genserver module,
%% explained in the lecture about Generic server.

%% Produce initial state
initial_state(Nick, GUIName) ->
    #client_st { gui = GUIName, nick = Nick, server = "", channels = []}.

%% ---------------------------------------------------------------------------

%% handle/2 handles each kind of request from GUI

%% All requests are processed by handle/2 receiving the request data (and the
%% current state), performing the needed actions, and returning a tuple
%% {reply, Reply, NewState}, where Reply is the reply to be sent to the
%% requesting process and NewState is the new state of the client.

%% Connect to server
handle(St, {connect, Server}) ->
	case St#client_st.server of
		 "" ->
			Data = {connect, self(), St#client_st.nick},
			io:fwrite("Client is sending: ~p~n", [Data]),
			ServerAtom = list_to_atom(Server),
			case catch genserver:request(ServerAtom, Data) of
				{'EXIT', Reason} ->
					Result = {error, server_not_reached, "Could not reach server"},
					NewSt = St;
				ok ->
					Result = ok,
					NewSt = St#client_st{server = ServerAtom};
				{error, user_already_connected} ->
					Result = {error, user_already_connected, "Trying to use taken nick"},
					NewSt = St
			end;
		_Else ->
			Result = {error, user_already_connected, "User already connected"},
			NewSt = St
	end,
	{reply, Result, NewSt};

%% Disconnect from server
handle(St, disconnect) ->
	case St#client_st.server of
		 "" -> 
			Result = {error, user_not_connected, "User not connected"},
			NewSt = St;
		_Else ->
			if
				0 < length(St#client_st.channels) ->
					Result = {error, leave_channels_first, "Leave all channels before disconnecting"},
					NewSt = St;
				0 == length(St#client_st.channels) ->
					Data = {disconnect, self(), St#client_st.nick},
					genserver:request(St#client_st.server, Data),
					Result = ok,
					NewSt = St#client_st{server = ""}
			end
	end,
	{reply, Result, NewSt};

% Join channel
handle(St, {join, Channel}) ->
	Joined = lists:member(Channel, St#client_st.channels),
	if
		Joined ->
			Response = {error, user_already_joined, "User has already joined this channel!"},
			NewSt = St;
		true ->
			Data = {join, St#client_st.nick, self(), Channel},
			Response = genserver:request(St#client_st.server, Data),
			NewSt = St#client_st{channels = [Channel|St#client_st.channels]}
	end,
    {reply, Response, NewSt} ;
    % {reply, {error, not_implemented, "Not implemented"}, St} ;

%% Leave channel
handle(St, {leave, Channel}) ->
	Joined = lists:member(Channel, St#client_st.channels),
	if
		Joined ->
		Data = {leave, St#client_st.nick, self(), Channel},
			Response = genserver:request(St#client_st.server, Data),
			NewSt = St#client_st{channels = St#client_st.channels -- [Channel]};
		true ->
			Response = {error, user_not_joined, "You cannot leave a channel you have not joined"},
			NewSt = St
	end,
    {reply, Response, NewSt} ;
    % {reply, {error, not_implemented, "Not implemented"}, St} ;

% Sending messages
handle(St, {msg_from_GUI, Channel, Msg}) ->
	InChannel = lists:member(Channel, St#client_st.channels),
	if 
		InChannel ->
			Response = genserver:request(St#client_st.server, {msg_from_GUI, Channel, St#client_st.nick, Msg});
		true ->
			Response = {error, user_not_joined, "You must join the channel to be able to send it messages"}
	end,
    {reply, Response, St} ;

%% Get current nick
handle(St, whoami) ->
    {reply, St#client_st.nick, St} ;

%% Change nick
handle(St, {nick, Nick}) ->
	case St#client_st.server of
		"" ->
			Result = ok,
			NewSt = St#client_st{nick = Nick};
		_Else ->
			Result = {error, user_already_connected, "You cannot change nick while connected to a server"},
			NewSt = St
	end,
    {reply, Result, NewSt} ;

%% Incoming message
handle(St = #client_st { gui = GUIName }, {incoming_msg, Channel, Name, Msg}) ->
    gen_server:call(list_to_atom(GUIName), {msg_to_GUI, Channel, Name++"> "++Msg}),
    {reply, ok, St}.