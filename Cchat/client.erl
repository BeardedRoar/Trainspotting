-module(client).
-export([handle/2, initial_state/2]).
-include_lib("./defs.hrl").
-incluse(stdlib).

%% inititial_state/2 and handle/2 are used togetger with the genserver module,
%% explained in the lecture about Generic server.

%% Produce initial state
initial_state(Nick, GUIName) ->
	% Not being connected to a server is represented as the server-part of the state being null
    #client_st { gui = GUIName, nick = Nick, server = null, channels = []}.

%% ---------------------------------------------------------------------------

%% handle/2 handles each kind of request from GUI

%% All requests are processed by handle/2 receiving the request data (and the
%% current state), performing the needed actions, and returning a tuple
%% {reply, Reply, NewState}, where Reply is the reply to be sent to the
%% requesting process and NewState is the new state of the client.

%% Connect to server
handle(St, {connect, Server}) ->
	case St#client_st.server of
		% Only connect if client is not already connected to a server
		null ->
			Data = {connect, self(), St#client_st.nick},
			ServerAtom = list_to_atom(Server),
			case catch genserver:request(ServerAtom, Data) of
				% If something goes terribly wrong, mainly if the server can't be reach, handle it instead of crashing
				{'EXIT', _Reason} ->
					Result = {error, server_not_reached, "Could not reach server"},
					NewSt = St;
				% If server is reached and doesn't returns an error, add it to the state
				ok ->
					Result = ok,
					NewSt = St#client_st{server = ServerAtom};
				% Handle the possibility that your nick is already taken by somebody else on the server
				{error, nick_taken} ->
					Result = {error, nick_taken, "Trying to use taken nick"},
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
		% If the client's not connected no disconnect is needed.
		null -> 
			Result = {error, user_not_connected, "User not connected"},
			NewSt = St;
		_Else ->
			if
				% Make sure client has left all channels before leaving
				0 < length(St#client_st.channels) ->
					Result = {error, leave_channels_first, "Leave all channels before disconnecting"},
					NewSt = St;
				0 == length(St#client_st.channels) ->
					Data = {disconnect, self(), St#client_st.nick},
					genserver:request(St#client_st.server, Data),
					Result = ok,
					NewSt = St#client_st{server = null}
			end
	end,
	{reply, Result, NewSt};

% Join channel
handle(St, {join, Channel}) ->
	% Check if the client has already joined the channel, otherwise join it
	ChannelAtom = list_to_atom(Channel),
	Joined = lists:member(ChannelAtom, St#client_st.channels),
	if
		Joined ->
			Response = {error, user_already_joined, "User has already joined this channel!"},
			NewSt = St;
		true ->
			Data = {join, St#client_st.nick, self(), ChannelAtom},
			Response = genserver:request(St#client_st.server, Data),
			NewSt = St#client_st{channels = [ChannelAtom|St#client_st.channels]}
	end,
    {reply, Response, NewSt} ;

%% Leave channel
handle(St, {leave, Channel}) ->
	% Only leave the channel if already in it
	ChannelAtom = list_to_atom(Channel),
	Joined = lists:member(ChannelAtom, St#client_st.channels),
	if
		Joined ->
		Data = {leave, St#client_st.nick, self()},
			Response = genserver:request(ChannelAtom, Data),
			NewSt = St#client_st{channels = St#client_st.channels -- [ChannelAtom]};
		true ->
			Response = {error, user_not_joined, "You cannot leave a channel you have not joined"},
			NewSt = St
	end,
    {reply, Response, NewSt} ;

% Sending messages
handle(St, {msg_from_GUI, Channel, Msg}) ->
	% Make sure you're in the channel you tries to send messages to
	ChannelAtom = list_to_atom(Channel),
	InChannel = lists:member(ChannelAtom, St#client_st.channels),
	if 
		InChannel ->
			Response = genserver:request(ChannelAtom, {msg_from_GUI, Channel, St#client_st.nick, Msg});
		true ->
			Response = {error, user_not_joined, "You must join the channel to be able to send it messages"}
	end,
    {reply, Response, St} ;

%% Get current nick
handle(St, whoami) ->
    {reply, St#client_st.nick, St} ;

%% Change nick
handle(St, {nick, Nick}) ->
	% Only change the nick if not connected
	case St#client_st.server of
		null ->
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
    {reply, ok, St};
	
%% Do work
handle(St, {work, Function, Input}) ->
	{reply, ok, St}.

