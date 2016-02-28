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
			Response = genserver:request(ServerAtom, Data),
			io:fwrite("Client received: ~p~n", [Response]),
			Result = ok,
			NewSt = St#client_st{server = ServerAtom};
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
					%%Data = {disconnect, self(), St#client_st.nick},
					%%genserver:request(St#client_st.server, Data),
					Result = ok,
					NewSt = St#client_st{server = ""}
			end
	end,
	{reply, Result, NewSt};

% Join channel
handle(St, {join, Channel}) ->
	io:fwrite("~p~n", [St#client_st.channels]),
    {reply, ok, St#client_st{channels = [Channel|St#client_st.channels]}} ;
    % {reply, {error, not_implemented, "Not implemented"}, St} ;

%% Leave channel
handle(St, {leave, Channel}) ->
    {reply, ok, St#client_st{channels = St#client_st.channels -- [Channel]}} ;
    % {reply, {error, not_implemented, "Not implemented"}, St} ;

% Sending messages
handle(St, {msg_from_GUI, Channel, Msg}) ->
    % {reply, ok, St} ;
    {reply, {error, not_implemented, "Not implemented"}, St} ;

%% Get current nick
handle(St, whoami) ->
    {reply, St#client_st.nick, St} ;

%% Change nick
handle(St, {nick, Nick}) ->
    {reply, ok, St#client_st{nick = Nick}} ;

%% Incoming message
handle(St = #client_st { gui = GUIName }, {incoming_msg, Channel, Name, Msg}) ->
    gen_server:call(list_to_atom(GUIName), {msg_to_GUI, Channel, Name++"> "++Msg}),
    {reply, ok, St}.
