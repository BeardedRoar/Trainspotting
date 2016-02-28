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
    Data = {connect, self(), St#client_st.nick},
    io:fwrite("Client is sending: ~p~n", [Data]),
    ServerAtom = list_to_atom(Server),
    Response = genserver:request(ServerAtom, Data),
    io:fwrite("Client received: ~p~n", [Response]),
	%% Response = genserver:request(serverAtom, [{connect, self(), St#client_st.nick}]),
    {reply, ok, St#client_st{server = ServerAtom}} ;
    % {reply, {error, not_implemented, "Not implemented"}, St} ;

%% Disconnect from server
handle(St, disconnect) ->
	% Probably needs more code, but a beginning.
	
	{reply, ok, St#client_st{server = ""}} ;
    % {reply, {error, not_implemented, "Not implemented"}, St} ;

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
