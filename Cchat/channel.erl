-module(client).
-export([handle/2, initial_state/2]).
-include_lib("./defs.hrl").
-incluse(stdlib).



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
		F = fun() -> genserver:request(element(2,N),{incoming_msg, _Channel, _Nick, _Msg})
					end,
					%%Spawn a new process for every message to be sent.
					spawn(F)
				end, Receivers),
	{reply, ok, St};
	
%% Produce initial state
initial_state(Name, ) ->
	% Not being connected to a server is represented as the server-part of the state being null
    #channel_st { name = GUIName, nick = Nick, server = null, channels = []}.

%% ---------------------------------------------------------------------------