-module(channel).
-export([handle/2, initial_state/1]).
-include_lib("./defs.hrl").
-incluse(stdlib).

%% Produce initial state
initial_state(Name) ->
    #channel_st {name = Name, clients = []}.

%% ---------------------------------------------------------------------------
%%Called when a client wishes to join the channel.
handle(St, {join, _Nick, _ClientId}) ->
	{reply, ok, St#channel_st{clients = [{_Nick, _ClientId}|St#channel_st.clients]}};

%%Called when a client wishes to leave the Channel.
handle(St, {leave, _Nick, _ClientId}) ->
	NewSt = St#channel_st{clients = St#channel_st.clients -- [{_Nick, _ClientId}]},
	{reply, ok, NewSt};
	
%%Called whenever a message is sent in the Channel. Sends the message to all other clients who have joined
%%this channel. 	
handle(St, {msg_from_GUI, Channel, _Nick, _Msg}) ->
	Receivers = lists:keydelete(_Nick, 1, St#channel_st.clients ),
	%%sends the message to everyone in the channel except for the sender.
	lists:foreach(fun(N) ->
		F = fun() -> genserver:request(element(2,N),{incoming_msg, Channel, _Nick, _Msg})
					end,
					%%Spawn a new process for every message to be sent.
					spawn(F)
				end, Receivers),
	{reply, ok, St}.
	
