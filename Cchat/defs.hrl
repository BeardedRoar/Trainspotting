% This record defines the structure of the client process.
%
% It contains the following fields:
%   gui: the name (or Pid) of the GUI process.
%   nick: the name of the client.
%	server: the server the client is currently connected to.
%	channels: a list of the channels the client currently is connected to.
-record(client_st, {gui, nick, server, channels}).

% This record defines the structure of the server process.
%
% It contains the following fields:
%	name: the name of the server.
%	clients: a list of all the clients currently connected to the server.
%	channels: a list of all the channels created on the server.
-record(server_st, {name, clients, channels}).

%This record defines the structure of the channel object.
%
%It contains the following fields:
%	name: the name of the channel.
%	clients: a list of all the clients who have joined the channel.
-record(channel_st, {name, clients}).
