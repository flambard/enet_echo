-module(enet_echo_peer).
-behaviour(gen_server).

-include_lib("enet/include/enet.hrl").

%% API
-export([ start_link/1 ]).

%% gen_server callbacks
-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2
        ]).

-record(state,
        {
         ip,
         port,
         peer,
         channels
        }).


%%%===================================================================
%%% API
%%%===================================================================

start_link(PeerInfo) ->
    gen_server:start_link(?MODULE, PeerInfo, []).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(PeerInfo) ->
    #{ ip := IP,
       port := Port,
       peer := Peer,
       channels := Channels
     } = PeerInfo,
    process_flag(trap_exit, true),
    link(Peer),
    State = #state{ ip = IP,
                    port = Port,
                    peer = Peer,
                    channels = Channels },
    {ok, State}.


handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


handle_cast(_Request, State) ->
    {noreply, State}.


handle_info({enet, Channel, #unsequenced{ data = Data }}, S) ->
    %%
    %% Received unsequenced data - echo it
    %%
    io:format("--> ~p\n", [Data]),
    #state{ channels = #{ Channel := ChannelPid }} = S,
    enet:send_unsequenced(ChannelPid, Data),
    io:format("<-- ~p\n", [Data]),
    {noreply, S};
handle_info({enet, Channel, #unreliable{ data = Data }}, S) ->
    %%
    %% Received unreliable data - echo it
    %%
    io:format("--> ~p\n", [Data]),
    #state{ channels = #{ Channel := ChannelPid }} = S,
    enet:send_unreliable(ChannelPid, Data),
    io:format("<-- ~p\n", [Data]),
    {noreply, S};
handle_info({enet, Channel, #reliable{ data = Data }}, S) ->
    %%
    %% Received reliable data - echo it
    %%
    io:format("--> ~p\n", [Data]),
    #state{ channels = #{ Channel := ChannelPid }} = S,
    enet:send_reliable(ChannelPid, Data),
    io:format("<-- ~p\n", [Data]),
    {noreply, S};
handle_info(_Info, S) ->
    {noreply, S}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
