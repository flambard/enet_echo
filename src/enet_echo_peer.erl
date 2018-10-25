-module(enet_echo_peer).
-behaviour(gen_server).

-include_lib("enet/include/enet.hrl").

%% API
-export([ start_link/2 ]).

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
         port
        }).


%%%===================================================================
%%% API
%%%===================================================================

start_link(IP, Port) ->
    gen_server:start_link(?MODULE, [IP, Port], []).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([IP, Port]) ->
    process_flag(trap_exit, true),
    {ok, #state{ ip = IP, port = Port }}.


handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


handle_cast(_Request, State) ->
    {noreply, State}.


handle_info({enet, connect, remote, {_Host, Channels}, _ConnectID}, State) ->
    %%
    %% Handshake successful - peer is connected
    %%
    %% - TODO: Keep track of channels
    %%
    {noreply, State};
handle_info({enet, Channel, #unsequenced{ data = Data }}, State) ->
    %%
    %% Received unsequenced data - echo it
    %%
    {noreply, State};
handle_info({enet, Channel, #unreliable{ data = Data }}, State) ->
    %%
    %% Received unreliable data - echo it
    %%
    {noreply, State};
handle_info({enet, Channel, #reliable{ data = Data }}, State) ->
    %%
    %% Received reliable data - echo it
    %%
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
