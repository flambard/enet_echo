-module(enet_echo_peer_sup).
-behaviour(supervisor).

%% API
-export([
         start_link/1
        ]).

%% Supervisor callbacks
-export([ init/1 ]).


%%%===================================================================
%%% API functions
%%%===================================================================

start_link(Port) ->
    supervisor:start_link(?MODULE, [Port]).


%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([ListeningPort]) ->
    Sup = self(),
    ConnectFun = fun(PeerInfo) ->
                         supervisor:start_child(Sup, [PeerInfo])
                 end,
    Options = [{peer_limit, 4}, {channel_limit, 4}],
    {ok, _Host} = enet:start_host(ListeningPort, ConnectFun, Options),
    SupFlags = #{
                 strategy => simple_one_for_one,
                 intensity => 1,
                 period => 5
                },
    ChildSpecs = [#{
                    id => enet_echo_peer,
                    start => {enet_echo_peer, start_link, []},
                    restart => permanent,
                    shutdown => 5000,
                    type => worker,
                    modules => [enet_echo_peer]
                   }],
    {ok, {SupFlags, ChildSpecs}}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
