-module(enet_echo_sup).
-behaviour(supervisor).

%% API
-export([
         start_link/0,
         start_peer_sup/1
        ]).

%% Supervisor callbacks
-export([ init/1 ]).

-define(SERVER, ?MODULE).


%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_peer_sup(Port) ->
    supervisor:start_child(?SERVER, [Port]).


%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
    SupFlags = #{
                 strategy => simple_one_for_one,
                 intensity => 1,
                 period => 3
                },
    ChildSpecs = [#{
                    id => enet_echo_peer_sup,
                    start => {enet_echo_peer_sup, start_link, []},
                    restart => permanent,
                    shutdown => infinity,
                    type => supervisor,
                    modules => [enet_echo_peer_sup]
                   }],
    {ok, {SupFlags, ChildSpecs}}.


%%====================================================================
%% Internal functions
%%====================================================================
