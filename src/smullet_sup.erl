-module(smullet_sup).

-behaviour(supervisor).

%% API
-export([start_link/2, start_link/3, start_child/2]).

%% Supervisor callbacks
-export([init/1]).


start_link(Timeout, Module) ->
    supervisor:start_link(?MODULE, [Timeout, Module]).


start_link(SupervisorName, Timeout, Module) ->
    supervisor:start_link({local, SupervisorName}, ?MODULE, [Timeout, Module]).


start_child(Supervisor, Key) ->
    supervisor:start_child(Supervisor, [Key]).


init(Params) ->
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 1,

    {ok, {{simple_one_for_one, MaxRestarts, MaxSecondsBetweenRestarts},
          [{smullet, {smullet_session, start_link, Params},
            temporary, 1000, worker, [smullet_session]}]}}.
