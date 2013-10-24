-module(smullet_app).
-behaviour(application).
-behaviour(supervisor).


%% application callbacks
-export([start/2, stop/1]).

%% Supervisor callbacks
-export([init/1]).


start(_StartType, _StartArgs) ->
    supervisor:start_link({local, smullet_sup}, ?MODULE, []).


stop(_) ->
    ok.


init([]) ->
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 1,

    {ok, {{simple_one_for_one, MaxRestarts, MaxSecondsBetweenRestarts},
          [{smullet_group, {smullet_group, start_link, []},
            permanent, infinity, supervisor, [smullet_group]}]}}.
