-module(smullet_session_test).
-behavoiur(smullet_session).

%% smullet_session callbacks
-export([init/1, handle_info/2, terminate/2]).

-include_lib("eunit/include/eunit.hrl").
-include("assert_ex.hrl").

-define(t, 200).
-define(last_state, {p, l, ?MODULE}).
-define(assertRef(Expression), ?assertMatchR(Ref, is_reference(Ref), Expression)).


sessions_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun sessions/0}.

sessions() ->
    %% create sessions
    ?assertMatchEx({ok, S1}, smullet_session:new(?MODULE, 1)),
    ?assertMatch({init, 1}, st()),
    ?assertMatch({error, {already_registered, S1}}, smullet_session:new(?MODULE, 1)),
    ?assertMatchEx({ok, S2}, smullet_session:new(?MODULE, 2)),
    ?assertMatch({init, 2}, st()),

    %% subscribe for messages
    Ref1 = ?assertRef(smullet_session:recv(S1)),
    Ref2 = ?assertRef(smullet_session:recv(S2)),

    %% impossible to subscribe if anyone is already subscribed to session
    ?assertMatch(error, smullet_session:recv(S1)),

    %% this message that is immediately sent to the subscriber
    ?assertMatch(ok, smullet_session:send(S1, m1)),
    ?assertMatch({msg, {Ref1, m1}}, msg()),

    %% this message is saved for next delivery
    ?assertMatch(ok, smullet_session:send(S1, m2)),
    ?assertMatch(no_msg, msg()),

    %% now it is delivered
    ?assertMatchEx(Ref3, is_reference(Ref3), smullet_session:recv(S1)),
    ?assertMatch({msg, {Ref3, m2}}, msg()),

    %% session is terminated due to inactivity
    ?assertMatch(ok, timer:sleep(?t + ?t)),
    ?assertMatch(undefined, smullet_session:find(1)),
    ?assertExit({noproc, _}, smullet_session:send(S1, m3)),

    %% but second is alive because was subscribed to
    ?assertMatchEx(ok, smullet_session:send(smullet_session:find(2), n1)),
    ?assertMatch({msg, {Ref2, n1}}, msg()).


init(Key) ->
    gproc:set_value_shared(?last_state, {init, Key}),
    {ok, undefined}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_, _State) ->
    ok.


st() ->
    gproc:get_value(?last_state, shared).


setup() ->
    application:start(gproc),
    gproc:reg_shared(?last_state),
    {ok, Pid} = smullet_sup:start_link(?MODULE, ?t, ?MODULE),
    Pid.


cleanup(Pid) ->
    gproc:unreg_shared(?last_state),
    unlink(Pid),
    exit(Pid, kill).


msg() ->
        receive
            Msg ->
                {msg, Msg}
        after 0 ->
                no_msg
        end.
