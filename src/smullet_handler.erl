-module(smullet_handler).
%% -behaviour(bullet_handler). % Sadly there is no such behaviour.

%% bullet handler callbacks
-export([init/4, stream/3, info/3, terminate/2]).


-callback session(Req) -> {ok, Session, Req}
                              | {not_found, Req}
                              when Req :: cowboy_req:req(),
                                   Session :: smullet_session:session().
-callback info(Msg) -> {reply, Data}
                           | ok | shutdown
                           when Msg :: term(),
                                Data :: iodata().


-record(state, {active, session, handler, ref}).


init(_Transport, Req, Opts, Active) ->
    {_, Handler} = lists:keyfind(smullet_handler, 1, Opts),
    case Handler:session(Req) of
        {ok, Session, Req1} ->
            Ref = case Active of
                      false ->
                          unefined;
                      _ when Active =:= once; Active =:= true ->
                          smullet_session:recv(Session)
                  end,
            {ok, Req1, #state{active = Active, session = Session,
                              handler = Handler, ref = Ref}};
        {not_found, Req1} ->
            {shutdown, Req1, undefined}
    end.


stream(_Data, Req, State) ->
    lager:debug("client sent: ~p", [_Data]),
    {ok, Req, State}.


info({Ref, Info}, Req, #state{ref=Ref, handler=Handler, active=Active, session=Session} = State) ->
    case Handler:info(Info) of
        shutdown ->
            {shutdown, Req, State};
        Result ->
            NewState = if Active -> State#state{ref=smullet_session:recv(Session)};
                          true -> State
                       end,
            case Result of
                {reply, Msg} ->
                    {reply, Msg, Req, NewState};
                ok ->
                    {ok, Req, NewState}
            end
    end;
info(Info, Req, #state{handler=Handler}=State) ->
    case Handler:info(Info) of
        {reply, Msg} ->
            {reply, Msg, Req, State};
        Other when Other =:= ok; Other =:= shutdown ->
            {Other, Req, State}
    end.


terminate(_Req, State) ->
    lager:info("terminating: ~p", [State]).
