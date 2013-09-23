%% This is a small modification of eunit's assertMatch macro
%% to return the result of expression.
%% Pattern and Guard are separated to get rid of unused variables warning.
-define(assertMatchR(Pattern, Expr),
        ((fun () ->
                  __Result = (Expr),
                  case __Result of
                      Pattern -> Pattern = __Result;
                      __V -> erlang:error({assertMatch_failed,
                                           [{module, ?MODULE},
                                            {line, ?LINE},
                                            {expression, (??Expr)},
                                            {pattern, (??Pattern)},
                                            {value, __V}]})
                  end
          end)())).
-define(assertMatchR(Pattern, Guard, Expr),
        ((fun () ->
                  __Result = (Expr),
                  case __Result of
                      Pattern when Guard -> Pattern = __Result;
                      __V -> erlang:error({assertMatch_failed,
                                           [{module, ?MODULE},
                                            {line, ?LINE},
                                            {expression, (??Expr)},
                                            {pattern, (??Pattern " when " ??Guard)},
                                            {value, __V}]})
                  end
          end)())).

%% These are helpers that allow to pattern match expression
%% and have matched variables exported to the outer context.
-define(assertMatchEx(Pattern, Expr), (Pattern = ?assertMatchR(Pattern, Expr))).
-define(assertMatchEx(Pattern, Guard, Expr), (Pattern = ?assertMatchR(Pattern, Guard, Expr))).
