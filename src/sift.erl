-module(sift).

%% API
-export([start/0,
         stop/0,
         metrics/0,
         get_value/1,
         report/0]).

start() ->
    ensure_started(sasl),
    ensure_started(gproc),
    ok = application:start(sift).

stop() ->
    ok = application:stop(sift).

metrics() ->
    gproc:select([{{{n, l, {sift, metric, '$1'}}, '_', '_'}, [], ['$1']}]).

get_value(Name) ->
    sift_metric:get_value(Name).

report() ->
    Results = [{N, get_value(N)} || N <- metrics()],
    io:format("~s~nSIFT METRICS REPORT~n~s~n",
              [string:copies("=", 72), string:copies("-", 72)]),
    lists:foreach(
      fun({Name, {ok, Val}}) ->
              io:format("~s = ~p~n", [Name, Val]);
         (_) ->
              ok
      end, Results),
    io:format("~s~n", [string:copies("=", 72)]),
    ok.
                              

%% internal functions

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, already_started} ->
            ok;
        Other ->
            Other
    end.
