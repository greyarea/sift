-module(sift_web).

-export([start/1,
         stop/0,
         loop/1]).

start(Config) ->
    mochiweb_http:start([{name, ?MODULE},
                         {loop, fun loop/1} | Config]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req) ->
    case Req:get(method) of
        'GET' ->
            Results = [{N, sift:get_value(N)} || N <- sift:metrics()],
            Metrics = [{list_to_binary(Key), Val} || {Key, {ok, Val}} <- Results],
            Output = {struct, [{metrics, {struct, Metrics}}]},
            Json = mochijson2:encode(Output),
            
            Req:respond({200, [{"Content-Type", "application/json"}], Json});
        _ ->
            Req:respond({501, [], []})
    end.
