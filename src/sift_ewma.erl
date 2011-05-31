-module(sift_ewma).

-export([new/2, update/2, tick/1, get_rate/1]).

-record(ewma, {alpha, interval, count, rate}).

new(Alpha, Interval) ->
    #ewma{alpha=Alpha, interval=Interval * 1000000, count=0}.

update(N, EWMA = #ewma{count=Count}) ->
    EWMA#ewma{count=Count + N}.

tick(EWMA = #ewma{alpha=Alpha, interval=Interval, count=Count, rate=Rate}) ->
    CurrentRate = Count / Interval,
    NewRate = case Rate of
                  undefined ->
                      CurrentRate;
                  _ ->
                      Rate + Alpha * (CurrentRate - Rate)
              end,
    EWMA#ewma{count=0, rate=NewRate}.

get_rate(#ewma{rate=Rate}) ->
    case Rate of
        undefined ->
            0;
        N ->
            N
    end.
