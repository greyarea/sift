%% @doc Meters measure the rate of events over a period of
%% time. Meters are created with a name and a time base, and they
%% produce four metrics, NAME.avg, NAME.avg1, NAME.avg5,
%% NAME.avg15. The first is the average rate of all events since the
%% metric was created. The rest are exponentially weighted moving
%% averages calculated similar to load averages in Linux.

-module(sift_meter).

-behaviour(sift_metric).

%% API
-export([create/1,
         create/2,
         mark/1,
         mark/2]).

%% sift_metric callbacks
-export([init/1,
         terminate/2,
         handle_cmd/3,
         get_value/2]).

-define(TICK_INTERVAL, 5).
-define(ALPHA(Minutes), (1 - math:exp(-?TICK_INTERVAL / 60 / Minutes))).

-record(st, {name, time_base, count, start_time,
             alpha1, alpha5, alpha15,
             ewma1, ewma5, ewma15}).

create(Name) ->
    create(Name, seconds).

create(Name, TimeBase) ->
    Config = [{name, Name},
              {metric, ?MODULE},
              {time_base, TimeBase}],
    supervisor:start_child(sift_metric_sup, [Config]).

mark(Name) ->
    mark(Name, 1).

mark(Name, Amount) ->
    Pid = case sift_metric:find(Name ++ ".avg") of
              false ->
                  {ok, P} = create(Name, seconds),
                  P;
              P ->
                  P
          end,
    sift_metric:do(Pid, mark, [Amount]),
    ok.

init(Config) ->
    Name = proplists:get_value(name, Config),
    TimeBase = proplists:get_value(time_base, Config),

    sift_metric:register(Name ++ ".avg"),
    sift_metric:register(Name ++ ".avg1"),
    sift_metric:register(Name ++ ".avg5"),
    sift_metric:register(Name ++ ".avg15"),

    EWMA1 = sift_ewma:new(?ALPHA(1), ?TICK_INTERVAL),
    EWMA5 = sift_ewma:new(?ALPHA(5), ?TICK_INTERVAL),
    EWMA15 = sift_ewma:new(?ALPHA(15), ?TICK_INTERVAL),

    timer:apply_interval(?TICK_INTERVAL * 1000,
                         sift_metric, do, [self(), tick, []]),

    {ok, #st{name=Name, time_base=TimeBase, count=0, start_time=now(),
             ewma1=EWMA1, ewma5=EWMA5, ewma15=EWMA15}}.

terminate(_Reason, _State) ->
    ok.

handle_cmd(mark, [X], State = #st{count=Count,
                                  ewma1=EWMA1, ewma5=EWMA5, ewma15=EWMA15}) ->
    NewEWMA1 = sift_ewma:update(X, EWMA1),
    NewEWMA5 = sift_ewma:update(X, EWMA5),
    NewEWMA15 = sift_ewma:update(X, EWMA15),

    {ok, State#st{count=Count + X, ewma1=NewEWMA1, ewma5=NewEWMA5, ewma15=NewEWMA15}};
handle_cmd(tick, [], State = #st{ewma1=EWMA1, ewma5=EWMA5, ewma15=EWMA15}) ->
    NewEWMA1 = sift_ewma:tick(EWMA1),
    NewEWMA5 = sift_ewma:tick(EWMA5),
    NewEWMA15 = sift_ewma:tick(EWMA15),
    {ok, State#st{ewma1=NewEWMA1, ewma5=NewEWMA5, ewma15=NewEWMA15}}.
 
get_value(Name, #st{name=BaseName, time_base=TimeBase, start_time=StartTime, count=Count,
                    ewma1=EWMA1, ewma5=EWMA5, ewma15=EWMA15}) ->
    Metric = lists:sublist(Name, length(BaseName) + 2, length(Name)),
    Rate = case Metric of
               "avg" ->
                   Diff = timer:now_diff(now(), StartTime),
                   Count / Diff * time_base_to_micros(TimeBase);
               [_, _, _ | T] ->
                   EWMA = case T of
                              "1" -> EWMA1;
                              "5" -> EWMA5;
                              "15" -> EWMA15
                          end,
                   sift_ewma:get_rate(EWMA) * time_base_to_micros(TimeBase)
           end,
    {ok, Rate}.

%% internal functions

time_base_to_micros(seconds) ->
    1000000;
time_base_to_micros(minutes) ->
    60000000;
time_base_to_micros(hours) ->
    3600000000.
