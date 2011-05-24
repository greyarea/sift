%% @doc Counters are values that can be incremented and decremented.
%% Counters are created with a name and can be changed by integer
%% values (commonly 1 and -1).
%% @end

-module(sift_counter).

-behaviour(sift_metric).

%% API
-export([create/1,
         create/2,
         inc/1,
         inc/2,
         dec/1,
         dec/2]).

%% sift_metric callbacks
-export([init/1,
         terminate/2,
         handle_cmd/3,
         get_value/1]).
 
create(Name) ->
    create(Name, 0).

create(Name, InitialValue) ->
    Config = [{name, Name},
              {metric, ?MODULE},
              {init_val, InitialValue}],
    supervisor:start_child(sift_metric_sup, [Config]).

inc(Name) ->
    inc(Name, 1).

inc(Name, Amount) ->
    case sift_metric:find(Name) of
        false ->
            {ok, _} = create(Name, Amount);
        Pid ->
            sift_metric:do(Pid, inc, [Amount])
    end,
    ok.

dec(Name) ->
    inc(Name, -1).

dec(Name, Amount) ->
    inc(Name, -Amount).

init(Config) ->
    InitialValue = proplists:get_value(init_val, Config),
    {ok, InitialValue}.

terminate(_Reason, _State) ->
    ok.

handle_cmd(inc, [], State) ->
    {ok, State + 1};
handle_cmd(inc, [X], State) ->
    {ok, State + X}.
 
get_value(State) ->
    {ok, State}.
