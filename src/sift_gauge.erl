%% @doc Gauges are the instantaneous value of something. Gauges are
%% created with a name and a fun which can return the value as a
%% float, integer, or undefined.
%% @end
-module(sift_gauge).

-behaviour(sift_metric).

%% API
-export([create/2]).

%% sift_metric callbacks
-export([init/1,
         terminate/2,
         handle_cmd/3,
         get_value/1]).
 
create(Name, ValueFun) ->
    Config = [{name, Name},
              {metric, ?MODULE},
              {value_fun, ValueFun}],
    supervisor:start_child(sift_metric_sup, [Config]).

init(Config) ->
    ValueFun = proplists:get_value(value_fun, Config),
    {ok, ValueFun}.

terminate(_Reason, _State) ->
    ok.

handle_cmd(_Cmd, _Args, State) ->
    {error, unknown_command, State}.
 
get_value(State) ->
    {ok, State()}.
