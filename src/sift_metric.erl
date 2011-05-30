-module(sift_metric).

-behaviour(gen_server).

%% API
-export([get_value/1, find/1, do/3, register/1, start_link/1]).

%% sift_metric behaviour callback
-export([behaviour_info/1]).

%% gen_server callbacks
-export([init/1,
         terminate/2,
         code_change/3,
         handle_call/3,
         handle_cast/2,
         handle_info/2]).

-record(st, {metric, metric_state}).

get_value(Name) ->
    case gproc:lookup_local_name({sift, metric, Name}) of
        undefined ->
            {error, no_exists};
        Pid ->
            gen_server:call(Pid, {get_value, Name})
    end.

find(Name) ->
    case gproc:lookup_local_name({sift, metric, Name}) of
        undefined ->
            false;
        Pid ->
            Pid
    end.

do(Pid, Command, Args) ->
    gen_server:cast(Pid, {cmd, Command, Args}).

register(Name) ->
    true = gproc:add_local_name({sift, metric, Name}),
    ok.

start_link(Config) ->
    gen_server:start_link(?MODULE, Config, []).

behaviour_info(callbacks) ->
    [{init, 1},
     {terminate, 2},
     {handle_cmd, 3},
     {get_value, 2}];
behaviour_info(_) ->
    undefined.

init(Config) ->
    Metric = proplists:get_value(metric, Config),
    case Metric:init(Config) of
        {ok, MetricState} ->
            gproc:add_local_property({sift, metric}, Metric),
            {ok, #st{metric=Metric, metric_state=MetricState}};
        Other ->
            {stop, Other}
    end.

terminate(Reason, #st{metric=Metric, metric_state=MetricState}) ->
    Metric:terminate(Reason, MetricState),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call({get_value, Name}, _From,
            State = #st{metric=Metric, metric_state=MetricState}) ->
    Reply = Metric:get_value(Name, MetricState),
    {reply, Reply, State#st{metric_state=MetricState}}.

handle_cast({cmd, Cmd, Args},
            State = #st{metric=Metric, metric_state=MetricState}) ->
    case Metric:handle_cmd(Cmd, Args, MetricState) of
        {error, Reason, NewMetricState} ->
            {stop, Reason, State#st{metric_state=NewMetricState}};
        {ok, NewMetricState} ->
            {noreply, State#st{metric_state=NewMetricState}}
    end.

handle_info(_Info, State) ->
    {stop, unknown_info, State}.
