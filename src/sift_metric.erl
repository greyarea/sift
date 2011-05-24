-module(sift_metric).

-behaviour(gen_server).

%% API
-export([get_value/1, find/1, do/3, start_link/1]).

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

get_value(Name) when is_list(Name) ->
    RegName = name_to_atom(Name),
    case whereis(RegName) of
        undefined ->
            {error, no_exists};
        _ ->
            gen_server:call(name_to_atom(Name), get_value)
    end;
get_value(Name) when is_atom(Name) ->
    get_value(atom_to_list(Name)).

find(Name) ->
    AtomName = name_to_atom(Name),
    case whereis(AtomName) of
        undefined ->
            false;
        Pid ->
            Pid
    end.

do(Pid, Command, Args) ->
    gen_server:cast(Pid, {cmd, Command, Args}).

start_link(Config) ->
    Name = proplists:get_value(name, Config),
    gen_server:start_link({local, name_to_atom(Name)}, ?MODULE, Config, []).

behaviour_info(callbacks) ->
    [{init, 1},
     {terminate, 2},
     {handle_cmd, 3},
     {get_value, 1}];
behaviour_info(_) ->
    undefined.

init(Config) ->
    Metric = proplists:get_value(metric, Config),
    case Metric:init(Config) of
        {ok, MetricState} ->
            {ok, #st{metric=Metric, metric_state=MetricState}};
        Other ->
            {stop, Other}
    end.

terminate(Reason, #st{metric=Metric, metric_state=MetricState}) ->
    Metric:terminate(Reason, MetricState),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call(get_value, _From,
            State = #st{metric=Metric, metric_state=MetricState}) ->
    Reply = Metric:get_value(MetricState),
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

%% internal functions

name_to_atom(Name) when is_atom(Name) ->
    list_to_atom("SIFT@" ++ atom_to_list(Name));
name_to_atom(Name) when is_list(Name) ->
    name_to_atom(list_to_atom(Name)).

