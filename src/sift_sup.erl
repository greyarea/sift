-module(sift_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    MetricSup = {sift_metric_sup,
                 {sift_metric_sup, start_link, []},
                 permanent,
                 infinity,
                 supervisor,
                 [sift_metric_sup]},

    DispatchFile = filename:join([filename:dirname(code:which(?MODULE)),
                                  "..",
                                  "priv",
                                  "dispatch.conf"]),
    {ok, Dispatch} = file:consult(DispatchFile),
    WebConfig = [{port, 7438},
                 {dispatch, Dispatch}],

    Web = {sift_web,
           {webmachine_mochiweb, start, [WebConfig]},
           permanent,
           5000,
           worker,
           dynamic},

    Processes = [MetricSup, Web],
    {ok, {{one_for_one, 5, 10}, Processes}}.
