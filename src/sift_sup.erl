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

    WebConfig = [{port, 7438},
                 {ip, any}],

    Web = {sift_web,
           {sift_web, start, [WebConfig]},
           permanent,
           5000,
           worker,
           dynamic},

    Processes = [MetricSup, Web],
    {ok, {{one_for_one, 5, 10}, Processes}}.
