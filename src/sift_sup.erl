-module(sift_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Metrics = {undefined,
               {sift_metric, start_link, []},
               permanent,
               5000,
               worker,
               [sift_metric]},
    {ok, {{simple_one_for_one, 5, 10}, [Metrics]}}.
