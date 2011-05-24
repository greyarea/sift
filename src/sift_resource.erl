-module(sift_resource).

-export([init/1,
         content_types_provided/2,
         to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) ->
    {ok, no_state}.

content_types_provided(ReqData, State) ->
    {[{"application/json", to_json}], ReqData, State}.

to_json(ReqData, State) ->
    Results = [{N, sift:get_value(N)} || N <- sift:metrics()],
    Metrics = [{list_to_binary(Key), Val} || {Key, {ok, Val}} <- Results],
    Output = {struct, [{metrics, {struct, Metrics}}]},
    {mochijson2:encode(Output), ReqData, State}.
