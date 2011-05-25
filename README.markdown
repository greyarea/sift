# sift

Sift is a metrics collection library for Erlang/OTP applications.

Five types of metrics are supported:
- gauges
- counters
- meters
- histograms
- timers

## Architecture

Each metric is represented by an Erlang process. The current value of
each metrics is represented in JSON form in the Sift HTTP output page.

Metrics spring into being when used, and don't normally need to be
pre-created. One exception to this is gauges, since they are
represented as funs returning the current value.

## Gauges

Examples: 

    sift_gauge:create("gauge.erlang.processes", fun() -> length(processes()) end).

    sift_gauge:create("gauge.erlang.waiting_msgs", fun () -> lists:sum([element(2, process_info(P, message_queue_len)) || P <- processes()]) end).

Now you can view the current values of these gauges with:

    sift:get_value("gauge.erlang.processes").

or:

    sift:get_value(gauge.erlang.waiting_msgs).

## Counters

Examples:
   
    sift_counter:inc("counter.duality.connected_users").
    sift_counter:dec("counter.duality.connected_users", 1).

There is no need to use `sift_counter:create/1` or
`sift_counter:create/2` as counters are automatically created when
first used. Automatically created counters start at zero.

## Introspection

You can list all metrics with:

    sift:metrics().

and generate a pretty report with:

    sift:report().

    
## HTTP API

Sift exposes all the metrics and their values at http://host:7438/ as
a JSON object.  Note that S-I-F-T is 7-4-3-8 in T9.

Example:

    {"metrics":{"gauge.erlang.processes":1023,"gauge.erlang.waiting_msgs",0}}
