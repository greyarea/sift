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

## Introspection

You can list all metrics with:

    sift:metrics().

and generate a pretty report with:

    sift:report().

    
