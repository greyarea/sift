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

