open Core
open Google_types
open GoogleProtobufTimestamp
open GoogleProtobufDuration

val to_or_error : ('a, Ocaml_protoc_plugin.Result.error) result -> 'a Or_error.t
val ok_exn : ('a, Ocaml_protoc_plugin.Result.error) result -> 'a
val time_of_timestamp : Timestamp.t -> Time_ns.t
val timestamp_of_time : Time_ns.t -> Timestamp.t
val span_of_duration : Duration.t -> Time_ns.Span.t
val duration_of_span : Time_ns.Span.t -> Duration.t
