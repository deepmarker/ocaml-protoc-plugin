open Core
open Google_types
open GoogleProtobufTimestamp
open GoogleProtobufDuration

let to_or_error = function
  | Ok x -> Ok x
  | Error x -> Error (Error.of_string (Ocaml_protoc_plugin.Result.show_error x))
;;

let ok_exn = function
  | Ok x -> x
  | Error x -> Format.kasprintf failwith "%a" Ocaml_protoc_plugin.Result.pp_error x
;;

let time_of_timestamp (x : Timestamp.t) =
  Time_ns.of_int_ns_since_epoch ((x.seconds * 1_000_000_000) + x.nanos)
;;

let timestamp_of_time x =
  let ns = Time_ns.to_int_ns_since_epoch x in
  let seconds = ns / 1_000_000_000 in
  let nanos = ns mod 1_000_000_000 in
  Timestamp.make ~seconds ~nanos ()
;;

let span_of_duration (x : Duration.t) =
  Time_ns.Span.of_int_ns ((x.seconds * 1_000_000_000) + x.nanos)
;;

let duration_of_span x =
  let ns = Time_ns.Span.to_int_ns x in
  let seconds = ns / 1_000_000_000 in
  let nanos = ns mod 1_000_000_000 in
  Duration.make ~seconds ~nanos ()
;;
