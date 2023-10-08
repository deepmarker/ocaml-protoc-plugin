open Core
module Time_ns = Time_ns_unix
module Any = GoogleProtobufAny.Any
module Api = GoogleProtobufApi
module Descriptor = GoogleProtobufDescriptor

module Duration = struct
  include GoogleProtobufDuration.Duration

  let span_of_duration (x : t) =
    Time_ns.Span.of_int_ns ((x.seconds * 1_000_000_000) + x.nanos)
  ;;

  let duration_of_span x =
    let ns = Time_ns.Span.to_int_ns x in
    let seconds = ns / 1_000_000_000 in
    let nanos = ns mod 1_000_000_000 in
    make ~seconds ~nanos ()
  ;;

  let yojson_of_t t =
    Stdlib.Format.ksprintf (fun s -> `String s) "%fs" (Time_ns.Span.to_sec (span_of_duration t))

  let t_of_yojson json =
    let str = Yojson.Safe.Util.to_string json in
    let secs = Float.of_string (String.sub str ~pos:0 ~len:(String.length str -1)) in
    Time_ns.Span.of_sec secs |> duration_of_span
end

module Empty = GoogleProtobufEmpty.Empty
module Field_mask = GoogleProtobufField_mask.FieldMask
module Source_context = GoogleProtobufSource_context.SourceContext

module Struct = struct
  include GoogleProtobufStruct

  module type StructSig = module type of Struct
  module type ValueSig = module type of Value

  module rec Struct : StructSig = struct
    include Struct

    let yojson_of_t xs =
      `Assoc (List.map xs ~f:(fun (k, v) -> k, Value.yojson_of_t (Option.value_exn ~here:[%here] v)))

    let t_of_yojson = function
      | `Assoc xs -> List.map xs ~f:(fun (k, v) -> k, Some (Value.t_of_yojson v))
      | #Yojson.Safe.t -> assert false
  end
  and Value : ValueSig = struct
    include Value

    let rec yojson_of_t = function
      | `Bool_value b -> `Bool b
      | `Struct_value x -> Struct.yojson_of_t x
      | `String_value x  -> `String x
      | `not_set -> assert false
      | `Number_value x -> `Float x
      | `Null_value _ -> `Null
      | `List_value xs  -> `List (List.map ~f:yojson_of_t xs)

    let t_of_yojson = function
      | `Assoc xs -> `Struct_value (List.map ~f:(fun (k, v) -> k, Some (Value.t_of_yojson v)) xs)
      | `Bool x -> `Bool_value x
      | `Float x -> `Number_value x
      | `List xs -> `List_value (List.map ~f:t_of_yojson xs)
      | `Null -> `Null_value (NullValue.NULL_VALUE)
      | `String x -> `String_value x
      | #Yojson.Safe.t -> assert false
  end
end

module Timestamp = struct
  include GoogleProtobufTimestamp.Timestamp

  let time_of_timestamp (x : t) =
    Time_ns.of_int_ns_since_epoch ((x.seconds * 1_000_000_000) + x.nanos)
  ;;

  let timestamp_of_time x =
    let ns = Time_ns.to_int_ns_since_epoch x in
    let seconds = ns / 1_000_000_000 in
    let nanos = ns mod 1_000_000_000 in
    make ~seconds ~nanos ()
  ;;

  let yojson_of_t t =
    `String (time_of_timestamp t |> Time_ns.to_string_iso8601_basic ~zone:Time_ns.Zone.utc)

  let t_of_yojson json =
    Yojson.Safe.Util.to_string json |> Time_ns.of_string |> timestamp_of_time
end

module Type = GoogleProtobufType
module Wrappers = GoogleProtobufWrappers
