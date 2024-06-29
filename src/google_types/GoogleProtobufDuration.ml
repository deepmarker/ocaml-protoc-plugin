(************************************************)
(*       AUTOGENERATED FILE - DO NOT EDIT!      *)
(************************************************)
(* Generated by: ocaml-protoc-plugin            *)
(* https://github.com/issuu/ocaml-protoc-plugin *)
(************************************************)
(*
  Source: google/protobuf/duration.proto
  Syntax: proto3
  Parameters:
    debug=false
    annot='[@@deriving compare, equal, hash, sexp, yojson]'
    opens=[Core; Ppx_yojson_conv_lib.Yojson_conv]
    int64_as_int=true
    int32_as_int=true
    fixed_as_int=false
    singleton_record=false
*)

open Ocaml_protoc_plugin.Runtime [@@warning "-33"]
open Core [@@warning "-33"]
open Ppx_yojson_conv_lib.Yojson_conv [@@warning "-33"]
module rec Duration : sig
  val name': unit -> string
  type t = { seconds: int; nanos: int } [@@deriving compare, equal, hash, sexp, yojson, bin_io]
  val create : ?seconds:int -> ?nanos:int -> unit -> t
  val to_proto: t -> Writer.t
  val from_proto: Reader.t -> (t, [> PResult.error]) Result.t
  val to_span : t -> Time_ns.Span.t
  val of_span : Time_ns.Span.t -> t
end = struct 
  let name' () = ""
  type t = { seconds: int; nanos: int } [@@deriving compare, equal, hash, sexp, yojson, bin_io]
  let create =
    fun ?seconds ?nanos () -> 
    let seconds = match seconds with Some v -> v | None -> 0 in
    let nanos = match nanos with Some v -> v | None -> 0 in
    { seconds; nanos }
  
  let to_proto =
    let apply = fun ~f:f' { seconds; nanos } -> f' [] seconds nanos in
    let spec = Serialize.C.( basic (1, int64_int, proto3) ^:: basic (2, int32_int, proto3) ^:: nil ) in
    let serialize = Serialize.serialize [] (spec) in
    fun t -> apply ~f:serialize t
  
  let from_proto =
    let constructor = fun _extensions seconds nanos -> { seconds; nanos } in
    let spec = Deserialize.C.( basic (1, int64_int, proto3) ^:: basic (2, int32_int, proto3) ^:: nil ) in
    let deserialize = Deserialize.deserialize [] spec constructor in
    fun writer -> deserialize writer |> PResult.open_error

    let to_span x =
    Time_ns.Span.of_int_ns ((x.seconds * 1_000_000_000) + x.nanos)
  ;;

  let of_span x =
    let ns = Time_ns.Span.to_int_ns x in
    let seconds = ns / 1_000_000_000 in
    let nanos = ns mod 1_000_000_000 in
    create ~seconds ~nanos ()
  ;;

  let yojson_of_t t =
    Stdlib.Format.ksprintf (fun s -> `String s) "%fs" (Time_ns.Span.to_sec (to_span t))

  let t_of_yojson json =
    let str = Yojson.Safe.Util.to_string json in
    let secs = Float.of_string (String.sub str ~pos:0 ~len:(String.length str -1)) in
    Time_ns.Span.of_sec secs |> of_span
end
