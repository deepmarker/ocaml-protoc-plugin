open Core
open Ppx_yojson_conv_lib.Yojson_conv

type t =
  | Varint of int64 (* int32, int64, uint32, uint64, sint32, sint64, bool, enum *)
  | Fixed_64_bit of int64 (* fixed64, sfixed64, double *)
  | Length_delimited of {
      offset : int;
      length : int;
      data : string;
    } (* string, bytes, embedded messages, packed repeated fields *)
  | Fixed_32_bit of int32 (* fixed32, sfixed32, float *)
[@@deriving bin_io, hash, compare, sexp, yojson]

let pp ppf t = Sexp.pp ppf (sexp_of_t t)
let show t = Stdlib.Format.asprintf "%a" pp t

let varint v = Varint v
let fixed_32_bit v = Fixed_32_bit v
let fixed_64_bit v = Fixed_64_bit v
let length_delimited ?(offset=0) ?length data =
  let length = Option.value ~default:(String.length data - offset) length in
  Length_delimited {offset; length; data}

