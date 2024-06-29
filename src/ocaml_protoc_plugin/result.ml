module ProtoField = Field
open Core

type error =
  [ `Premature_end_of_input
  | `Unknown_field_type of int
  | `Wrong_field_type of string * ProtoField.t
  | `Illegal_value of string * ProtoField.t
  | `Unknown_enum_value of int
  | `Oneof_missing
  | `Required_field_missing ] [@@deriving sexp]

let pp_error ppf t = Sexp.pp ppf (sexp_of_error t)

type 'a t = ('a, error) result

let ( >>| ) v f = match v with Ok x -> Ok (f x) | Error err -> Error err
let ( >>= ) v f = match v with Ok x -> f x | Error err -> Error err
let open_error = function
  | Ok _ as v -> v
  | Error #error as v -> v

(* Extra functions (from Base) *)

let return x = Ok x
let fail x = Error x
let get ~msg = function
  | Ok v -> v
  | Error _ -> failwith msg


let pp pp fmt = function
  | Ok v -> Format.fprintf fmt "Ok %a" pp v
  | Error (#error as e) -> Format.fprintf fmt "Error %a" pp_error e
(* let show : 'a t -> string = Format.asprintf "%a" pp *)

open Base

let to_or_error = function
  | Ok x -> Ok x
  | Error x -> Error (Error.of_lazy_sexp (lazy (sexp_of_error x)))
;;

let ok_exn = function
  | Ok x -> x
  | Error x -> Stdlib.Format.kasprintf failwith "%a" pp_error x
;;
