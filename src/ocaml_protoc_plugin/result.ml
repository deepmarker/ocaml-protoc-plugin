(** This module provides result type and functions for compatibility
 * with OCaml 4.06 *)

type error =
  [ `Premature_end_of_input
  | `Unknown_field_type of int
  | `Wrong_field_type of string * Field.t
  | `Illegal_value of string * Field.t
  | `Unknown_enum_value of int
  | `Oneof_missing
  | `Required_field_missing ]

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

let pp_error : Format.formatter -> [> error] -> unit = fun fmt -> function
  | `Premature_end_of_input ->
    Format.pp_print_string fmt
      "`Premature_end_of_input"
  | `Unknown_field_type x ->
    (Format.fprintf fmt
       "`Unknown_field_type (@[<hov>";
     (Format.fprintf fmt "%d") x;
     Format.fprintf fmt "@])")
  | `Wrong_field_type x ->
    (Format.fprintf fmt
       "`Wrong_field_type (@[<hov>";
     ((fun (a0, a1) ->
         Format.fprintf fmt "(@[";
         ((Format.fprintf fmt "%S") a0;
          Format.fprintf fmt ",@ ";
          (Field.pp fmt) a1);
         Format.fprintf fmt "@])")) x;
     Format.fprintf fmt "@])")
  | `Illegal_value x ->
    (Format.fprintf fmt
       "`Illegal_value (@[<hov>";
     ((fun (a0, a1) ->
         Format.fprintf fmt "(@[";
         ((Format.fprintf fmt "%S") a0;
          Format.fprintf fmt ",@ ";
          (Field.pp fmt) a1);
         Format.fprintf fmt "@])")) x;
     Format.fprintf fmt "@])")
  | `Unknown_enum_value x ->
    (Format.fprintf fmt
       "`Unknown_enum_value (@[<hov>";
     (Format.fprintf fmt "%d") x;
     Format.fprintf fmt "@])")
  | `Oneof_missing ->
    Format.pp_print_string fmt "`Oneof_missing"
  | `Required_field_missing ->
    Format.pp_print_string fmt
      "`Required_field_missing"
let show_error : error -> string = Format.asprintf "%a" pp_error

let pp pp fmt = function
  | Ok v -> Format.fprintf fmt "Ok %a" pp v
  | Error (#error as e) -> Format.fprintf fmt "Error %a" pp_error e
(* let show : 'a t -> string = Format.asprintf "%a" pp *)

open Base

let to_or_error = function
  | Ok x -> Ok x
  | Error x -> Error (Error.of_string (show_error x))
;;

let ok_exn = function
  | Ok x -> x
  | Error x -> Stdlib.Format.kasprintf failwith "%a" pp_error x
;;
