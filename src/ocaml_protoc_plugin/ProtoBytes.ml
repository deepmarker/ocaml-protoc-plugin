open Core

include String

let sexp_of_t x = Sexp.Atom (Base64.encode_exn x)
let t_of_sexp = function
  | Sexp.Atom b64 -> Base64.decode_exn b64
  | List _ -> assert false

let yojson_of_t x = `String (Base64.encode_exn x)

let t_of_yojson = function
  | `String b64 -> Base64.decode_exn b64
  | #Yojson.Safe.t -> assert false


