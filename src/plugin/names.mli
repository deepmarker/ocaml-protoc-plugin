val module_name : ?mangle_f:(string -> string) -> string -> string

val is_reserved : string -> bool
val to_snake_case : string -> string

(* My functions *)

val escape_reserved : string -> string
