open Spec.Descriptor.Google.Protobuf

type t

(** Get the module name of a proto file *)
val module_name_of_proto : string -> string

val create : FileDescriptorProto.t list -> t

(* Below are option types because the path might not exist in DB. *)

(* Return segments of ocaml name, in normal order. *)
val ocaml_name : t -> string list -> string list

val file_name : t -> string list -> string
val is_cyclic : t -> string list -> bool

(** [dump t] writes an unspecified representation of [t] on stderr. *)
val dump : t -> unit
