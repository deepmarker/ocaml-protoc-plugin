open Spec.Descriptor.Google.Protobuf
open Base

type element = {
  file_name : string; (** filename of the .proto file *)
  ocaml_name : string list;
  cyclic : bool;
}

type t = element StringMap.t

(** Get the module name of a proto file *)
val module_name_of_proto : string -> string

val create : FileDescriptorProto.t list -> t

(** [dump t] writes an unspecified representation of [t] on stderr. *)
val dump : t -> unit
