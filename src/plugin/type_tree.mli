open MoreLabels
open Spec.Descriptor.Google.Protobuf

module StringMap : sig
  include Map.S with type key = string
  val add_uniq: key:key -> data:'a -> 'a t -> 'a t
end

type element = {
  file_name : string; (** filename of the .proto file *)
  ocaml_name : string;
  cyclic : bool;
}

type t = element StringMap.t

(** Get the module name of a proto file *)
val module_name_of_proto : string -> string

val create : FileDescriptorProto.t list -> t

(** [dump t] writes an unspecified representation of [t] on stderr. *)
val dump : t -> unit
