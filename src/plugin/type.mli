open Spec.Descriptor.Google.Protobuf

include module type of Type_intf

val add_fd : t -> FileDescriptorProto.t -> t
val ocaml_name : t -> string list -> string

(** [file_name t path] is the filename of the proto file where the
    identifier at [path] is defined. *)
val file_name : t -> string list -> string

(** [package t path] is the package component of [path]. *)
val package : t -> string list -> string list

val length : t -> int
val is_cyclic : t -> string list -> bool
val dump : t -> out_channel -> unit
