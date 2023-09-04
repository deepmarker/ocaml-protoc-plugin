open Spec.Descriptor.Google.Protobuf

include module type of Type_intf

val add_fd : t -> FileDescriptorProto.t -> t
val ocaml_name : t -> string list -> string
val file_name : t -> string list -> string
val length : t -> int
val is_cyclic : t -> string list -> bool
val dump : t -> out_channel -> unit
