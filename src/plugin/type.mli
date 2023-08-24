open Spec.Descriptor.Google.Protobuf
open Base

include module type of Type_intf

val of_fd : FileDescriptorProto.t -> t

(** [to_name t] is a map fqn -> deps where fqn is the reverse FQN list
    and deps are relative dependency names *)
val to_names: t -> StringSet.t StringListMap.t
