open Spec.Descriptor.Google.Protobuf

include module type of Type_intf

val of_fd : FileDescriptorProto.t -> t
