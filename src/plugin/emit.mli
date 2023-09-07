open Spec.Descriptor.Google.Protobuf

val parse_proto_file:
  params:Parameters.t -> Type.t -> FileDescriptorProto.t -> Code.t
