open Spec.Descriptor.Google.Protobuf

type t = {
  type' : string;
  constructor: string;
  apply: string;
  deserialize_spec: string;
  serialize_spec: string;
  default_constructor_sig: string;
  default_constructor_impl: string;
}

type syntax = [`Proto2 | `Proto3]

val create:
  params:Parameters.t ->
  syntax:[< syntax ] ->
  is_cyclic: bool ->
  is_map_entry: bool ->
  has_extensions: bool ->
  scope:Scope.t ->
  fields:FieldDescriptorProto.t list -> OneofDescriptorProto.t list -> t
