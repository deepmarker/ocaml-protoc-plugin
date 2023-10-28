open Core
module Any = GoogleProtobufAny.Any
module Api = GoogleProtobufApi
module Descriptor = GoogleProtobufDescriptor
module Duration = GoogleProtobufDuration.Duration
module Empty = GoogleProtobufEmpty.Empty
module Field_mask = GoogleProtobufField_mask.FieldMask
module Source_context = GoogleProtobufSource_context.SourceContext

module Struct = struct
  include GoogleProtobufStruct

  module type StructSig = module type of Struct
  module type ValueSig = module type of Value

  module rec Struct : StructSig = struct
    include Struct

    let yojson_of_t xs =
      `Assoc (List.map xs ~f:(fun (k, v) -> k, Value.yojson_of_t (Option.value_exn ~here:[%here] v)))

    let t_of_yojson = function
      | `Assoc xs -> List.map xs ~f:(fun (k, v) -> k, Some (Value.t_of_yojson v))
      | #Yojson.Safe.t -> assert false
  end
  and Value : ValueSig = struct
    include Value

    let rec yojson_of_t = function
      | `Bool_value b -> `Bool b
      | `Struct_value x -> Struct.yojson_of_t x
      | `String_value x  -> `String x
      | `not_set -> assert false
      | `Number_value x -> `Float x
      | `Null_value _ -> `Null
      | `List_value xs  -> `List (List.map ~f:yojson_of_t xs)

    let t_of_yojson = function
      | `Assoc xs -> `Struct_value (List.map ~f:(fun (k, v) -> k, Some (Value.t_of_yojson v)) xs)
      | `Bool x -> `Bool_value x
      | `Float x -> `Number_value x
      | `List xs -> `List_value (List.map ~f:t_of_yojson xs)
      | `Null -> `Null_value (NullValue.NULL_VALUE)
      | `String x -> `String_value x
      | #Yojson.Safe.t -> assert false
  end
end

module Timestamp = GoogleProtobufTimestamp.Timestamp
module Type = GoogleProtobufType
module Wrappers = GoogleProtobufWrappers
