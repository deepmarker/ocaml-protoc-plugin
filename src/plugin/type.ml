open StdLabels
open Spec.Descriptor.Google.Protobuf

include Type_intf

let enum name values = { name ; kind = Enum { values } }
let service name methods = {name ; kind = Service { methods } }
let extension name = { name ; kind = Extension }
let file file_name package contents = { file_name ; package; contents }

let of_enum EnumDescriptorProto.{ name; value = values; _ } =
  let name = Option.get name in
  let values =
    List.map ~f:(fun EnumValueDescriptorProto.{ name; _ } -> Option.get name) values
  in
  enum name values

let of_service ServiceDescriptorProto.{ name; method' = methods; _ } =
  let name = Option.get name in
  let meths =
    List.map ~f:(fun MethodDescriptorProto.{ name; _ } -> Option.get name) methods
  in
  service name meths

let of_extension FieldDescriptorProto.{ name; _ } = extension (Option.get name)

let split_oneof_fields fields =
  let rec group acc ~eq = function
    | [] when acc = [] -> []
    | [] -> [ List.rev acc ]
    | x1 :: (x2 :: _  as xs) when eq x1 x2 -> group (x1 :: acc) ~eq xs
    | x :: xs -> (List.rev (x :: acc)) :: group [] ~eq xs
  in
  let field_number_of_field = function
    | FieldDescriptorProto.{ oneof_index = None; _ } -> failwith "Only oneof fields here"
    | FieldDescriptorProto.{ oneof_index = Some number; _ } -> number
  in

  let fields = List.sort ~cmp:(fun a b -> compare (field_number_of_field a) (field_number_of_field b)) fields in
  group [] ~eq:(fun a b -> field_number_of_field a = field_number_of_field b) fields

let rec of_message DescriptorProto.{ name; field = fields; nested_type = nested_types; enum_type = enums; oneof_decl = oneof_decls; extension = extensions; _} =
  let name = Option.value ~default:"All messages must have a name" name in
  let depends =
    List.fold_left fields ~init:[] ~f:(fun acc -> function
      | FieldDescriptorProto.{ type_name = Some type_name; type' = Some Type.TYPE_MESSAGE; _ } ->
        type_name :: acc
      | FieldDescriptorProto.{ type_name = Some type_name; type' = Some Type.TYPE_ENUM; _ } ->
        type_name :: acc
      | _ -> acc) in
  let enums = List.map ~f:of_enum enums in
  let extensions = List.map ~f:of_extension extensions in
  let nested_types = List.map ~f:of_message nested_types in
  let types = List.sort ~cmp:compare (enums @ extensions @ nested_types) in

  let field_name FieldDescriptorProto.{ name; _} =
    Option.get name
  in
  let (plain_fields, oneof_fields) =
    List.partition
      ~f:(function FieldDescriptorProto.{ proto3_optional = Some true; _ } -> true
                 | { oneof_index = None; _ } -> true
                 | _ -> false) fields in
  let plain_fields =
    let acc = List.map ~f:field_name plain_fields in
    List.fold_left ~init:acc ~f:(fun acc OneofDescriptorProto.{ name; _ } ->
      (Option.get name) :: acc
    ) oneof_decls
  in
  let oneof_fields =
    split_oneof_fields oneof_fields
    |> List.map ~f:(List.map ~f:field_name)
  in
  { name; kind = Message { types; depends; plain_fields; oneof_fields } }

let of_fd (t : FileDescriptorProto.t) =
  let messages = List.map ~f:of_message t.message_type in
  let enums = List.map ~f:of_enum t.enum_type in
  let services = List.map ~f:of_service t.service in
  let extensions = List.map ~f:of_extension t.extension in
  let deps = enums @ messages @ services @ extensions in
  let name = Option.get t.name in
  let package = Option.fold t.package ~none:[] ~some:(String.split_on_char ~sep:'.') in
  file name package deps

open Base

let split =
  String.split_on_char ~sep:'.'

let add_to_path path name =
  List.fold_left name ~init:path ~f:(fun a x -> x :: a)

(* Produce a map fqn -> deps. All names are fqn. *)
(* acc is a string string map of path in reverse order! *)
let to_names ({ file_name = _; package; contents }: t) =
  let on_elt path a { name; kind } =
    let a, deps =
      match kind with
      | Extension ->
        (* We allow things to be extended multiple times I guess? *)
        let key = add_to_path path (split name) in
        StringListMap.add a ~key ~data:StringSet.empty, StringSet.empty
      | Service _
      | Enum _ ->
        StringListMap.update a ~key:(name :: path)
          ~f:(function None -> Some StringSet.empty | Some _ -> assert false), StringSet.empty
      | Message { types; depends; plain_fields = _; oneof_fields = _}  ->
        (* Add depends, these are absolute names *)
        let a = StringListMap.add a ~key:(add_to_path [] depends) ~data:StringSet.empty in
        a, List.fold_left types ~init:StringSet.empty ~f:(fun a x -> StringSet.add x.name a)
    in
    StringListMap.add a ~key:path ~data:deps
  in
  let path = List.rev package in
  List.fold_left contents ~f:(on_elt path) ~init:StringListMap.empty
