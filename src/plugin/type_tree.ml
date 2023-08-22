open StdLabels
open MoreLabels
open Spec.Descriptor.Google.Protobuf

module StringSet = Set.Make(String)
module StringMap = struct
  include Map.Make(String)

  (** Fail with an error if the key already exists *)
  let add_uniq ~key ~data map =
    update ~key ~f:(function
      | None -> Some data
      | Some _ -> failwith (Printf.sprintf "Key %s already exists" key)
    ) map
end

let module_name_of_proto file =
  Filename.chop_extension file
  |> Filename.basename
  |> String.capitalize_ascii
  |> String.map ~f:(function '-' -> '_' | c -> c)

module Type = struct
  (* Name is definitely relative, here. *)
  type t = {
    name: string;
    types: t list;
    depends: string list;
    fields: string list * string list list;
    enum_names: string list;
    service_names: string list
  }

  let create ?(types=[]) ?(depends=[]) ?(fields=([], [])) ?(enum_names=[]) ?(service_names=[]) name =
    { name; types; depends; fields; enum_names; service_names }

  let of_enum EnumDescriptorProto.{ name; value = values; _ } =
    let name = Option.get name in
    let enum_names =
      List.map ~f:(fun EnumValueDescriptorProto.{ name; _ } -> Option.get name) values
    in
    create ~enum_names name

  let of_service ServiceDescriptorProto.{ name; method' = methods; _ } =
  let name = Option.get name in
  let service_names =
    List.map ~f:(fun MethodDescriptorProto.{ name; _ } -> Option.get name) methods
  in
  create ~service_names name

  let of_extension FieldDescriptorProto.{ name; _ } =
    let name = Option.get name in
    create name

  let rec traverse path map { name; types; depends; _ } =
    let path = path ^ "." ^ name in
    let map = StringMap.add ~key:path ~data:(StringSet.of_list depends) map in
    List.fold_left ~init:map ~f:(traverse path) types
end

type element = {
  file_name: string;
  ocaml_name: string list;
  cyclic: bool
}

type t = element StringMap.t

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

let rec map_message DescriptorProto.{ name; field = fields; nested_type = nested_types; enum_type = enums; oneof_decl = oneof_decls; extension = extensions; _} =
  let name = Option.value ~default:"All messages must have a name" name in
  let depends =
    List.fold_left ~init:[] ~f:(fun acc -> function
      | FieldDescriptorProto.{ type_name = Some type_name; type' = Some Type.TYPE_MESSAGE; _ } ->
        type_name :: acc
      | FieldDescriptorProto.{ type_name = Some type_name; type' = Some Type.TYPE_ENUM; _ } ->
        type_name :: acc
      | _ -> acc
    ) fields
  in
  let enums = List.map ~f:Type.of_enum enums in
  let extensions = List.map ~f:Type.of_extension extensions in
  let nested_types = List.map ~f:map_message nested_types in
  let types = List.sort ~cmp:compare (enums @ extensions @ nested_types) in
  let fields =
    let field_name FieldDescriptorProto.{ name; _} =
      Option.value ~default:"Field names cannot be null" name
    in
    let (plain_fields, oneof_fields) = List.partition ~f:(function FieldDescriptorProto.{ proto3_optional = Some true; _ } -> true
                                                                 | { oneof_index = None; _ } -> true
                                                                 | _ -> false) fields in
    let plain_fields =
      let acc = List.map ~f:field_name plain_fields in
      List.fold_left ~init:acc ~f:(fun acc OneofDescriptorProto.{ name; _ } ->
        (Option.value ~default:"Oneof names cannot be null" name) :: acc
      ) oneof_decls
    in
    let oneof_fields =
      split_oneof_fields oneof_fields
      |> List.map ~f:(List.map ~f:field_name)
    in
    plain_fields, oneof_fields
  in
  Type.create name ~types ~depends ~fields

let types_of_fd (t:FileDescriptorProto.t) =
  let messages = List.map ~f:map_message t.message_type in
  let enums = List.map ~f:Type.of_enum t.enum_type in
  let services = List.map ~f:Type.of_service t.service in
  let extensions = List.map ~f:Type.of_extension t.extension in
  let types = enums @ messages @ services @ extensions in
  let packages = Option.fold ~none:[] ~some:(String.split_on_char ~sep:'.') t.package in
  let types = List.fold_right packages ~init:types ~f:(fun name types -> [Type.create ~types name]) in
  types

let create_cyclic_map types =
  let is_cyclic map name =
    let rec inner name seen =
      (* If a type has more than one depend, then the cyclic chain is broken, and
         we can stop processing further *)
      match StringMap.find_opt name map with
      | None -> seen
      | Some depends when StringSet.cardinal depends = 1 ->
        let unseen = StringSet.diff depends seen in
        let seen = StringSet.union depends seen in
        StringSet.fold ~init:seen ~f:inner unseen
      | Some _ -> seen
    in
    let seen = inner name StringSet.empty in
    StringSet.mem name seen
  in
  let map = List.fold_left ~init:StringMap.empty ~f:(Type.traverse "") types in
  StringMap.mapi ~f:(fun name _ -> is_cyclic map name) map

(** Create a map: proto_name -> ocaml_name.
    Mapping is done in multiple passes to prioritize which mapping wins in case of name clashes
*)
let rec uniq_name names ocaml_name =
  match List.assoc_opt ocaml_name names with
  | None -> ocaml_name
  | Some _ -> uniq_name names (ocaml_name ^ "'")

let create_name_map ~standard_f ~mangle_f names =
  let names =
    List.map ~f:(fun name ->
      let mangle_name = mangle_f name in
      let standard_name = standard_f name in
      (name, mangle_name, standard_name)
    ) names
  in
  let standard_name_map =
    let inject ~f map =
      List.fold_left ~init:map ~f:(fun map (name, mangled_name, standard_name) ->
        match f name mangled_name standard_name with
        | true when StringMap.mem mangled_name map -> map
        | true -> StringMap.add ~key:mangled_name ~data:name map
        | false -> map
      ) names
    in
    StringMap.empty
    |> inject ~f:(fun name mangled_name _standard_name -> String.equal mangled_name name)
    |> inject ~f:(fun _name mangled_name standard_name -> String.equal mangled_name standard_name)
    |> inject ~f:(fun name mangled_name _standard_name -> String.equal (String.lowercase_ascii mangled_name) (String.lowercase_ascii name))
    |> inject ~f:(fun _name mangled_name standard_name -> String.equal (String.lowercase_ascii mangled_name) (String.lowercase_ascii standard_name))
  in
  List.fold_left names ~init:[] ~f:(fun names (proto_name, ocaml_name, _) ->
    let ocaml_name =
      match StringMap.find_opt ocaml_name standard_name_map with
      | Some name when String.equal name proto_name -> ocaml_name
      | Some _ -> ocaml_name ^ "'"
      | None -> ocaml_name
    in
    (uniq_name names ocaml_name, proto_name) :: names
  )
  |> List.fold_left ~init:StringMap.empty ~f:(fun map (ocaml_name, proto_name) ->
    StringMap.add ~key:proto_name ~data:ocaml_name map
  )

(* Build an element map from a string map where string is the name of
   the protobuf thing. *)
let add_names ~file_name ~path ~ocaml_name t names =
  StringMap.fold names ~init:t ~f:(fun ~key ~data map ->
    StringMap.add_uniq
      ~key:(path ^ "." ^ key)
      ~data:({ file_name; ocaml_name = ocaml_name @ [data]; cyclic = false })
      map
  )

let rec map_type file_name cyclic_map ~mangle_f ~map ~name_map path (t:Type.t) =
  let ocaml_name =
    let ocaml_name = StringMap.find t.name name_map in
    match StringMap.find path map with
    | { ocaml_name = []; _ } -> ocaml_name
    | { ocaml_name = path; _ } -> ocaml_name @ path
  in
  let path = path ^ "." ^ t.name in
  let cyclic = StringMap.find path cyclic_map in
  let map =
    create_name_map
      ~standard_f:(Names.field_name ~mangle_f:(fun x -> x))
      ~mangle_f:(Names.field_name ~mangle_f)
      (fst t.fields)
    |> add_names ~file_name ~path ~ocaml_name map
  in
  let map =
    List.fold_left ~init:map ~f:(fun map fields ->
      create_name_map
        ~standard_f:(Names.poly_constructor_name ~mangle_f:(fun x -> x))
        ~mangle_f:(Names.poly_constructor_name ~mangle_f)
        fields
      |> add_names ~file_name ~path ~ocaml_name map
    ) (snd t.fields)
  in
  let map =
    create_name_map
      ~standard_f:(Names.module_name ~mangle_f:(fun x -> x))
      ~mangle_f:(Names.module_name ~mangle_f)
      t.enum_names
    |> add_names ~file_name ~path ~ocaml_name map
  in

  let map =
    create_name_map
      ~standard_f:(Names.field_name ~mangle_f:(fun x -> x))
      ~mangle_f:(Names.field_name ~mangle_f)
      t.service_names
    |> add_names ~file_name ~path ~ocaml_name map
  in
  let map = StringMap.add_uniq ~key:path ~data:{ file_name; ocaml_name; cyclic } map in
  traverse_types file_name cyclic_map ~mangle_f map path t.types

(** Add all types + all dependencies of those types *)
and traverse_types file_name cyclic_map ~mangle_f map path types =
  let name_map =
    List.map ~f:(fun { Type.name; _ } -> name) types |>
    create_name_map
      ~standard_f:(Names.module_name)
      ~mangle_f:(Names.module_name ~mangle_f)
  in
  List.fold_left types ~init:map ~f:(fun map type_ ->
    map_type file_name cyclic_map ~mangle_f ~map ~name_map path type_)

(** Create a type db: map proto-type -> { module_name, ocaml_name, is_cyclic } *)
let create_file_db ~mangle cyclic_map file_name types =
  let mangle_f = match mangle with
    | true -> Names.to_snake_case
    | false -> fun x -> x
  in
  traverse_types file_name cyclic_map ~mangle_f StringMap.empty "" types

let option_mangle_names FileDescriptorProto.{ options; _ } =
  Option.map Spec.Options.Ocaml_options.get options
  |> function
  | Some (Ok (Some v)) -> v
  | Some (Ok None) -> false
  | None -> false
  | Some (Error _e) -> failwith "Could not parse ocaml-protoc-plugin options with id 1074"

let file_db_of_proto proto_file =
  let types = types_of_fd proto_file in
  let name = Option.get proto_file.name in
  let cyclic_map = create_cyclic_map types in
  create_file_db ~mangle:(option_mangle_names proto_file) cyclic_map name types

let create files =
  List.fold_left files ~init:StringMap.empty ~f:(fun a x ->
    StringMap.merge ~f:(fun _ a -> function
      | None -> a
      | b -> b
    ) a (file_db_of_proto x)
  )

let dump t =
  Printf.eprintf "Type map:\n";
  StringMap.iter t ~f:(fun ~key ~data:{file_name; ocaml_name; cyclic; _ } ->
    let module_name = String.concat ~sep:"." ocaml_name in
    Printf.eprintf "     %S -> %S#%S, C:%b\n" key file_name module_name cyclic
  ) ;
  Printf.eprintf "Type map end:\n%!"
