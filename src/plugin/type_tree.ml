open StdLabels
open MoreLabels
open Spec.Descriptor.Google.Protobuf
open Base

(* *)
type t = (string list, Type.t) Hashtbl.t

(* let is_cyclic map fqn = *)
(*   let rec inner fqn seen = *)
(*     match StringListMap.find_opt fqn map with *)
(*     | Some depends when StringSet.cardinal depends = 1 -> *)
(*       let unseen = StringSet.diff depends seen in *)
(*       let seen = StringSet.union depends seen in *)
(*       StringSet.fold unseen ~init:seen ~f:inner *)
(*     | _ -> *)
(*       (\* If a type has more than one depend, then the cyclic chain is broken, and *)
(*          we can stop processing further *\) *)
(*       seen *)
(*   in *)
(*   let seen = inner fqn StringSet.empty in *)
(*   StringSet.mem fqn seen *)

(* let create_cyclic_map types = *)
(*   let map = Type.to_names types in *)
(*   StringListMap.mapi ~f:(fun fqn _ -> is_cyclic map fqn) map *)

let rec uniq_name names ocaml_name =
  match List.assoc_opt ocaml_name names with
  | None -> ocaml_name
  | Some _ -> uniq_name names (ocaml_name ^ "'")

(** Create a map: proto_name -> ocaml_name.
    Mapping is done in multiple passes to prioritize which mapping wins in case of name clashes *)
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
      ~data:({ file_name; ocaml_name = ocaml_name @ [data] })
      map
  )

let rec map_type file_name ~mangle_f ~map ~name_map path (t:Type.t) =
  let ocaml_name =
    let ocaml_name = StringMap.find t.name name_map in
    match StringMap.find path map with
    | { ocaml_name = []; _ } -> ocaml_name
    | { ocaml_name = path; _ } -> ocaml_name @ path
  in
  let path = path ^ "." ^ t.name in
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
  let map = StringMap.add_uniq ~key:path ~data:{ file_name; ocaml_name } map in
  traverse_types file_name ~mangle_f map path t.types

(** Add all types + all dependencies of those types *)
and traverse_types file_name ~mangle_f map path types =
  let name_map =
    List.map ~f:(fun { Type.name; _ } -> name) types |>
    create_name_map
      ~standard_f:(Names.module_name)
      ~mangle_f:(Names.module_name ~mangle_f)
  in
  List.fold_left types ~init:map ~f:(fun map type_ ->
    map_type file_name ~mangle_f ~map ~name_map path type_)

(** Create a type db: map proto-type -> { module_name, ocaml_name, is_cyclic } *)
let create_file_db ~mangle file_name types =
  let mangle_f = match mangle with
    | true -> Names.to_snake_case
    | false -> fun x -> x
  in
  traverse_types file_name ~mangle_f StringMap.empty "" types

let option_mangle_names FileDescriptorProto.{ options; _ } =
  Option.map Spec.Options.Ocaml_options.get options
  |> function
  | Some (Ok (Some v)) -> v
  | Some (Ok None) -> false
  | None -> false
  | Some (Error _e) -> failwith "Could not parse ocaml-protoc-plugin options with id 1074"

let file_db_of_proto proto_file =
  let types = Type.of_fd proto_file in
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
