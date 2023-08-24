open StdLabels
open MoreLabels
open Base

let dump_tree = false

(** Module to avoid name clashes in a local scope *)
module Local = struct
  type t = (string, unit) Hashtbl.t
  let init () : t = Hashtbl.create 2
  let get_unique_name t preferred_name =
    let rec inner name =
      match Hashtbl.mem t name with
      | true -> inner (name ^ "'")
      | false when Names.is_reserved name -> inner (name ^ "'")
      | false -> name

   in
   let name = inner preferred_name in
   Hashtbl.add t ~key:name ~data:();
   name
end

open Spec.Descriptor.Google.Protobuf

type t = {
  name: string;
  package_depth: int;
  proto_path: string;
  type_db: Type_tree.t
}

let init files =
  let type_db = Type_tree.create files in
  if dump_tree then Type_tree.dump type_db;
  { name = ""; proto_path = ""; package_depth = 0; type_db; }

let for_descriptor t FileDescriptorProto.{ name; package; _ } =
  let name = Option.get  name in
  let package_depth = Option.fold ~none:0 ~some:(fun p -> String.split_on_char ~sep:'.' p |> List.length) package in
  { t with package_depth; name; proto_path = "" }

let push t name = { t with proto_path = t.proto_path ^ "." ^ name }

let rec drop n = function
  | [] -> []
  | _ :: xs when n > 0 -> drop (n - 1) xs
  | xs -> xs

let get_scoped_name ?postfix t name =
  let open Type_tree in
  let { ocaml_name; file_name; _ } = StringMap.find name t.type_db in
  let type_name = match String.equal file_name t.name with
    | true ->
      ocaml_name
      |> drop t.package_depth
      |> String.concat ~sep:"."
    | false -> String.concat ~sep:"." (file_name :: ocaml_name)
  in
  (* Strip away the package depth *)
  Option.fold ~none:type_name ~some:(fun postfix -> type_name ^ "." ^ postfix) postfix

let get_name t name =
  let open Type_tree in
  let path = t.proto_path ^ "." ^ name in
  match StringMap.find_opt path t.type_db with
    | Some { ocaml_name; _ } -> List.rev ocaml_name |> List.hd
    | None -> failwith (Printf.sprintf "Cannot find %s in %s." name t.proto_path)

let get_name_exn t name =
  let name = Option.value ~default:"Does not contain a name" name in
  get_name t name

let get_current_scope t =
  let open Type_tree in
  let { file_name; _ } = StringMap.find t.proto_path t.type_db in
  (String.lowercase_ascii file_name) ^ t.proto_path

let get_package_name { proto_path; _ } =
  match String.split_on_char ~sep:'.' proto_path with
  | _ :: xs -> List.rev xs |> List.tl |> List.rev |> String.concat ~sep:"." |> Option.some
  | _ -> None

let is_cyclic t =
  let open Type_tree in
  let { cyclic; _ } = StringMap.find t.proto_path t.type_db in
  cyclic
