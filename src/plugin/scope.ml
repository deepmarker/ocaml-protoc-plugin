open StdLabels
open MoreLabels

(** Module to avoid name clashes in a local scope *)
module Local = struct
  type t = (string, unit) Hashtbl.t
  let create () : t = Hashtbl.create 2
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
  name: string; (* Protobuf file name. *)
  package: string list ; (* package in normal order *)
  proto_path: string list ; (* path component in reverse order. *)
  type_db: Type_tree.t
}

let create FileDescriptorProto.{ name; package; _ } type_db =
  let name = Option.get name in
  let package =
    Option.fold ~none:[] ~some:(String.split_on_char ~sep:'.') package in
  { name; package; proto_path = []; type_db }

let push t name = { t with proto_path = name :: t.proto_path }

let rec drop n = function
  | [] -> []
  | _ :: xs when n > 0 -> drop (n - 1) xs
  | xs -> xs

(* proto_name is raw FQN protobuf name (normal order). *)
let get_scoped_name ?postfix t proto_name =
  let name = String.split_on_char ~sep:'.' proto_name in
  let ocaml_name = Type_tree.ocaml_name t.type_db name in
  let file_name = Type_tree.file_name t.type_db name in
  (* Strip away the package depth *)
  let type_name = match String.equal file_name t.name with
    | true ->
      (* Local name *)
      ocaml_name
      |> drop (List.length t.package)
      |> String.concat ~sep:"."
    | false ->
      (* Remote name: check this is correct. *)
      String.concat ~sep:"." (file_name :: ocaml_name)
  in
  Option.fold postfix ~none:type_name ~some:(fun postfix -> type_name ^ "." ^ postfix)

(* Relative name, last element? *)
let get_name t name =
  let path = name :: t.proto_path in
  match Type_tree.ocaml_name t.type_db path with
  | exception _ ->
    failwith (Printf.sprintf "Cannot find %s in %s." name (String.concat ~sep:"." t.proto_path))
  | ocaml_name ->
    (* Get last element. *)
    List.rev ocaml_name |> List.hd

let get_name_exn t name = get_name t (Option.get name)

(* Dunno what this is for!! *)
let get_current_scope t =
  let file_name = Type_tree.file_name t.type_db t.proto_path in
  let __ =(String.lowercase_ascii file_name) :: List.rev t.proto_path in
  (* TODO: FIX!! *)
  ""

let get_package_name { proto_path; _ } =
  match proto_path with
  | [] -> None
  | _ :: xs -> List.rev xs |> String.concat ~sep:"." |> Option.some

let is_cyclic t =  Type_tree.is_cyclic t.type_db t.proto_path

