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

(* Type of a scope.  *)
type t = {
  name: string; (* Protobuf file name. *)
  path: string list ; (* package in reverse order. *)
  package_length: int ; (* length of package name *)
  type_db: Type.t
}

let replace_path t path = { t with path = List.rev path }

let pp ppf t =
  let open Format in
  fprintf ppf "file:%s package:%a" t.name (pp_print_list ~pp_sep:(fun ppf () -> pp_print_char ppf '.') pp_print_string) t.path


let create FileDescriptorProto.{ name; package; _ } type_db =
  let name = Option.get name in
  let path =
    Option.fold ~none:[] ~some:(String.split_on_char ~sep:'.') package |> List.rev in
  let package_length = List.length path in
  { name; package_length; path ; type_db }

let push t name = { t with path = name :: t.path }

(* proto_name is raw FQN protobuf name (normal order). *)
let get_scoped_name ?postfix t proto_name =
  (* Printf.fprintf !Base.debug "get_scoped_name %s\n" proto_name ; *)
  let name = String.split_on_char ~sep:'.' proto_name in
  let name = match name with
    | "" :: tl ->
      (* Sometimes names with empty first prefix are pushed here! *)
      List.rev tl
    | _ -> List.rev name
  in
  let ocaml_name = Type.ocaml_name t.type_db name in
  let file_name = Type.file_name t.type_db name in
  (* Strip away the package depth *)
  let type_name = match String.equal file_name t.name with
    | true ->
      (* Local name *)
      ocaml_name
    | false ->
      (* Remote name, prefix with file *)
      Filename.(basename file_name|> chop_extension |> String.capitalize_ascii) ^ "." ^ ocaml_name
  in
  Option.fold postfix ~none:type_name ~some:(fun postfix -> type_name ^ "." ^ postfix)

(* Relative name, last element? *)
let get_name t name =
  (* Look for name in full path! *)
  let path = name :: t.path in
  match Type.ocaml_name t.type_db path with
  | exception exn ->
    Type.dump t.type_db !Base.debug ;
    Printf.fprintf !Base.debug "# %s\n" (Printexc.to_string exn) ;
    let path = String.concat ~sep:"." (List.rev t.path) in
    failwith (Printf.sprintf "Cannot find %s in <%s> (DB has %d entries)"
                name path (Type.length t.type_db) )
  | ocaml_name ->
    (* Get last element. *)
    ocaml_name

let get_name_exn t name = get_name t (Option.get name)

(* Dunno what this is for!! *)
let get_current_scope t =
  let file_name = Type.file_name t.type_db t.path in
  let __ =(String.lowercase_ascii file_name) :: List.rev t.path in
  (* TODO: FIX!! *)
  ""

let drop n xs =
  assert (n <= List.length xs);
  let rec inner n xs = if n <= 0 then xs
    else inner (pred n) (List.tl xs)
  in inner n xs

let get_package_name { path; package_length; _ } =
  let len = List.length path in
  let package = drop (len - package_length) path in
  List.rev package


let is_cyclic t =  Type.is_cyclic t.type_db t.path

