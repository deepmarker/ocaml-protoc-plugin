open StdLabels
open MoreLabels

let drop n xs =
  assert (n <= List.length xs);
  let rec inner n xs = if n <= 0 then xs
    else inner (pred n) (List.tl xs)
  in inner n xs

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
  proto_file_name: string; (* Protobuf file name. *)
  path: string list ; (* package in reverse order. *)
  package_length: int ; (* length of package name *)
  type_db: Type.t
}

let replace_path t path = { t with path = List.rev path }

let pp ppf t =
  let open Format in
  fprintf ppf "file:%s package:%a" t.proto_file_name
    (pp_print_list ~pp_sep:(fun ppf () -> pp_print_char ppf '.') pp_print_string) t.path


let create FileDescriptorProto.{ name; package; _ } type_db =
  let proto_file_name = Option.get name in
  let path =
    Option.fold ~none:[] ~some:(String.split_on_char ~sep:'.') package |> List.rev in
  let package_length = List.length path in
  { proto_file_name; package_length; path ; type_db }

let push t name = { t with path = name :: t.path }

let sanitize s =
  let b = Bytes.of_string s in
  Bytes.iteri b ~f:(fun i x ->
    match x with
    | '-' -> Bytes.set b i '_'
    | _ ->
      (* TODO: Other things to cleanup? *)
      ()
  );
  Bytes.unsafe_to_string b

let prefix_len scope_path path =
  let rec inner common scope path =
    match scope, path with
    | [], _ | _, [] -> common
    | scope :: scopes, path:: paths ->
      if String.equal scope path then inner (succ common) scopes paths
      else common in
  inner 0 scope_path path

(* Need to return SCOPED name so this is wrong. *)
(* DO SCOPING. *)
let get_scoped_name ?postfix t proto_name =
  let name = String.split_on_char ~sep:'.' proto_name in
  (* Format.fprintf !Base.debug' "get_scoped_name %s (Scope: %a) (pfxlen: %d)\n" proto_name pp t pfxlen ; *)
  let name = match name with
    | "" :: tl -> tl
    (* Sometimes names with empty first prefix are pushed here! *)
    | _ -> name
  in

  (* THIS MUST STAY HERE / Sorry shit code. *)
  let pfxlen = prefix_len (List.rev t.path) name in
  (* THIS MUST STAY HERE *)

  let name_rev = List.rev name (* Reverse because the below functions
                              already reverse the name... *) in
  let ocaml_name = Type.ocaml_name t.type_db name_rev in
  let file_name = Type.file_name t.type_db name_rev in
  (* Strip away the package depth *)

  (* Goal is to drop the prefix and append the rest to the name. *)
  let local_name pfxlen =
    (try
       (* Format.fprintf !Base.debug' "get_scoped_name %s (Scope: %a) (pfxlen: %d)\n" proto_name pp t pfxlen ; *)
       let prefix = List.tl name_rev |> List.rev |> drop pfxlen |> List.map ~f:String.capitalize_ascii in
       let prefix = String.concat prefix ~sep:"" in
       match prefix with
       | "" -> ocaml_name
       | _ -> prefix ^ "." ^ ocaml_name
     with _ -> ocaml_name) in

  let type_name = match String.equal file_name t.proto_file_name with
    | true ->
      local_name pfxlen
    | false ->
      (* Remote name is composed of package plus basename of proto
         file name, concatened+capitalized. Replace unfriendly characters too! *)
      let file = Filename.(basename file_name |> chop_extension |> String.capitalize_ascii) in
      let file = sanitize file in
      let remote_pkg_name = Type.package t.type_db name_rev in
      let prefix = remote_pkg_name @ [file] |> List.map ~f:String.capitalize_ascii in
      let prefix = String.concat prefix ~sep:"" in
      (* Package name embedded in prefix. Now we need to add local name. *)
      let pfxlen = prefix_len name remote_pkg_name in
      prefix ^ "." ^ local_name pfxlen
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

let get_current_scope t = String.concat ~sep:"." (List.rev t.path)

let get_package_name { path; package_length; _ } =
  let len = List.length path in
  let package = drop (len - package_length) path in
  List.rev package


let is_cyclic t =  Type.is_cyclic t.type_db t.path

