open StdLabels
open Spec.Descriptor.Google.Protobuf

include Type_intf

(* Original cycle detection algo is:
   - Only look at messages that have only one children, apparently?
   - Stop when you don't see anything new anymore?
*)

(*
   A message is cyclic if, from exploring a child, one can see the message.
*)

let pp_path ppf t =
  Format.pp_print_string ppf (String.concat ~sep:"." t)

let rec find t path =
  match path with
  | [] -> t
  | h :: tl ->
    let ch = List.find t.chs ~f:(fun ch -> String.equal ch.name h) in
    find ch tl

(* This will return the set of nodes accessible from path *)
let accessible_from_path t path =
  let x = find t path in
  let tbl = Hashtbl.create 13 in
  let rec inner x =
    Hashtbl.add tbl x () ;
    List.iter x.chs ~f:(fun x ->
      if not (Hashtbl.mem tbl x) then
        inner x)
  in
  inner x ;
  (* Format.fprintf !Base.debug' *)
  (*   "ACCESSIBLE: %d nodes are accesible from %a@." *)
  (*   (Hashtbl.length tbl) pp_path path ; *)
  tbl

(* This function is only called of messages! *)
let rec is_cyclic torig acc (t:t) path =
  (* Printf.fprintf !Base.debug "is_cyclic %s\n" (String.concat ~sep:"." path); *)
  match path with
  | [] ->
    (* Format.fprintf !Base.debug' "is_cyclic : %a\n" pp_kind t.kind ; *)
    (match t.kind with
     | Message ->
       (* For all children, check if they points to an already seen structure?? *)
       List.fold_left ~init:false t.chs ~f:(fun a x ->
         let seen =
           match x.kind with
           | Field { type_name = Some x} ->
             let path = String.split_on_char ~sep:'.' x |> List.tl in
             let tbl = accessible_from_path torig path in
             (* If current is accessible from children path, there is a loop. *)
             let found = Hashtbl.mem tbl t in
             if found then Format.fprintf !Base.debug'
               "CYCLE: %d nodes are reachable from %a@."
               (Hashtbl.length tbl) pp_path path ;
             found
           | UnionField { type_name = Some x } ->
             let path = String.split_on_char ~sep:'.' x |> List.tl in
             let tbl = accessible_from_path torig path in
             (* If current is accessible from children path, there is a loop. *)
             let found = Hashtbl.mem tbl t in
             if found then
             Format.fprintf !Base.debug'
               "CYCLE: %d nodes are reachable from %a@."
               (Hashtbl.length tbl) pp_path path ;
             found
           | _ -> false in
         a || seen)
     | _ -> false
    )
  | h :: tl ->
    let ch = List.find t.chs ~f:(fun ch -> String.equal ch.name h) in
    is_cyclic torig (t::acc) ch tl

let is_cyclic t path = is_cyclic t [] t (List.rev path)

let package file name = { file; name; chs = []; kind = Package }

let rec add_package ?(extension=false) t pkg k = match pkg with
  | [] -> k t
  | seg :: rest ->
    let chs, modified =
      List.fold_left t.chs ~init:([], false) ~f:(fun (xs, modified) x ->
        match x with
        | { name; _ } when String.equal name seg ->
          (match x.kind, extension with
           | Package, _ -> ()
           | Message, _ -> ()
           | _, false ->
             (* Not a package, not extending anything, do not allow
                package to overload a previously defined thing. *)
             Format.kasprintf failwith "Trying to erase a %a (%s) with a package segment" pp_kind x.kind name
           | _ -> ());
          (add_package x rest k :: xs, true)
        | _ -> (x::xs, modified)) in
    if modified then { t with chs }
    else { t with chs = add_package (package t.file seg) rest k :: chs }

(* Add one enum *)
let of_enum file (x : EnumDescriptorProto.t) =
    let fields = List.map x.value ~f:(fun (x : EnumValueDescriptorProto.t)  ->
      enum_value file (Option.get x.name)) in
    enum file (Option.get x.name) fields

(* Add one field *)
let of_field file (x : FieldDescriptorProto.t) =
  (* Depending on oneof_index, determine type field or union.
     TODO: proto3 optional fields!
  *)
  match x.oneof_index, x.proto3_optional with
  | None, _
  | Some _, Some true -> field file (Option.get x.name) x.type_name
  | _ -> union file (Option.get x.name) x.type_name

(* Add one oneof *)
let of_oneof file (x : OneofDescriptorProto.t) =
    oneof file (Option.get x.name)

(* TODO: add more stuff? *)
let rec of_message file (x : DescriptorProto.t) =
  let nested = List.map x.nested_type ~f:(of_message file) in
  let enums = List.map x.enum_type ~f:(of_enum file) in
  let fields = List.map x.field ~f:(of_field file) in
  let oneofs = List.map x.oneof_decl ~f:(of_oneof file) in
  message file (Option.get x.name) (nested @ fields @ oneofs @ enums)

let of_service file (x : ServiceDescriptorProto.t) =
  service file (Option.get x.name) []

(* Protobuf FQN is prefixed by . to denote absolute path. *)
let of_extension_field file (x: FieldDescriptorProto.t) =
  (* FQN of extendee I guess! Just register it as an extendee field. *)
  (* FQN is prefixed by dot to denote absolute path. *)
  let extendee = Option.get x.extendee in
  (* Printf.fprintf !Base.debug "extendee: %s\n" extendee ; *)
  let extendee = String.split_on_char ~sep:'.' extendee |> List.tl in
  extendee, extension_field file (Option.get x.name)

let add_fd t (fd : FileDescriptorProto.t) =
  (* File is the proto file path relative to the proto_path directory. *)
  let file = Option.get fd.name in
  (* Printf.fprintf !Base.debug "# Adding FD %s\n" file ; *)
  let package = Option.fold fd.package ~none:[] ~some:(String.split_on_char ~sep:'.') in
  let t =
  add_package t package (fun t ->
    (* t is the root after adding package segs. *)
    (* Add enums *)
    let enums = List.rev_map ~f:(of_enum file) fd.enum_type in
    let messages = List.rev_map ~f:(of_message file) fd.message_type in
    let services = List.rev_map ~f:(of_service file) fd.service in
    let open List in
    let chs =
      rev_append services @@
      rev_append enums @@
      rev_append messages t.chs
      (* rev_append extensions *)
    in
    { t with chs } ) in
  (* Process extensions at the end because it can be in another package. *)
  let extensions = List.map ~f:(of_extension_field file) fd.extension in
  List.fold_left extensions ~init:t ~f:(fun a (pkg, ext) ->
    add_package ~extension:true a pkg (fun t ->
      { t with chs = (ext :: t.chs) }))

let pathologic = ['_']

exception Starts_at of int

let first_non_pathologic s =
  try
    String.iteri s ~f:(fun i c ->
      if not (List.mem ~set:pathologic c) then raise (Starts_at i));
    String.length s
  with Starts_at i -> i

let strip_and_capitalize name =
  let pos = first_non_pathologic name in
  let len = String.length name - pos in
  let name = String.sub name ~pos ~len in
  let capitalized = Char.(equal name.[0] (uppercase_ascii name.[0])) in
  String.capitalize_ascii name ^ String.make ( pos + if capitalized then 0 else 1) '_'

let uncapitalize s =
  let capitalized = Char.(equal s.[0] (uppercase_ascii s.[0])) in
  let underscore_last = Char.equal s.[String.length s - 1] '_' in
  let score = List.fold_left ~init:0
                ~f:(fun a x -> if x then succ a else a) [capitalized; underscore_last] in
  let trailing = String.make score '_' in
  String.uncapitalize_ascii s ^  trailing

let name kind segs =
  match kind with
  | Field _ ->
    (* Fields can be any case, protobuf does not restrict that
       apparently? Can be at least capitalized! *)
    List.hd segs |> uncapitalize |> Names.escape_reserved
  | ExtensionField ->
    (* Check that name is available!! *)
    "Extended__" ^  List.hd (segs)
  | Oneof ->
    (* Generate names of make functions, etc. *)
    List.hd segs |> uncapitalize
  | UnionField _ ->
    (* Polyvariants MUST be capitalized, even if people do not respect that. *)
    "`" ^ String.capitalize_ascii (List.hd segs)
  | EnumValue ->
    (* Needs to be capitalized in OCaml, protobuf allow lowercase
       enums apparently. *)
    List.hd segs |> String.capitalize_ascii
  | Message | Enum | Service ->
    (* Need to strip possible pathologic first characters. *)
    strip_and_capitalize (List.hd segs)

  (* This case is different and returns concatened all segs and not just hd... why?? *)
  | Package | Extension ->
    (* Check. Remove dashes *)
    let segs = List.rev_map segs ~f:(fun x ->
      (* assert (String.index_opt x '-' |> Option.is_none) ; *)
      String.capitalize_ascii x) in
    String.concat ~sep:"" segs
  | kind ->
    Format.kasprintf failwith "not supported %a" pp_kind kind

(* takes the path in reverse order *)
let rec ocaml_name a t path =
  (* Printf.fprintf !Base.debug "ocaml_name: %s | %s\n" *)
  (*   (String.concat ~sep:"." a) (String.concat ~sep:"." (List.rev path)) ; *)
  match path with
  | [] -> name t.kind a
  | h :: ts ->
    match List.find_opt t.chs ~f:(fun ch -> String.equal ch.name h) with
    | None -> Format.kasprintf failwith "Cannot find component %s in <%s>" h (String.concat ~sep:"." a)
    | Some v ->
      ocaml_name (h::a) v ts

let ocaml_name t path = ocaml_name [] t (List.rev path)

(* takes the path in reverse order *)
let rec file_name a t path =
  match path with
  | [] -> t.file
  | h :: ts ->
    let v = List.find t.chs ~f:(fun ch -> String.equal ch.name h) in
    file_name (h::a) v ts

let file_name t path = file_name [] t (List.rev path)

(* takes the path in reverse order *)
let rec package a t path =
  match path with
  | [] -> List.rev a
  | h :: ts ->
    match List.find_opt t.chs ~f:(fun ch -> String.equal ch.name h) with
    | None -> Format.kasprintf failwith "Cannot find component %s in <%s>" h (String.concat ~sep:"." a)
    | Some v ->
      match v.kind with
      | Package ->
        package (h::a) v ts
      | _ ->
        package a v ts


let package t path = package [] t (List.rev path)

(* total number of nodes (including the root) *)
let rec length t =
  List.fold_left t.chs ~init:1 ~f:(fun a x -> a + length x)

let dump_one oc pfx { file; name; kind; _ } =
  let fqn = name :: pfx |> List.rev |> String.concat ~sep:"." in
  let ppf = Format.formatter_of_out_channel oc in
  Format.fprintf ppf "%s (%a) @@ %s@." fqn pp_kind kind file

let rec dump oc pfx t =
  dump_one oc pfx t ;
  let pfx = t.name :: pfx in
  List.iter t.chs ~f:(dump oc pfx)

let dump t oc = dump oc [] t

