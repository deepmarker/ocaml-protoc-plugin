open StdLabels
open Spec.Descriptor.Google.Protobuf

include Type_intf

let package file name = { file; name; chs = []; kind = Package }

(* let of_service ServiceDescriptorProto.{ name; method' = methods; _ } = *)
(*   let name = Option.get name in *)
(*   let meths = *)
(*     List.map ~f:(fun MethodDescriptorProto.{ name; _ } -> Option.get name) methods *)
(*   in *)
(*   service name meths *)

(* let of_extension FieldDescriptorProto.{ name; _ } = extension (Option.get name) *)

(* let split_oneof_fields fields = *)
(*   let rec group acc ~eq = function *)
(*     | [] when acc = [] -> [] *)
(*     | [] -> [ List.rev acc ] *)
(*     | x1 :: (x2 :: _  as xs) when eq x1 x2 -> group (x1 :: acc) ~eq xs *)
(*     | x :: xs -> (List.rev (x :: acc)) :: group [] ~eq xs *)
(*   in *)
(*   let field_number_of_field = function *)
(*     | FieldDescriptorProto.{ oneof_index = None; _ } -> failwith "Only oneof fields here" *)
(*     | FieldDescriptorProto.{ oneof_index = Some number; _ } -> number *)
(*   in *)

(*   let fields = List.sort ~cmp:(fun a b -> compare (field_number_of_field a) (field_number_of_field b)) fields in *)
(*   group [] ~eq:(fun a b -> field_number_of_field a = field_number_of_field b) fields *)

(* let rec of_message DescriptorProto.{ name; field = fields; *)
(*                                      nested_type = nested_types; *)
(*                                      enum_type ; *)
(*                                      oneof_decl = oneof_decls; _} = *)
(*   let name = Option.get name in *)
(*   (\* Fields can be messages or enums... ok... *\) *)
(*   let depends = *)
(*     List.fold_left fields ~init:[] ~f:(fun acc -> function *)
(*       | FieldDescriptorProto.{ type_name = Some type_name; type' = Some Type.TYPE_MESSAGE; _ } -> *)
(*         type_name :: acc *)
(*       | FieldDescriptorProto.{ type_name = Some type_name; type' = Some Type.TYPE_ENUM; _ } -> *)
(*         type_name :: acc *)
(*       | _ -> acc) in *)
(*   (\* Inner types can be enums too! *\) *)
(*   let nested_types = List.map nested_types ~f:of_message in *)
(*   let enum_types = List.map enum_type ~f:of_enum in *)
(*   let nested = List.sort ~cmp:compare (List.rev_append enum_types nested_types) in *)

(*   let field_name FieldDescriptorProto.{ name; _} = *)
(*     Option.get name *)
(*   in *)
(*   let (plain_fields, oneof_fields) = *)
(*     List.partition *)
(*       ~f:(function FieldDescriptorProto.{ proto3_optional = Some true; _ } -> true *)
(*                  | { oneof_index = None; _ } -> true *)
(*                  | _ -> false) fields in *)
(*   let plain_fields = *)
(*     let acc = List.map ~f:field_name plain_fields in *)
(*     List.fold_left ~init:acc ~f:(fun acc OneofDescriptorProto.{ name; _ } -> *)
(*       (Option.get name) :: acc *)
(*     ) oneof_decls *)
(*   in *)
(*   let oneof_fields = *)
(*     split_oneof_fields oneof_fields *)
(*     |> List.map ~f:(List.map ~f:field_name) *)
(*   in *)
(*   { name; kind = Message { Type_intf.nested; depends; plain_fields; oneof_fields } } *)

let rec add_package ?(extension=false) t pkg k = match pkg with
  | [] -> k t
  | seg :: rest ->
    let chs, modified =
      List.fold_left t.chs ~init:([], false) ~f:(fun (xs, modified) x ->
        match x with
        | { name; _ } when String.equal name seg ->
          (match x.kind, extension with
           | Package, _ -> ()
           | _, false ->
             (* Not a package, not extending anything, do not allow
                package to overload a previously defined thing. *)
             assert false
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
  | Some _, Some true -> field file (Option.get x.name)
  | _ -> union file (Option.get x.name)

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
  Printf.fprintf !Base.debug "extendee: %s\n" extendee ;
  let extendee = String.split_on_char ~sep:'.' extendee |> List.tl in
  extendee, extension_field file (Option.get x.name)

let add_fd t (fd : FileDescriptorProto.t) =
  let file = Option.get fd.name in
  Printf.fprintf !Base.debug "# Adding FD %s\n" file ;
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

let name kind segs =
  match kind with
  | Field ->
    Names.escape_reserved (List.hd segs)
  | Oneof ->
    (* Generate names of make functions, etc. *)
    List.hd segs
  | UnionField ->
    (* Polyvariants MUST be capitalized, even if people do not respect that. *)
    "`" ^ String.capitalize_ascii (List.hd segs)
  | EnumValue ->
    List.hd segs
  | Message | Enum | Service ->
    let segs = List.rev_map segs ~f:String.capitalize_ascii in
    String.concat ~sep:"" segs
  | Package | Extension ->
    assert false
  | _ ->
    failwith "not implemented"

(* takes the path in reverse order *)
let rec ocaml_name a t path =
  (* Printf.fprintf Base.debug "ocaml_name: %s | %s\n" *)
  (*   (String.concat ~sep:"." a) (String.concat ~sep:"." (List.rev path)) ; *)
  match path with
  | [] ->
    (* TODO: Implement real name based on t. *)
    name t.kind a
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

(* total number of nodes (including the root) *)
let rec length t =
  List.fold_left t.chs ~init:1 ~f:(fun a x -> a + length x)

let is_cyclic _t _path = false

let dump_one oc pfx { file; name; kind; _ } =
  let fqn = name :: pfx |> List.rev |> String.concat ~sep:"." in
  let ppf = Format.formatter_of_out_channel oc in
  Format.fprintf ppf "%s (%a) @@ %s@." fqn pp_kind kind file

let rec dump oc pfx t =
  dump_one oc pfx t ;
  let pfx = t.name :: pfx in
  List.iter t.chs ~f:(dump oc pfx)

let dump t oc = dump oc [] t

