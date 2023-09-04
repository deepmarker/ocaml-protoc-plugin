open StdLabels
open Spec.Descriptor.Google.Protobuf

module IntSet = Set.Make(struct type t = int let compare = compare end)

(** Slightly overloaded name here.
    Its also used for all other types which would go into a module *)
type module' = {
  module_name : string;
  signature : Code.t;
  implementation : Code.t;
}

(* Enums are not mangled - Maybe they should be lowercased though. *)
let emit_enum_type ~scope ~params
      EnumDescriptorProto.{name; value = values; options = _; reserved_range = _; reserved_name = _} =
  let name = Option.get name in
  let module_name = Scope.get_name scope name in
  let signature = Code.init () in
  let implementation = Code.init () in
  let scope = Scope.push scope name in
  let t = Code.init () in
  let open Code in
  emit t `None "type t = %s %s"
    (List.map ~f:(fun EnumValueDescriptorProto.{name; _} -> Scope.get_name_exn scope name) values
     |> String.concat ~sep:" | "
    )
    params.Parameters.annot;
  append signature t;
  append implementation t;
  emit signature `None "val to_int: t -> int";
  emit signature `None "val from_int: int -> (t, [> Runtime'.Result.error]) result";

  emit implementation `Begin "let to_int = function";
  List.iter ~f:(fun EnumValueDescriptorProto.{name; number; _} ->
    emit implementation `None "| %s -> %d" (Scope.get_name_exn scope name) (Option.get number)
  ) values;
  emit implementation `End "";

  emit implementation `Begin "let from_int = function";
  let _ =
    List.fold_left ~init:IntSet.empty ~f:(fun seen EnumValueDescriptorProto.{name; number; _} ->
        let idx = (Option.get number) in
        match IntSet.mem idx seen with
        | true -> seen
        | false ->
          emit implementation `None "| %d -> Ok %s" idx (Scope.get_name_exn scope name);
          IntSet.add idx seen
      ) values
  in
  emit implementation `None "| n -> Error (`Unknown_enum_value n)";
  emit implementation `End "";
  {module_name; signature; implementation}

let emit_method t local_scope scope service_name MethodDescriptorProto.{ name; input_type; output_type; _} =
  let name = Option.get name in
  let uncapitalized_name = String.uncapitalize_ascii name |> Scope.Local.get_unique_name local_scope in
  (* To keep symmetry, only ensure that lowercased names are unique
     so that the upper case names are aswell.  We should remove this
     mapping if/when we deprecate the old API *)
  let capitalized_name = String.capitalize_ascii uncapitalized_name in

  let package_segs = Scope.get_package_name scope in
  let package_name =
    match package_segs with
    | [] -> ""
    | p -> String.concat ~sep:"." p
  in
  (* We're in some context right now *)
  let input_type = Option.get input_type in
  (* Printf.fprintf !Base.debug "input type = %s\n" input_type ; *)
  let output_type = Option.get output_type in
  let input = Scope.get_scoped_name scope input_type in
  let input_t = Scope.get_scoped_name scope ~postfix:"t" input_type in
  let output = Scope.get_scoped_name scope output_type in
  let output_t = Scope.get_scoped_name scope ~postfix:"t" output_type in
  let open Code in
  emit t `Begin "module %s = struct" capitalized_name;
  emit t `None "let package_name = %s" (match package_name with "" -> "None" | x -> Printf.sprintf {|Some "%s"|} x);
  emit t `None "let service_name = \"%s\"" service_name;
  emit t `None "let method_name = \"%s\"" name;
  emit t `None "let name = \"/%s%s/%s\"" package_name service_name name;
  emit t `None "module Request = %s" input;
  emit t `None "module Response = %s" output;
  emit t `End "end";
  emit t `Begin "let %s = " uncapitalized_name;
  emit t `None "(module %s : Runtime'.Service.Message with type t = %s ), "
    input
    input_t;
  emit t `None "(module %s : Runtime'.Service.Message with type t = %s )"
    output
    output_t;
  emit t `End "";
  ()

let emit_service_type scope ServiceDescriptorProto.{ name; method' = methods; _ } =
  let name = Option.get name in
  let t = Code.init () in
  Code.emit t `Begin "module %s = struct" (Scope.get_name scope name);
  let local_scope = Scope.Local.create () in

  List.iter methods ~f:(emit_method t local_scope (Scope.push scope name) name) ;
  Code.emit t `End "end";
  t

let emit_extension ~scope ~params (field : FieldDescriptorProto.t) =
  let name = Option.get field.name in
  (* I guess we need to push extendee scope here! *)
  let extendee = Option.get field.extendee in
  let extendee_segs = String.split_on_char extendee ~sep:'.' |> List.tl in
  let scope = List.fold_left extendee_segs ~init:scope ~f:Scope.push in
  let module_name = Scope.get_name scope name in
  let extendee_type = Scope.get_scoped_name scope ~postfix:"t" extendee in
  let extendee_field = Scope.get_scoped_name scope ~postfix:"extensions'" extendee in
  (* Create the type of the type' / type_name *)
  let t =
    let params = Parameters.{params with singleton_record = false} in
    Types.make ~params ~syntax:`Proto2 ~is_cyclic:false ~scope ~is_map_entry:false ~has_extensions:false ~fields:[field] []
  in

  let signature = Code.init () in
  let implementation = Code.init () in
  let open Code in
  append implementation signature;

  emit signature `None "type t = %s %s" t.type' params.annot;
  emit signature `None "val get: %s -> (%s, [> Runtime'.Result.error]) result" extendee_type t.type';
  emit signature `None "val set: %s -> %s -> %s" extendee_type t.type' extendee_type;

  emit implementation `None "type t = %s %s" t.type' params.annot;
  emit implementation `None "let get extendee = Runtime'.Extensions.get %s (extendee.%s) |> Runtime'.Result.open_error" t.deserialize_spec extendee_field ;
  emit implementation `Begin "let set extendee t =";
  emit implementation `None "let extensions' = Runtime'.Extensions.set (%s) (extendee.%s) t in" t.serialize_spec extendee_field;
  emit implementation `None "{ extendee with %s = extensions' }" extendee_field;
  emit implementation `End "";
  { module_name; signature; implementation }

let is_map_entry = function
  | Some MessageOptions.{ map_entry = Some true; _ } -> true
  | _ -> false

(** Emit the nested types. *)
let emit_sub dest ~is_implementation ~is_first {module_name; signature; implementation} =
  let () =
    match is_first with
    | true -> Code.emit dest `Begin "module rec %s : sig" module_name
    | false -> Code.emit dest `Begin "and %s : sig" module_name
  in
  Code.append dest signature;
  let () =
    match is_implementation with
    | false -> ()
    | true ->
      Code.emit dest `EndBegin "end = struct ";
      Code.append dest implementation
  in
  Code.emit dest `End "end";
  ()

let rec emit_nested_types ~syntax ~signature ~implementation ?(is_first = true) nested_types =
  match nested_types with
  | [] -> ()
  | sub :: subs ->
    emit_sub ~is_implementation:false signature ~is_first sub;
    emit_sub ~is_implementation:true implementation ~is_first sub;
    emit_nested_types ~syntax ~signature ~implementation ~is_first:false subs

(* Emit a message plus all its subtypes.
   Why is this not being called recursively, but rather calling sub functions which never returns
*)
let rec emit_message ~params ~syntax scope
    DescriptorProto.{ name; field = fields; extension = extensions;
                      nested_type = nested_types; enum_type = enum_types;
                      extension_range = extension_ranges; oneof_decl = oneof_decls; options;
                      reserved_range = _; reserved_name = _ } : module' =

  let signature = Code.init () in
  let implementation = Code.init () in

  let has_extensions = not (extension_ranges = []) in
  (* Ignore empty modules *)
  let module_name, scope =
    match name with
    | None -> "", scope
    | Some name ->
      let module_name = Scope.get_name scope name in
      module_name, Scope.push scope name
  in

  (* Emit nested types *)


  List.map ~f:(emit_enum_type ~scope ~params) enum_types
  @ List.map ~f:(emit_message ~params ~syntax scope) nested_types
  @ List.map ~f:(emit_extension ~scope ~params) extensions
  |> emit_nested_types ~syntax ~signature ~implementation;

  let is_map_entry = is_map_entry options in
  let is_cyclic = Scope.is_cyclic scope in
  let extension_ranges =
    extension_ranges
    |> List.map ~f:(function
      | DescriptorProto.ExtensionRange.{ start = Some start; end' = Some end'; _ } -> (start, end')
      | _ -> failwith "Extension ranges must be defined"
    )
    |> List.map ~f:(fun (s, e) -> Printf.sprintf "(%d, %d)" s e)
    |> String.concat ~sep:"; "
    |> Printf.sprintf "[%s]"
  in
  let Types.{ type'; constructor; apply; deserialize_spec; serialize_spec; default_constructor_sig; default_constructor_impl } =
    Types.make ~params ~syntax ~is_cyclic ~is_map_entry ~has_extensions ~scope ~fields oneof_decls
  in
  ignore (default_constructor_sig, default_constructor_impl);
  let open Code in
  (* Emit below only if name above is not None. Can this happen? *)
  emit signature `None "val name': unit -> string";
  emit signature `None "type t = %s %s" type' params.annot;
  emit signature `None "val make : %s" default_constructor_sig;
  emit signature `None "val to_proto: t -> Runtime'.Writer.t";
  emit signature `None "val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result";

  emit implementation `None "let name' () = \"%s\"" (Scope.get_current_scope scope);
  emit implementation `None "type t = %s %s" type' params.annot;
  emit implementation `Begin "let make =";
  emit implementation `None "%s" default_constructor_impl;
  emit implementation `End "";

  emit implementation `Begin "let to_proto =";
  emit implementation `None "let apply = %s in" apply;
  emit implementation `None "let spec = %s in" serialize_spec;
  emit implementation `None "let serialize = Runtime'.Serialize.serialize %s (spec) in" extension_ranges;
  emit implementation `None "fun t -> apply ~f:serialize t";
  emit implementation `End "";

  emit implementation `Begin "let from_proto =";
  emit implementation `None "let constructor = %s in" constructor;
  emit implementation `None "let spec = %s in" deserialize_spec;
  emit implementation `None "let deserialize = Runtime'.Deserialize.deserialize %s spec constructor in" extension_ranges;
  emit implementation `None "fun writer -> deserialize writer |> Runtime'.Result.open_error";
  emit implementation `End "";
  {module_name; signature; implementation}

let append_impl ~params ~syntax scope message_type services =
  let {module_name = _; implementation; _} = emit_message ~params ~syntax scope message_type in
  List.iter ~f:(fun service ->
    Code.append implementation (emit_service_type scope service)
  ) services;
  implementation

let emit_banner impl (params:Parameters.t) name syntax =
  let open Code in
  emit impl `None "(************************************************)";
  emit impl `None "(*       AUTOGENERATED FILE - DO NOT EDIT!      *)";
  emit impl `None "(************************************************)";
  emit impl `None "(* Generated by: ocaml-protoc-plugin            *)";
  emit impl `None "(* https://github.com/issuu/ocaml-protoc-plugin *)";
  emit impl `None "(************************************************)";
  emit impl `None "(*";
  emit impl `None "  Source: %s" name;
  emit impl `None "  Syntax: %s" (match syntax with `Proto2 -> "proto2" | `Proto3 -> "proto3");
  emit impl `None "  Parameters:";
  emit impl `None "    debug=%b" params.debug;
  emit impl `None "    annot='%s'" params.annot;
  emit impl `None "    opens=[%s]" (String.concat ~sep:"; " params.opens);
  emit impl `None "    int64_as_int=%b" params.int64_as_int;
  emit impl `None "    int32_as_int=%b" params.int32_as_int;
  emit impl `None "    fixed_as_int=%b" params.fixed_as_int;
  emit impl `None "    singleton_record=%b" params.singleton_record;
  emit impl `None "*)"

let parse_proto_file ~params scope
    FileDescriptorProto.{ name; package=_; dependency = _; public_dependency = _;
                          weak_dependency = _; message_type = message_types;
                          enum_type = enum_types; service = services; extension;
                          options = _; source_code_info = _; syntax; }
  =
  let name = Option.get name in

  let syntax = match syntax with
    | None | Some "proto2" -> `Proto2
    | Some "proto3" -> `Proto3
    | _ -> failwith "Unsupported syntax"
  in
  let open Code in
  let implementation = init () in
  emit_banner implementation params name syntax ;
  emit implementation `None "";
  List.iter
    ("Ocaml_protoc_plugin.Runtime" :: params.opens)
    ~f:(emit implementation `None "open %s [@@warning \"-33\"]" ) ;
  let message_type =
    DescriptorProto.{name = None; nested_type=message_types; enum_type = enum_types;
                     field = []; extension; extension_range = []; oneof_decl = [];
                     options = None; reserved_range = []; reserved_name = []; }
  in
  append implementation (append_impl ~params ~syntax scope message_type services);
  let ocaml_name =
    (String.map name ~f:(function '-' -> '_' | c -> c)
     |> Filename.remove_extension) ^ ".ml" in
  ocaml_name, implementation
