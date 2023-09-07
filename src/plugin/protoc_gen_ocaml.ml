open StdLabels
module Descriptor = Spec.Descriptor.Google.Protobuf
module Plugin = Spec.Plugin.Google.Protobuf.Compiler

(* Read from stdin *)
let read () =
  In_channel.input_all stdin
  |> Ocaml_protoc_plugin.Reader.create
  |> Plugin.CodeGeneratorRequest.from_proto
  |> function
  | Ok v -> v
  | Error _ -> failwith "Could not decode generator request"

(* Write to stdout *)
let write response =
  Plugin.CodeGeneratorResponse.to_proto response
  |> Ocaml_protoc_plugin.Writer.contents
  |> output_string stdout

(* takes raw args from FileDescriptorProto.t *)
let file_name ?package name =
  let tidy = String.map ~f:(function '-' -> '_' | c -> c) in
  let name = Filename.(basename name |> tidy |> remove_extension) in
  let name =
  match package with
  | None -> name
  | Some pkg ->
    let pkg = String.split_on_char ~sep:'.' pkg in
    let pkg = String.concat ~sep:"" (List.map pkg ~f:String.capitalize_ascii) in
    pkg ^ String.capitalize_ascii name in
  let ret = name ^ ".ml" in
  Printf.fprintf !Base.debug "file_name: name %s / package %s / final = %s\n"
    name (Option.value package ~default:"<nil>") ret ;
  ret

(* file_to generate: proto files to process, among proto_file available *)
(* I can decide where to put the code. *)
let parse_request (x : Plugin.CodeGeneratorRequest.t) =
  let params = Parameters.parse (Option.value ~default:"" x.parameter) in
  if params.debug then (
    List.iter x.file_to_generate ~f:(fun x ->
      Printf.fprintf !Base.debug "### File to generate: %s\n" x) ;
    List.iter x.proto_file ~f:(fun (x:Descriptor.FileDescriptorProto.t) ->
      let name = Option.get x.name in
      let package = Option.value ~default:"" x.package in
      Printf.fprintf !Base.debug "### Proto file: %s (%s)\n" name package)
    ) ;
  (* Find the correct file to process *)
  let target_proto_files =
    List.filter x.proto_file ~f:(fun Descriptor.FileDescriptorProto.{name; _} ->
      List.mem ~set:x.file_to_generate (Option.get name)
    )
  in
  let type_db = List.fold_left x.proto_file ~init:Type.empty ~f:(Type.add_fd) in
  (* Debug *)
  (* Type.dump type_db !Base.debug ; *)
  (* *)
  List.map target_proto_files ~f:(fun (proto_file : Descriptor.FileDescriptorProto.t) ->
    let name = Option.get proto_file.name in
    let fn = file_name ?package:proto_file.package name in
    (* let _fn = file_name ?package name in *)
    (* Printf.fprintf !Base.debug "parse_proto_file: name = %s, file_name = %s\n" name fn ; *)
    let code = Emit.parse_proto_file ~params type_db proto_file in
    (* if params.debug then Printf.eprintf "%s\n%!" (Code.contents code); *)
    (* Here, build the resulting file name. *)
    fn, code
  )

(* Generate the code *)
let main () =
  let open Plugin.CodeGeneratorResponse in
  let request = read () in
  let outputs = parse_request request in
  (* Apparently this can be done several times? *)
  let response_of_output (filename, code) =
    File.make ~name:filename ~content:(Code.contents code) ()
  in
  let files = List.map ~f:response_of_output outputs in
  let response = make ~supported_features:1 ~file:files () in
  write response

let main () =
  Printexc.record_backtrace true;
  try main () with
  | exn ->
    let bt = Printexc.get_raw_backtrace () in
    Printexc.raise_with_backtrace exn bt

let () =
  main ()
  (* Fun.protect main ~finally:(fun () -> *)
  (*   Out_channel.close !Base.debug) *)
