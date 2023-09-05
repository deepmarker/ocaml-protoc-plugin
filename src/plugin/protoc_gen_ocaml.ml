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
  Type.dump type_db !Base.debug ;
  (* *)
  List.map target_proto_files ~f:(fun (proto_file : Descriptor.FileDescriptorProto.t) ->
    let scope = Scope.create proto_file type_db in
    let name, code = Emit.parse_proto_file ~params scope proto_file in
    (* if params.debug then Printf.eprintf "%s\n%!" (Code.contents code); *)
    Filename.basename name, code
  )

let main () =
  let request = read () in
  let outputs = parse_request request in
  (* Apparently this can be done several times? *)
  let response_of_output (filename, code) =
    Plugin.CodeGeneratorResponse.File.make ~name:filename ~content:(Code.contents code) ()
  in
  let files = List.map ~f:response_of_output outputs in
  let response = Plugin.CodeGeneratorResponse.make ~supported_features:1 ~file:files () in
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
