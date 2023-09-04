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

let parse_request Plugin.CodeGeneratorRequest.{file_to_generate = files_to_generate; parameter = parameters; proto_file = proto_files; compiler_version = _} =
  let params = Parameters.parse (Option.value ~default:"" parameters) in
  if params.debug then (
    List.iter files_to_generate ~f:(fun x ->
      Format.eprintf "File to generate: %s\n" x) ;
    List.iter proto_files ~f:(fun (x:Descriptor.FileDescriptorProto.t) ->
      let name = Option.get x.name in
      let package = Option.value ~default:"" x.package in
      Format.eprintf "%s (%s)\n" name package)
    ) ;
  (* Find the correct file to process *)
  let target_proto_files =
    List.filter proto_files ~f:(fun Descriptor.FileDescriptorProto.{name; _} ->
      List.mem ~set:files_to_generate (Option.get name)
    )
  in
  let type_db = List.fold_left proto_files ~init:Type.empty ~f:(Type.add_fd) in
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
  let response_of_output (name, code) =
    Plugin.CodeGeneratorResponse.File.make ~name ~content:(Code.contents code) ()
  in
  let response : Plugin.CodeGeneratorResponse.t =
    Plugin.CodeGeneratorResponse.make ~supported_features:1 ~file:(List.map ~f:response_of_output outputs) ()
  in
  write response

let main () =
  Printexc.record_backtrace true;
  try main () with
  | exn ->
    let bt = Printexc.get_raw_backtrace () in
    Printexc.raise_with_backtrace exn bt

let () =
  main ()
  (* Fun.protect main ~finally:(fun () -> Out_channel.close !Base.debug) *)
