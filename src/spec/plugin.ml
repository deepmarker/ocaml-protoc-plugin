(************************************************)
(*       AUTOGENERATED FILE - DO NOT EDIT!      *)
(************************************************)
(* Generated by: ocaml-protoc-plugin            *)
(* https://github.com/issuu/ocaml-protoc-plugin *)
(************************************************)
(*
  Source: google/protobuf/compiler/plugin.proto
  Syntax: proto2
  Parameters:
    debug=false
    annot=''
    opens=[]
    int64_as_int=true
    int32_as_int=true
    fixed_as_int=false
    singleton_record=false
*)

open Ocaml_protoc_plugin.Runtime [@@warning "-33"]
(**/**)
module Imported'modules = struct
  module Descriptor = Descriptor
end
(**/**)
module Google = struct
  module Protobuf = struct
    module Compiler = struct
      module rec Version : sig
        val name': unit -> string
        type t = { major: int option; minor: int option; patch: int option; suffix: string option } 
        val create : ?major:int -> ?minor:int -> ?patch:int -> ?suffix:string -> unit -> t
        val to_proto: t -> Runtime'.Writer.t
        val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
      end = struct 
        let name' () = "plugin.google.protobuf.compiler.Version"
        type t = { major: int option; minor: int option; patch: int option; suffix: string option }
        let create =
          fun ?major ?minor ?patch ?suffix () -> 
          
          { major; minor; patch; suffix }
        
        let to_proto =
          let apply = fun ~f:f' { major; minor; patch; suffix } -> f' [] major minor patch suffix in
          let spec = Runtime'.Serialize.C.( basic_opt (1, int32_int) ^:: basic_opt (2, int32_int) ^:: basic_opt (3, int32_int) ^:: basic_opt (4, string) ^:: nil ) in
          let serialize = Runtime'.Serialize.serialize [] (spec) in
          fun t -> apply ~f:serialize t
        
        let from_proto =
          let constructor = fun _extensions major minor patch suffix -> { major; minor; patch; suffix } in
          let spec = Runtime'.Deserialize.C.( basic_opt (1, int32_int) ^:: basic_opt (2, int32_int) ^:: basic_opt (3, int32_int) ^:: basic_opt (4, string) ^:: nil ) in
          let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
          fun writer -> deserialize writer |> Runtime'.Result.open_error
        
      end
      and CodeGeneratorRequest : sig
        val name': unit -> string
        type t = { file_to_generate: string list; parameter: string option; proto_file: Imported'modules.Descriptor.Google.Protobuf.FileDescriptorProto.t list; compiler_version: Version.t option } 
        val create : ?file_to_generate:string list -> ?parameter:string -> ?proto_file:Imported'modules.Descriptor.Google.Protobuf.FileDescriptorProto.t list -> ?compiler_version:Version.t -> unit -> t
        val to_proto: t -> Runtime'.Writer.t
        val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
      end = struct 
        let name' () = "plugin.google.protobuf.compiler.CodeGeneratorRequest"
        type t = { file_to_generate: string list; parameter: string option; proto_file: Imported'modules.Descriptor.Google.Protobuf.FileDescriptorProto.t list; compiler_version: Version.t option }
        let create =
          fun ?file_to_generate ?parameter ?proto_file ?compiler_version () -> 
          let file_to_generate = match file_to_generate with Some v -> v | None -> [] in
          let proto_file = match proto_file with Some v -> v | None -> [] in
          { file_to_generate; parameter; proto_file; compiler_version }
        
        let to_proto =
          let apply = fun ~f:f' { file_to_generate; parameter; proto_file; compiler_version } -> f' [] file_to_generate parameter proto_file compiler_version in
          let spec = Runtime'.Serialize.C.( repeated (1, string, not_packed) ^:: basic_opt (2, string) ^:: repeated (15, (message (fun t -> Imported'modules.Descriptor.Google.Protobuf.FileDescriptorProto.to_proto t)), not_packed) ^:: basic_opt (3, (message (fun t -> Version.to_proto t))) ^:: nil ) in
          let serialize = Runtime'.Serialize.serialize [] (spec) in
          fun t -> apply ~f:serialize t
        
        let from_proto =
          let constructor = fun _extensions file_to_generate parameter proto_file compiler_version -> { file_to_generate; parameter; proto_file; compiler_version } in
          let spec = Runtime'.Deserialize.C.( repeated (1, string, not_packed) ^:: basic_opt (2, string) ^:: repeated (15, (message (fun t -> Imported'modules.Descriptor.Google.Protobuf.FileDescriptorProto.from_proto t)), not_packed) ^:: basic_opt (3, (message (fun t -> Version.from_proto t))) ^:: nil ) in
          let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
          fun writer -> deserialize writer |> Runtime'.Result.open_error
        
      end
      and CodeGeneratorResponse : sig
        module rec Feature : sig
          type t = FEATURE_NONE | FEATURE_PROTO3_OPTIONAL 
          val to_int: t -> int
          val from_int: int -> (t, [> Runtime'.Result.error]) result
        end
        and File : sig
          val name': unit -> string
          type t = { name: string option; insertion_point: string option; content: string option } 
          val create : ?name:string -> ?insertion_point:string -> ?content:string -> unit -> t
          val to_proto: t -> Runtime'.Writer.t
          val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
        end
        val name': unit -> string
        type t = { error: string option; supported_features: int option; file: CodeGeneratorResponse.File.t list } 
        val create : ?error:string -> ?supported_features:int -> ?file:CodeGeneratorResponse.File.t list -> unit -> t
        val to_proto: t -> Runtime'.Writer.t
        val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
      end = struct 
        module rec Feature : sig
          type t = FEATURE_NONE | FEATURE_PROTO3_OPTIONAL 
          val to_int: t -> int
          val from_int: int -> (t, [> Runtime'.Result.error]) result
        end = struct 
          type t = FEATURE_NONE | FEATURE_PROTO3_OPTIONAL 
          let to_int = function
            | FEATURE_NONE -> 0
            | FEATURE_PROTO3_OPTIONAL -> 1
          
          let from_int = function
            | 0 -> Ok FEATURE_NONE
            | 1 -> Ok FEATURE_PROTO3_OPTIONAL
            | n -> Error (`Unknown_enum_value n)
          
        end
        and File : sig
          val name': unit -> string
          type t = { name: string option; insertion_point: string option; content: string option } 
          val create : ?name:string -> ?insertion_point:string -> ?content:string -> unit -> t
          val to_proto: t -> Runtime'.Writer.t
          val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
        end = struct 
          let name' () = "plugin.google.protobuf.compiler.CodeGeneratorResponse.File"
          type t = { name: string option; insertion_point: string option; content: string option }
          let create =
            fun ?name ?insertion_point ?content () -> 
            
            { name; insertion_point; content }
          
          let to_proto =
            let apply = fun ~f:f' { name; insertion_point; content } -> f' [] name insertion_point content in
            let spec = Runtime'.Serialize.C.( basic_opt (1, string) ^:: basic_opt (2, string) ^:: basic_opt (15, string) ^:: nil ) in
            let serialize = Runtime'.Serialize.serialize [] (spec) in
            fun t -> apply ~f:serialize t
          
          let from_proto =
            let constructor = fun _extensions name insertion_point content -> { name; insertion_point; content } in
            let spec = Runtime'.Deserialize.C.( basic_opt (1, string) ^:: basic_opt (2, string) ^:: basic_opt (15, string) ^:: nil ) in
            let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
            fun writer -> deserialize writer |> Runtime'.Result.open_error
          
        end
        let name' () = "plugin.google.protobuf.compiler.CodeGeneratorResponse"
        type t = { error: string option; supported_features: int option; file: CodeGeneratorResponse.File.t list }
        let create =
          fun ?error ?supported_features ?file () -> 
          let file = match file with Some v -> v | None -> [] in
          { error; supported_features; file }
        
        let to_proto =
          let apply = fun ~f:f' { error; supported_features; file } -> f' [] error supported_features file in
          let spec = Runtime'.Serialize.C.( basic_opt (1, string) ^:: basic_opt (2, uint64_int) ^:: repeated (15, (message (fun t -> CodeGeneratorResponse.File.to_proto t)), not_packed) ^:: nil ) in
          let serialize = Runtime'.Serialize.serialize [] (spec) in
          fun t -> apply ~f:serialize t
        
        let from_proto =
          let constructor = fun _extensions error supported_features file -> { error; supported_features; file } in
          let spec = Runtime'.Deserialize.C.( basic_opt (1, string) ^:: basic_opt (2, uint64_int) ^:: repeated (15, (message (fun t -> CodeGeneratorResponse.File.from_proto t)), not_packed) ^:: nil ) in
          let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
          fun writer -> deserialize writer |> Runtime'.Result.open_error
        
      end
    end
  end
end