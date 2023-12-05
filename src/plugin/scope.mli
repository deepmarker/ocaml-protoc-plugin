open Spec.Descriptor.Google.Protobuf

module Local : sig
  type t
  val create: unit -> t
  val get_unique_name: t -> string -> string
end

(** Type of a scope *)
type t

val replace_path : t -> string list -> t

val pp : Format.formatter -> t -> unit

(** [create fd db] is a scope for [fd] using type database [db]. If
    [fd] is namespaced (package not empty), resulting scope takes it
    into account. *)
val create: FileDescriptorProto.t -> Type.t -> t

(** Push an identifier to the current scope *)
val push : t -> string -> t


(** Get OCaml names. *)

(** Get the ocaml name of the given proto type name, based on the current scope *)
val get_scoped_name : ?postfix:string -> t -> string -> string

(** Get the (absolute?) ocaml name of the given proto type name, based
    on the current scope. Need to escape dashes and other unfriendly
    characters. *)
val get_name : t -> string -> string

(** Get the ocaml name of the given proto type name, based on the current scope *)
val get_name_exn : t -> string option -> string


(** Get the type of the protobuf message *)
val get_current_scope : t -> string

(** Get the package name. This function assumes callee is in service scope *)
val get_package_name : t -> string list

(** Tell if the type pointed to by the current scope is part of a cycle. *)
val is_cyclic: t -> bool
