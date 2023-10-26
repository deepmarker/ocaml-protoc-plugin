type t = (int * Field.t) list [@@deriving bin_io, compare, hash, sexp, yojson]
val default : t
val pp : Format.formatter -> t -> unit
val show : t -> string
val equal : t -> t -> bool
val compare : t -> t -> int
val get : ('b -> 'b, 'b) Deserialize.S.compound_list -> t -> 'b Result.t
val set : ('a -> Writer.t, Writer.t) Spec.Serialize.compound_list -> t -> 'a -> t
