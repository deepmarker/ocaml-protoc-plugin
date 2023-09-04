open MoreLabels

module StringSet : Set.S with type elt = string

module StringMap : sig
  include Map.S with type key = string
  val add_uniq: key:key -> data:'a -> 'a t -> 'a t
end

module StringListSet : Set.S with type elt = string list
module StringListMap : Map.S with type key = string list

val debug : out_channel ref
