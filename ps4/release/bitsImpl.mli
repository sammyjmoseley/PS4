module Core : sig
  type 'a tree = Leaf of 'a | Node of 'a tree * 'a tree
  type 'a tbit = Zero | One of 'a tree
  type 'a t    = 'a tbit list

  (** if [t] is a valid 'a t, returns [t]; otherwise raises an exception *)
  val rep_ok : 'a t -> 'a t

  include ListLike.CORE with type 'a t := 'a t
end

include ListLike.S with type 'a t := 'a Core.t

