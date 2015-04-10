(**
 * ListLike.CORE contains all of the operations that a random access list
 * implementor must write
 *
 * Note: You MAY NOT change this signature
 *)
module type CORE = sig
  type 'a t

  (** compares two list-like values for equality.  Uses (=) to compare values
      of type 'a *)
  val equals : 'a t -> 'a t -> bool

  (** An empty list-like *)
  val empty  : 'a t

  (** (cons h tl) returns the list-like representing h::tl *)
  val cons   : 'a -> 'a t -> 'a t

  (** (decons l) returns Some (h,tl) if l represents h::tl or None if l represents [] *)
  val decons : 'a t -> ('a * 'a t) option


  (** [lookup l n] returns the nth element of the list-like l *)
  val lookup : 'a t -> int -> 'a option

  (** [update l n x] returns a listlike that is the same as l except that the
      nth entry has been replaced by x. *)
  val update : 'a t -> int -> 'a -> 'a t option

  (** retuns the length of a list-like *)
  val length : 'a t -> int
end

(**
 * The ListLike.Spec functor defines the properties that a valid implementation
 * of ListLike.Core must satisfy.
 *
 * Each property has a test function that takes an argument for each variable,
 * and returns true if the arguments satisfy the property.  For example, one
 * requirement for the [equals] function to be correct is that for any list l,
 * [equals l l] returns true.  This is checked by [equals_self : 'a t -> bool]
 * which is defined by [let equals_self l = equals l l]
 *
 * This module should contain TESTs of the properties, so that an implementor
 * can simply write
 *
 * TEST_MODULE ListLike.Spec(TheirImpl)
 *
 * to test TheirImpl against the specification.
 *
 * Note: You SHOULD extend this signature.
 *)
module Spec (C : CORE) : sig
  open C

  (** for all l, equals l l *)
  val equals_self : 'a t -> bool

  (** for all l1 and l2, if [equals l1 l2] then [equals l2 l1]. *)
  val equals_symmetry : 'a t * 'a t -> bool

  (* ... Other specs ... *)
end

(**
 * The Extensions module contains useful functions defined for any ListLike
 * implementation
 *
 * Note: You MAY extend this signature
 *)
module type EXTENSION = sig
  type 'a t

  (* common higher-order functions *)
  val map        : ('a -> 'b) -> 'a t -> 'b t
  val fold_left  : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
  val fold_right : ('b -> 'a -> 'a) -> 'b t -> 'a -> 'a

  (* conversion functions *)
  val to_list : 'a t -> 'a list
  val of_list : 'a list -> 'a t
end

(**
 * A generic implementation of the EXTENSION functions
 *)
module Extend (Core:CORE) : EXTENSION with type 'a t := 'a Core.t

(**
 * ListLike.S combines the CORE and EXTENSION module types; it will typically
 * be exposed by implementations and required by users.
 *
 * Note: You SHOULD NOT extend this signature (except by extending EXTENSION)
 *)

module type S = sig
  type 'a t

  include CORE      with type 'a t := 'a t
  include EXTENSION with type 'a t := 'a t
end

