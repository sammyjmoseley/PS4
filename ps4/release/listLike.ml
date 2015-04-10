
(******************************************************************************)
(** module types from .mli                                                   **)
(******************************************************************************)

(* see .mli *)
module type CORE = sig
  type 'a t
  val equals : 'a t -> 'a t -> bool
  val empty  : 'a t
  val cons   : 'a -> 'a t -> 'a t
  val decons : 'a t -> ('a * 'a t) option
  val lookup : 'a t -> int -> 'a option
  val update : 'a t -> int -> 'a -> 'a t option
  val length : 'a t -> int
end

(* see .mli *)
module type EXTENSION = sig
  type 'a t
  val map        : ('a -> 'b) -> 'a t -> 'b t
  val fold_left  : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
  val fold_right : ('b -> 'a -> 'a) -> 'b t -> 'a -> 'a
  val to_list : 'a t -> 'a list
  val of_list : 'a list -> 'a t
end

(* see .mli *)
module type S = sig
  type 'a t
  include CORE      with type 'a t := 'a t
  include EXTENSION with type 'a t := 'a t
end

(******************************************************************************)
(** Extend functor                                                           **)
(******************************************************************************)

module Extend (Core : CORE) = struct

  type 'a t = 'a Core.t

  let rec map f l = 
    match Core.decons l with
    | None ->  Core.empty
    | Some (h,t) -> Core.cons (f h) (map f t)
  let rec fold_left f a l =
    match Core.decons l with
    | None -> a
    | Some (h, t) -> fold_left f (f a h) t
  let rec fold_right f l a = 
    match Core.decons l with
    | None -> a
    | Some (h, t) -> f h (fold_right f t a)
  let rec to_list l =
    match Core.decons l with
    | None -> []
    | Some (h, t) -> h::(to_list t)
  let rec of_list l =
    match l with
    | [] -> Core.empty
    | h::t -> Core.cons h (of_list t)

end

(******************************************************************************)
(** CORE module specification functor                                        **)
(******************************************************************************)

(* see .mli *)
module Spec (C : CORE) = struct
  open QCheck
  open Assertions

  open C
  module E = Extend(C)

  (****************************************************************************)
  (** specifications **********************************************************)
  (****************************************************************************)

  let test_passes = true
  let test_fails  = false

  let equals_self l = equals l l

  let equals_symmetry (l1,l2) = if equals l1 l2 then equals l2 l1 else test_passes

  (** TODO: additional tests *)

  (****************************************************************************)
  (** QCheck generators *******************************************************)
  (****************************************************************************)

  (** [arb_listlike n] returns an arbitrary list of length n *)
  let rec arb_listlike_len (n : int) : (char C.t Arbitrary.t) =
    (* Note: this function uses the monad idiom which will be
     * covered lecture in the near future.  You should not need to write
     * functions using this idiom to complete this assignment.  You should be
     * able to use the functions in the QCheck.Arbitrary module to build any
     * additional arbitrary values that you need.  See arb_listlike and
     * arb_listlike_pair below. *)
    let open Arbitrary in
    if n = 0
    then
      return C.empty
    else
      Arbitrary.alpha        >>= fun hd ->
      arb_listlike_len (n-1) >>= fun tl ->
      return (C.cons hd tl)

  (** [arb_listlike] is an arbitrary list with an arbitrary (but small) length *)
  let arb_listlike : (char C.t) Arbitrary.t =
    Arbitrary.((return C.empty) ||| (small_int >>= arb_listlike_len))

  (** [arb_listlike_pair] is an arbitrary pair of lists *)
  let arb_listlike_pair : (char C.t * char C.t) Arbitrary.t =
    Arbitrary.pair arb_listlike arb_listlike

  (** TODO: additional generators if needed *)

  (****************************************************************************)
  (** Tests *******************************************************************)
  (****************************************************************************)

  TEST_UNIT "equals_self"     = assert_qcheck arb_listlike      equals_self
  TEST_UNIT "equals_symmetry" = assert_qcheck arb_listlike_pair equals_symmetry

  (** TODO: additional tests *)

end

