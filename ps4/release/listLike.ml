
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

  (** testing whether cons and decons have an inverse relationship*)
  let cons_decons (x, l) = match decons (cons x l) with
                           | None -> equals empty l
                           |  Some (x', l') -> equals l l'

  (** tests if length of empty list is 0*)
  let empty_length = (length empty = 0)

  (** tests if cons adds an element to the beginning of the list*)
  let cons_first (x, l) = match lookup (cons x l) 0 with
                          | None -> test_fails
                          | Some a -> a = x

  (** checks if cons only increases list length by one *)
  let cons_length (x, l) = (length l) + 1 = length (cons x l)

  (** checks if decons on an empty list returns None *)
  let decons_empty = (decons empty = None)

  (** checks if decons decreases list length by one *)
  let decons_length l = 
    let l' = match decons l with
    | None -> l (** what do i do here? i could expect a non-empty list*)
    | Some (h, tl) -> tl in
    length l - 1 = length l'

  (** checks if decons takes away the first element of the list *)
  let decons_first (x, l) = match decons (cons x l) with
                           | None -> equals empty l
                           |  Some (x', l') -> x = x'

  (** checks if decons a list of length 1 returns the empty list
      Requires: l has length 1*)
  let decons_one l = 
    match decons l with
    | None -> test_fails 
    | Some (x', l') -> equals l' empty

  (** checks if lookup returns None if the index is greater than or equal to the size of the input list *)
  let lookup_bounds l = match lookup l (length l) with
                        | None -> test_passes
                        | Some a -> test_fails

  (** checks if lookup returns None if the index specified is negative *)
  let lookup_negative l = match lookup l (-1) with
                          | None -> test_passes
                          | Some a -> test_fails

  (** checks if update returns None if the index is greater than or equal to the size of the input list *)
  let update_bounds (x, l) = match update l (length l) x with
                             | None -> test_passes
                             | Some a -> test_fails

  (** checks if update returns None if the index specified is negative *)
  let update_negative (x, l) = match update l (-1) x with
                          | None -> test_passes
                          | Some a -> test_fails

  (** Checks if update doesn't change the size of the list 
      Requires (length l) >= 1*)
  let update_length (x, l) = match update l 0 x with
                             | None -> test_fails
                             | Some l' -> length l = length l'

  (** Checks if updating a single element to the same value doesn't change the list
      Requires (length l) >= 1*)
  let update_nothing l = 
    let first = match lookup l 0 with
                | None -> failwith "empty list"
                | Some a -> a in
    match update l 0 first with
    | None -> test_fails
    | Some l' -> equals l l'










(** 
equals - list of different lengths (same list, all but last character) shouldn't be equal
          list of same elements but different order, shouldn't be equal

empty - length is 0. DONE

cons -
check that cons actually adds shit to the beginning of the list. (using lookup) DONE
add 2 things on list and make sure that they come out in order.
cons an element -> length + 1 DONE

decons - decons a list of length one: get an empty list. DONE
decons actually takes away first element of list. using cons DONE
decons an element -> length - 1 ALMOST
decons an empty list should return None. DONE

cons and decons have an inverse relationship. DONE

lookup & update - negative input. out of bounds input. DONE
               
lookup -    
lookup 1, after deconsing something, lookup 0 should be some as lookup 1 from b4.

update - lookup before and after update and it should change to new value.
        update shouldn't change length of list. DONE


length -


 *)


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

  let arb_value_listlike : (char * char C.t) Arbitrary.t = 
    Arbitrary.pair Arbitrary.alpha arb_listlike

  let arb_value_listlike_len (n : int) : (char * char C.t) Arbitrary.t = 
    Arbitrary.pair Arbitrary.alpha (arb_listlike_len n)

  (** TODO: additional generators if needed *)

  (****************************************************************************)
  (** Tests *******************************************************************)
  (****************************************************************************)

  TEST_UNIT "equals_self"     = assert_qcheck arb_listlike                equals_self
  TEST_UNIT "equals_symmetry" = assert_qcheck arb_listlike_pair           equals_symmetry


  TEST_UNIT "cons_decons"     = assert_qcheck arb_value_listlike          cons_decons


  TEST_UNIT "empty_length"    = assert_true                               empty_length
  TEST_UNIT "cons_first"      = assert_qcheck arb_value_listlike          cons_first
  TEST_UNIT "cons_length"     = assert_qcheck arb_value_listlike          cons_length
  TEST_UNIT "decons_empty"    = assert_true                               decons_empty
  TEST_UNIT "decons_length"   = assert_qcheck (arb_listlike_len 4)        decons_length
  TEST_UNIT "decons_first"    = assert_qcheck (arb_value_listlike)        decons_first
  TEST_UNIT "decons_one"      = assert_qcheck (arb_listlike_len 1)        decons_one
  TEST_UNIT "lookup_bounds"   = assert_qcheck arb_listlike                lookup_bounds
  TEST_UNIT "lookup_negative" = assert_qcheck arb_listlike                lookup_negative
  TEST_UNIT "update_bounds"   = assert_qcheck arb_value_listlike          update_bounds
  TEST_UNIT "update_negative" = assert_qcheck arb_value_listlike          update_negative
  TEST_UNIT "update_length"   = assert_qcheck (arb_value_listlike_len 4)  update_length
  TEST_UNIT "update_nothing"  = assert_qcheck (arb_listlike_len 2)        update_nothing






  (** TODO: additional tests *)

end

