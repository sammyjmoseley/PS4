
module Core = struct
  type 'a tree    = Leaf of 'a | Node of 'a tree * 'a tree
  type 'a tbit    = Zero | One of 'a tree
  type 'a bitlist = 'a tbit list

  let bitlist_rep_ok bits = failwith "TODO"

  type 'a t = 'a bitlist

  let rep_ok l     = failwith "TODO"

  let equals l l'  = failwith "TODO"
  let empty        = (* TODO *) []
  let cons x l     = 
    let rec cons_helper x l = 
      match l with
      | [] -> [One(x)]
      | Zero::t -> One(x)::t
      | (One h)::t -> Zero::(cons_helper (Node (x, h)) t) in
    cons_helper (Leaf x) l
  let decons l     =  failwith "TODO"
    (* let rec decons_helper l =
      match l with
      | [] -> None
      | Zero::t ->
      | One(Leaf x)::t -> (Leaf x, Zero::t)
      | One(Node (l, r)) -> (l,) *)

  let rec lookup l n   = failwith "TODO"

  let update l n x = failwith "TODO"
  let length l     = failwith "TODO"
end

include Core
include ListLike.Extend(Core)

TEST_MODULE "BitsImpl Spec tests" = ListLike.Spec(Core)

