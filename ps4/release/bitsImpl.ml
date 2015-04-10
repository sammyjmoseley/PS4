
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
  let decons l     = 
    let rec decons_helper l =
      match l with
      | [] -> None
      | Zero::t ->
        (match decons_helper t with
        | None -> None
        | Some (Leaf x, t) -> Some (Leaf x, t)
        | Some (Node(l,r), t) -> Some (l, One(r)::t))
      | One(Leaf x)::t -> Some (Leaf x, Zero::t)
      | One(Node (l, r))::t -> Some (l, One(r)::t) in
    match decons_helper l with
    | None -> None
    | Some (Leaf x, [Zero]) -> Some (x, [])
    | Some (Leaf x, t) -> Some (x, t)
    | _ -> failwith "error"

  let rec lookup l n   = failwith "TODO"

  let update l n x = failwith "TODO"
  let length l     = failwith "TODO"
end

include Core
include ListLike.Extend(Core)

TEST_MODULE "BitsImpl Spec tests" = ListLike.Spec(Core)

