module Ints = struct
  type t = int
  let compare x y = x - y
end

module IntMap = Map.Make(Ints)

module Core = struct
  type 'a t = 'a IntMap.t

  let rep_ok l = failwith "TODO"

  let equals l l'  = 
    IntMap.equal (=) l l'
  let empty        = (* TODO *) IntMap.empty
  let cons x l     = 
    IntMap.add ((IntMap.cardinal l) + 1) x l
  let decons l     =
    if IntMap.is_empty l then
      None
    else 
      let n = IntMap.cardinal l
      let x = IntMap.find n l in
      let l' = IntMap.remove n l in
      Some (x, l')

  let lookup l n   = 
    if n>=(IntMap.cardinal l) then
      None
    else
      Some (IntMap.find (n+1) l)
  let update l n x = 
    if n>=(IntMap.cardinal l) then
      None
    else
      Some (IntMap.add (n+1) x l)
  let length l     = 
    IntMap.cardinal
end

include Core
include ListLike.Extend(Core)

TEST_MODULE "MapImpl Spec tests" = ListLike.Spec(Core)

