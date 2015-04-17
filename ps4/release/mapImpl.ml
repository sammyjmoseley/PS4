module Ints = struct
  type t = int
  let compare x y = x - y
end

module IntMap = Map.Make(Ints)

module Core = struct
  type 'a t = 'a IntMap.t

  let rep_ok l = 
    let rec rep_ok_helper l n = 
      if n < (IntMap.cardinal l) then
        (IntMap.mem n l) && (rep_ok_helper l (n+1))
      else 
        true in
    rep_ok_helper l 0

  let equals l l'  = 
    IntMap.equal (=) l l'
  let empty        = (* TODO *) IntMap.empty
  let cons x l     = 
    IntMap.add (IntMap.cardinal l) x l
  let decons l     =
    if IntMap.is_empty l then
      None
    else 
      let n = IntMap.cardinal l in
      let x = IntMap.find (n-1) l in
      let l' = IntMap.remove (n-1) l in
      Some (x, l')

  let lookup l n   = 
    if n>=(IntMap.cardinal l) || n<0 then
      None
    else
      Some (IntMap.find ((IntMap.cardinal l) - n -1) l)
  let update l n x = 
    if n>=(IntMap.cardinal l) || n<0 then
      None
    else
      Some (IntMap.add ((IntMap.cardinal l) - n -1) x l)
  let length l     = 
    IntMap.cardinal l
end

include Core
include ListLike.Extend(Core)

TEST_MODULE "MapImpl Spec tests" = ListLike.Spec(Core)

