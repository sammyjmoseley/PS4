
module Core = struct
  type 'a t = 'a list

  let equals l l'  = 
  	match (l, l') with
  	| ([], []) -> true
  	| (h::t, h'::t') -> (h==t) && (equals t t')
  	| _ -> false
  let empty        = []

  let cons x l     = 
  	x::l
  let decons l     =
  	match l with
  	| [] -> None
  	| h::t -> Some (h, t)

  let lookup l n   =
  	match l with
  	| [] -> None
  	| h::t -> if n==0 then Some h else lookup t (n-1)

  let update l n x =
  	match l with
  	| [] -> None
  	| h::t -> if n==0 then Some x::t else h::(update l (n-1) x)

  let length l     = 
  	match l with
  	| [] -> 0
  	| h::t -> 1 + (length t)
end

include Core
include ListLike.Extend(Core)

TEST_MODULE "ListImpl Spec tests" = ListLike.Spec(Core)

