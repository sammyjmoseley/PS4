
module Core = struct
  type 'a t = 'a list

  let rec equals l l'  = 
  	match (l, l') with
  	| ([], []) -> true
  	| (h::t, h'::t') -> (h=h') && (equals t t')
  	| _ -> false
  let empty        = []

  let cons x l     = 
  	x::l
  let decons l     =
  	match l with
  	| [] -> None
  	| h::t -> Some (h, t)

  let rec lookup l n   =
  	match l with
  	| [] -> None
  	| h::t -> if n==0 then Some h else lookup t (n-1)

  let rec update l n x =
  	match l with
  	| [] -> None
  	| h::t ->
      if n=0 then
        Some (cons x t)
      else
        match update t (n-1) x with
        | Some y -> Some (cons x y)
        | _ -> None

  let rec length l     = 
  	match l with
  	| [] -> 0
  	| h::t -> 1 + (length t)
end

include Core
include ListLike.Extend(Core)

TEST_MODULE "ListImpl Spec tests" = ListLike.Spec(Core)

