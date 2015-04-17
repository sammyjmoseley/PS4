
module Core = struct
  type 'a tree    = Leaf of 'a | Node of 'a tree * 'a tree
  type 'a tbit    = Zero | One of 'a tree
  type 'a bitlist = 'a tbit list

  let bitlist_rep_ok bits = failwith "TODO"

  type 'a t = 'a bitlist

  let rep_ok l     =
  	let rec rep_ok_helper l n =
  		let rec tree_depth t =
  			match t with
  			| Leaf(x) -> Some 0
  			| Node(l, r) ->
  				(match (tree_depth l, tree_depth r) with
  				| (Some x, Some y) -> if x=y then Some (x+1) else None
  				| _ -> None) in
  		match l with
  		| [] -> true
  		| Zero::t -> rep_ok_helper t (n+1)
  		| One(h)::t -> (match tree_depth h with
  						| None -> false
  						| Some x -> x=n) && rep_ok_helper t (n+1) in
  	rep_ok_helper l 0

  

  let empty        = []

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
      | One(Node (l, r))::t -> Some (l, One(r)::Zero::t) in
    match decons_helper l with
    | None -> None
    | Some (Leaf x, [Zero]) -> Some (x, [])
    | Some (Leaf x, t) -> Some (x, t)
    | _ -> failwith "error"



  let rec equals l l'  =
  	match (decons l, decons l') with
  	| (Some(h, t), Some(h', t')) -> (h=h') && (equals t t')
  	| (None, None) -> true
  	| _ -> false

  let rec lookup l n   = 
  	let (^) a b = int_of_float ((float_of_int a) ** (float_of_int b)) in
  	let rec eleNum l n (a, b) =
  		match l with
  		| [] -> None
  		| Zero::t ->
  			eleNum t n (a+1, b)
  		| One(x)::t ->
  			let c = 2^a in
  			let (a', b') = (a+1, c+b) in
  			if n<b' then
  				Some (c, x)
  			else
  				eleNum t n (a', b') in
  	let rec travTree t d n =
  		match t with
  		| Leaf x -> if n=0 then Some x else None
  		| Node(l, r) ->
  			let d' = d/2 in
  			if n<d' then
  				travTree l d' n
  			else
  				travTree r d' (n-d') in
  	match eleNum l n (0, 0) with
  	| None -> None
  	| Some (n', t) -> travTree t n' n


  let rec update l n x =
  	if l=[] then
  		None
  	else
  		match decons l with
  		| Some(h, t) ->
  			if n=0 then
  				Some (cons x t)
  			else
  				(match update t (n-1) x with
  				| Some y -> Some (cons x y)
  				| _ -> None)
  		| _ -> None

  
  let rec length l     = 
	match l with
	| [] -> 0
	| Zero::t -> 2*(length t)
	| One(x)::t -> 1+2*(length t)
end

include Core
include ListLike.Extend(Core)

TEST_MODULE "BitsImpl Spec tests" = ListLike.Spec(Core)

