module PerformanceTest (C : ListLike.CORE) = struct
	let testOperation op n num_trials= 
		let rec genList size =
			if size > 0 then
				C.cons (Random.int 100) (genList (size - 1))
			else
				C.empty in
		let rec multLists size =
			if size > 0 then
				(genList n)::(multLists (size - 1))
			else
				[] in
		let rec applyOp lst op =
			match lst with
			| [] -> []
			| h::t -> (op h)::(applyOp t op) in
		let lst = multLists num_trials in
		let t_i = Sys.time () in
		let lst' = applyOp lst op in
		let t_f = Sys.time () in
		(t_f -. t_i)

	let testLookUp n num_rep =
		testOperation (fun l -> C.lookup l (n-1)) n num_rep
	let testCons n num_rep =
		testOperation (C.cons 1) n num_rep
	let rec repeatRange a b f =
		if a<b then
			(print_string ((string_of_int a) ^ "," ^ (string_of_float (f a)) ^ "\n"); 
			repeatRange (a+1) b f)
		else ()

end