module PerformanceTest (C : Core) = struct
	let testOperation op n = 
		let rec genList size =
			if size > 0 then
				Core.cons (Random.int 100) (genList (size - 1))
			else
				Core.empty in
		let rec multLists size =
			if size > 0 then
				(genList n)::(multLists (size - 1))
			else
				[] in
		let num_trials = 100 in
		let lst = multLists num_trials in
		let t_i = Sys.time in
		let lst' = map op lst in
		let t_f = Sys.time in
		(t_f-t_i)/(float_of_int num_trials)
	let testLookUp file_name =
		let sizes = [10, 20, 50, 100, 1000, 10000] in
		let testSize n =
			testOperation (C.lookup (Random.int n)) n in
		let test n_rep size =
			let rec testHelper n s =
				if n > 0 then
					testSize s + testHelper (n-1) s
				else
					0.0 in
			(testHelper n_rep size)/(float_of_int n_rep) in
		map (fun x -> (x, test (x/2) x)) sizes
end

include Core