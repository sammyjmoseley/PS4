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
		let lst = multLists 100 in
		let t_i = Sys.time in
		let lst' = map op lst in
		let t_f = Sys.time in
		t_f-t_i

end