let booleans = (any 2)

let restrict x =
  { bottom with cp = Ints.inter x.cp booleans.cp }

let notb x =
  { bottom with cp =
      Ints.fold
	(fun i res ->
	  match i with
	  | 0 -> Ints.add 1 res
	  | 1 -> Ints.add 0 res
	  | _ -> res ) x.cp Ints.empty;
  }
