open Data

(* ints *)

let restrict_intcp x = { bottom with int = x.int; cp = x.cp; }

let restrict_not_intcp x =
  { x with int = bottom.int; cp = bottom.cp; }

let singleton const =
  { bottom with
    int = Int_interv.cst const;
  }

let any =
  { bottom with int = Int_interv.top;  }

let join x y =
  { bottom with int = Int_interv.join x.int y.int; }

let add x y =
  { bottom with int = Int_interv.add x.int y.int }

let op1 ( f : Int_interv.t -> Int_interv.t) x =
  { bottom with int = f x.int }

let op2 ( f : Int_interv.t -> Int_interv.t -> Int_interv.t) x y =
  { bottom with int = f x.int y.int }

let comp c x y =
  begin
    match Int_interv.comp c x.int y.int with
    | Some true -> { bottom with cp = Ints.singleton 1 }
    | Some false -> { bottom with cp = Ints.singleton 0 }
    | None -> { bottom with cp = Ints.add 1 ( Ints.singleton 0 ) }
  end,
    restrict_intcp x,
    restrict_intcp y

let make_comp c x y =
  let xi, yi = Int_interv.make_comp c x.int y.int in
  { x with int = xi }, { y with int = yi }

let is_int env d =
  let res = Ints.empty in
  let res =
    if Ints.is_empty d.cp && Int_interv.is_bottom d.int
    then res
    else Ints.add 1 res
  in
  let res =
    if is_bottom env { d with int = bottom.int; cp = bottom.cp; }
    then res
    else Ints.add 0 res
  in
  { bottom with cp = res ; }

let is_out x a b =
  failwith "is_out"
