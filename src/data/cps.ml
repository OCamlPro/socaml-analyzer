let any i =
  let rec aux res = function
    | 0 -> res
    | n	-> let n = pred n in aux (Ints.add n res) n
  in { bottom with cp = aux Ints.empty i }

let singleton i =
  { bottom with cp = Ints.singleton i }


let has v d = Ints.mem v d.cp

let is_one d env =
  Ints.cardinal d.cp = 1 &&
  is_bottom env { d with cp = bottom.cp }

let restrict ?v d =
  match v with
    Some v -> singleton v
  | None -> { bottom with cp = d.cp }


