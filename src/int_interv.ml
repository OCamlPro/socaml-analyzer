type t = ( int * int ) option
let maximum = max_int
let minimum = min_int

(* Just an idea for later: *)
(* open Big_int *)
(* type t = ( big_int * big_int ) option *)
(* let maximum = big_int_of_int max_int *)
(* let minimum = big_int_of_int min_int *)

let bottom = None
let top = Some ( minimum, maximum)

let is_bottom x = x = None
let is_top x = match x with
    None -> false
  | Some (a,b) -> a <= min_int && b >= max_int

let is_leq x y =
  match x, y with
  | None, _ -> true
  | _, None -> false
  | Some ( a1, b1), Some ( a2, b2) -> a1 >= a2 && b1 <= b2

let join x y =
  match x, y with
  | None, a | a, None -> a
  | Some ( a1, b1), Some ( a2, b2) -> Some (min a1 a2, max b1 b2)

let join_list = List.fold_left join bottom

let widening x y =
  match x, y with
  | None, _ -> y
  | _, None -> assert false
  | Some ( a1, b1), Some ( a2, b2) ->
    Some (
      ( if a2 < a1 then min_int else a2),
      ( if b2 > b1 then max_int else b2))

let print fmt x =
  match x with
    | Some(a,b) ->
	if is_top x then pp_print_string fmt "top"
	else
	  fprintf fmt "[%i,%i]" a b
    | None -> pp_print_string fmt "bot"

let meet x y =
  match x, y with
  | None, _ | _, None -> None
  | Some ( a1, b1), Some ( a2, b2) ->
    let a = max a1 a2
    and b = min b1 b2 in
    if a > b then None else Some ( a, b)

let cst c = Some (c,c)

let addcst x c =
  match x with
  | None -> x
  | Some ( a, b) ->
    if c > 0
    then
      let bc = b+c in
      Some (
	( if a = minimum then minimum else a+c),
	( if bc < b then maximum else bc))
    else
      let ac = a+c in
      Some (
	( if ac > a then minimum else ac),
	( if b = maximum then maximum else b+c))

let uminus x =
  match x with
  | None -> x
  | Some ( a, b) ->
    Some (
      (if b = maximum then minimum else ~-b),
      (if a = minimum then maximum else ~-a))
    


let leqcst x c = meet x ( Some ( minimum, c))
let geqcst x c = meet x ( Some ( c, maximum))

