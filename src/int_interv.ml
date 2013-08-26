type t = ( int * int ) option
let maximum = max_int
let minimum = min_int

(* Just an idea for later: *)
(* open Big_int *)
(* type t = ( big_int * big_int ) option *)
(* let maximum = big_int_of_int max_int *)
(* let minimum = big_int_of_int min_int *)

let bottom : t= None
let top : t = Some ( minimum, maximum)

let is_bottom x = x = bottom
let is_top x = match (x : t) with
    None -> false
  | Some (a,b) -> a <= min_int && b >= max_int

let is_leq x y =
  match (x : t), (y : t) with
  | None, _ -> true
  | _, None -> false
  | Some ( a1, b1), Some ( a2, b2) -> a1 >= a2 && b1 <= b2

let join (x:t) (y:t) =
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
  let open Format in
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
    
let add x y =
  match x, y with
  | None, _ | _, None -> None
  | Some ( xl, xg), Some ( yl, yg) ->
    Some (
      if xl = minimum || yl = minimum then minimum else xl + yl,
      if xg = maximum || yg = maximum then maximum else  xg + yg)

let sub x y = add x (uminus y)
let mul x y = match x, y with
  | None, _ | _, None -> None
  | Some ( xl, xg), Some ( yl, yg) ->
    let aux x y =
      if x = 0 || y = 0
      then 0
      else
	if ( x = minimum && y < 0 ) || ( x = maximum && y > 0 ) || ( y = minimum && x < 0 ) || ( y = maximum && x > 0 ) 
	then maximum
	else
	  if ( x = maximum && y < 0 ) || ( x = minimum && y > 0 ) || ( y = maximum && x < 0 ) || ( y = minimum && x > 0 )
	  then minimum
	  else x * y

    let xlyl = aux xl yl
    and xlyg = aux xl yg
    and xgyl = aux xg yl
    and xgyg = aux xg yg in
    Some ( min ( min xlyl xlyg) ( min xgyl xgyg), max ( max xlyl xlyg) ( max xgyl xgyg))
let div x y = top
let modulo x y = top
let band x y = top
let bor x y = top
let bxor x y = top
let blsl x y = top
let blsr x y = top
let basr x y = top

type compres = bool option

let comp c x y =
  let open Lambda in
  let test_eq xl xg yl yg =
    if xl = xg && yl = yg
    then Some ( xl = yl)
    else if xg < yl || yg < xl
    then Some false
    else None
  in
  let test_lt xl xg yl yg =
    if xg < yl
    then Some true
    else if xl >= yg
    then Some false
    else None
  in
  let test_le xl xg yl yg =
    if xg <= yl
    then Some true
    else if xl > yg
    then Some false
    else None
  in
  match x, y with
  | None, _ | _, None -> None
  | Some ( xl, xg), Some ( yl, yg) ->
    begin
      match c with
      | Ceq -> test_eq xl xg yl yg
      | Cneq ->
	(match test_eq xl xg yl yg with
	| None -> None
	| Some b -> Some (not b))
      | Clt -> test_lt xl xg yl yg
      | Cgt -> test_lt yl yg xl yg
      | Cle -> test_le xl xg yl yg
      | Cge -> test_le yl yg xl yg
    end
      

let leqcst x c = meet x ( Some ( minimum, c))
let geqcst x c = meet x ( Some ( c, maximum))
