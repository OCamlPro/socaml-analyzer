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
      ( if xl = minimum || yl = minimum then minimum else xl + yl ),
      ( if xg = maximum || yg = maximum then maximum else  xg + yg ) )

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
    in

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

(* let add_cst c x = *)
(*   match x with *)
(*   | None -> x *)
(*   | Some (a, b) -> *)
(*     Some ( *)
(*       ( if a = min_int then a else a + c), *)
(*       ( if b = max_int then b else b + c) ) *)

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

let make_comp c x y : t * t =
  let open Lambda in
  match x, y with
  | None, _ | _, None -> bottom, bottom
  | Some _, Some _ when is_top x || is_top y ->  x, y
  | Some ( xl, xg ), Some ( yl, yg ) ->
    begin
      match c with
      | Ceq ->
	let l = max xl yl
	and g = min xg yg
	in
	if l <= g
	then ( let r = Some ( l, g ) in r, r )
	else ( bottom, bottom )
      | Cneq ->
	if xl = xg
	then
	  (
	    if xl = yl
	    then
	      if yl = yg
	      then ( bottom, bottom )
	      else ( x, Some ( succ yl, yg ) )
	    else if xg = yg
	    then ( x, Some ( yl, pred yg ) )
	    else ( x, y )
	  )
	else if yl = yg
	then
	  (
	    if yl = xl
	    then ( Some ( succ xl, xg ), y )
	    else if yg = xg
	    then ( Some ( xl, pred xg ), y )
	    else ( x, y )
	  )
	else ( x, y )
      | Clt ->
	if xl >= yg
	then ( bottom, bottom )
	else ( Some ( xl, min xg (pred yg) ), Some ( max yl (succ xl), yg ) )
      | Cgt ->
	if xg <= yl
	then ( bottom, bottom )
	else ( Some ( max xl (succ yl), xg ), Some ( yl, min yg (pred xg) ) )
      | Cle ->
	if xl > yg
	then ( bottom, bottom )
	else ( Some ( xl, min xg yg ), Some ( max yl xl, yg ) )
      | Cge ->
	if xg < yl
	then ( bottom, bottom )
	else ( Some ( max xl yl, xg ), Some ( yl, min yg xg ) )
	
    end
    

let leqcst x c = meet x ( Some ( minimum, c))
let geqcst x c = meet x ( Some ( c, maximum))
