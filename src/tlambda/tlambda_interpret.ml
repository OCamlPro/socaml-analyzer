open Ident
open Lambda
open Asttypes
open Tlambda
open Common_types

module Idm = Map.Make ( struct type t = Ident.t let compare = compare end )

type v =
| Int of int
| Intn of nativeint
| Int32 of int32
| Int64 of int64
| Float of float
| Floata of float array
| String of string
| Cp of int
| Block of int * id ref list
| Fun of F.t * id list
| Unknown

type env = v Idm.t
type fun_table = ( F.t, tlambda ) Hashtbl.t

let (env_empty:env) = Idm.empty

exception Staticraise of int * v list
exception Exn of v

let id_fun = { stamp = max_int; name = "#switch_f"; flags = 0}
let id_arg = { stamp = pred ( pred max_int); name = "#arg_f"; flags = 0}

let get_env env i = Idm.find i env
let set_env env i v = Idm.add i v env

let rec assign_list e l1 l2 =
  List.fold_left2 set_env e l1 l2

let val_to_bool v = match v with
  | Cp 0 | Int 0 -> false
  | Cp _ | Int _ -> true
  | _ -> assert false

let val_to_int i = match i with
  | Int i
  | Cp i -> i
  | _ -> assert false

let of_bool b = Cp ( if b then 1 else 0)
let of_int i = Int i

let val_unit = Cp 0
let match_failure = Exn val_unit (* This is indeed false *) (* TODO *)

let rec tlambda (funs:fun_table) (env:env) = function
  | Tend i -> get_env env i
  | Tlet { te_id; te_lam; te_in }-> 
    let v = tcontrol funs env te_lam in
    let env = set_env env te_id v in
    tlambda funs env te_in
  | Trec { tr_decls; tr_in; }  ->
    let env =
      List.fold_left
        (fun env (i,p,l) ->
           set_env env i ( call_prim env funs p l )
        )
        env tr_decls in
    tlambda funs env tr_in

and structured_constant = function
  | Const_base c ->
    let open Asttypes in
    begin
      match c with
      | Const_int i -> Int i
      | Const_char c -> Int (Char.code c)
      | Const_string s -> String s
      | Const_float f -> Float ( float_of_string f)
      | Const_int32 i -> Int32 i
      | Const_int64 i -> Int64 i
      | Const_nativeint i -> Intn i
    end
  | Const_pointer i -> Cp i
  | Const_block (i, l) -> failwith "TODO: const_block"
    (* Block ( i, List.map structured_constant l) *)
  | Const_float_array l -> failwith "TODO: float_array const"
  | Const_immstring s -> String s
  

and tcontrol funs env = function
  | Tvar i -> get_env env i
  | Tconst sc -> structured_constant sc
  | Tapply ( f, x) -> call_fun funs (get_env env f) (get_env env x)
  | Tprim ( p, l) -> call_prim env funs p l
  | Tswitch ( i, s) ->
    let switch_handle i l =
      let b =
	try List.assoc i l with
	  Not_found ->
	    ( match s.t_failaction with
	    | Some b -> b
	    | None -> raise match_failure)
      in
      tlambda funs env b
    in
    begin
      match get_env env i with
      | Int i | Cp i -> switch_handle i s.t_consts
      | Block (i,l) -> switch_handle i s.t_blocks
      | _ -> assert false
    end
  | Tstaticraise ( i, l) -> raise ( Staticraise ( i, List.map (get_env env) l))
  | Tstaticcatch ( lam, (i,l), lam2) ->
    begin
      try tlambda funs env lam with
      | Staticraise ( i2, l2) when i2 = i ->
	let env = assign_list env l l2 in
	tlambda funs env lam2
    end
  | Traise i -> raise ( Exn ( get_env env i))
  | Ttrywith ( lam, i, lam2) ->
      begin
	try tlambda funs env lam with
	  Exn v ->
	    let env = set_env env i v in
	    tlambda funs env lam2
      end
  | Tifthenelse ( i, l1, l2) ->
    if val_to_bool ( get_env env i)
    then tlambda funs env l1
    else tlambda funs env l2
  | Twhile ( cond, body) ->
    while ( val_to_bool ( tlambda funs env cond))
    do ignore ( tlambda funs env body) done;
    val_unit
  | Tfor ( id, start, stop, direction, body) ->
    let start = val_to_int ( get_env env start) in
    let stop = val_to_int ( get_env env stop) in
    let f i =
      let env = set_env env id (Int i) in
      ignore ( tlambda funs env body)
    in
    begin
      if direction = Asttypes.Upto
      then for i =  start to stop do f i done
      else for i =  start downto stop do f i done
    end;
    val_unit
  | Tlazyforce _ -> Unknown
  | Tccall _ -> Unknown
  | Tsend _ -> Unknown

    
and call_fun funs f x =
  match f with
  | Fun ( i, l ) ->
    begin
      let body = Hashtbl.find funs i in
      let e =
	env_empty
 |> Idm.add id_fun f
 |> Idm.add id_arg x
      in
      tlambda funs e body
    end
  | _ -> assert false

and call_prim env funs p l =
  let g i = get_env env i in
  match p, l with
  (* Utilities *)
  (* Blocks *)
  | TPmakeblock (i,_), _ ->  ( Block ( i, List.map ref l))
  | TPfield i, [b]
  | TPfloatfield i, [b] ->
    begin
      match g b with
      | Block ( _, l) ->
        g !( List.nth l i )
      | _ -> assert false
    end
  | TPsetfield ( i, _), [b;v]
  | TPsetfloatfield i, [b;v] ->
    begin
      match g b with
      | Block (_,l) ->
        (List.nth l i) := v; val_unit
      | _ -> assert false
    end
  | TPduprecord _, [i] ->
    begin
      match g i with
      |  Block (i,l) ->
        Block (i, List.map (fun i -> ref !i) l )
      | _ -> assert false
    end
  (* Booleans *)
  | TPnot, [b] -> of_bool ( not (val_to_bool (g b)))
  (* Ints *)
  | TPnegint, [x] -> of_int ( ~- (val_to_int (g x)))
  | TPaddint, [ x; y] -> of_int ( ( val_to_int (g x)) + ( val_to_int (g y)))
  | TPsubint, [ x; y] -> of_int ( ( val_to_int (g x)) - ( val_to_int (g y)))
  | TPmulint, [ x; y] -> of_int ( ( val_to_int (g x)) * ( val_to_int (g y)))
  | TPdivint, [ x; y] -> of_int ( ( val_to_int (g x)) / ( val_to_int (g y)))
  | TPmodint, [ x; y] -> of_int ( ( val_to_int (g x)) mod ( val_to_int (g y)))
  | TPandint, [ x; y] -> of_int ( ( val_to_int (g x)) land ( val_to_int (g y)))
  | TPorint, [ x; y] -> of_int ( ( val_to_int (g x)) lor ( val_to_int (g y)))
  | TPxorint, [ x; y] -> of_int ( ( val_to_int (g x)) lxor ( val_to_int (g y)))
  | TPlslint, [ x; y] -> of_int ( ( val_to_int (g x)) lsl ( val_to_int (g y)))
  | TPlsrint, [ x; y] -> of_int ( ( val_to_int (g x)) lsr ( val_to_int (g y)))
  | TPasrint, [ x; y] -> of_int ( ( val_to_int (g x)) asr ( val_to_int (g y)))
  | TPintcomp c, [ x; y] -> of_bool ( comparison c ( val_to_int (g x))  ( val_to_int (g y)))
  | TPoffsetint _, _ -> failwith "TODO: ask Pierre"
  | TPoffsetref _, _ -> failwith "TODO: ask Pierre"
  (* Floats *)
  
 
  | TPfun i, _ -> Fun ( i, (* List.map g *) l )
  | TPfunfield i, [] ->
    begin
      match g id_fun with
      | Fun ( _, l) -> g ( List.nth l i )
      | _ -> assert false
    end
  | _, _ -> failwith "TODO: primitives"

and comparison = function
  | Ceq -> (=)
  | Cneq -> (<>)
  | Clt -> (<)
  | Cgt -> (>)
  | Cle -> (<=)
  | Cge -> (>=)



