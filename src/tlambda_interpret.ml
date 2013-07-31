open Ident
open Lambda
open Asttypes
open Tlambda

module Idm = Map.Make ( struct type t = Ident.t let compare = compare end )

type value = v ref
and v =
| Int of int
| Intn of nativeint
| Int32 of int32
| Int64 of int64
| Float of float
| Floata of float array
| String of string
| Cp of int
| Block of int * value list

type env = value Idm.t
type fun_table = tlambda array

let (env_empty:env) = Idm.empty

exception Staticraise of int * value list
exception Exn of value

let id_fun = { stamp = max_int; name = "#switch_f"; flags = 0}
let id_arg = { stamp = pred ( pred max_int); name = "#arg_f"; flags = 0}

let get_env env i = Idm.find i env
let set_env env i v = Idm.add i v env

let rec assign_list e l1 l2 =
  List.fold_left2 set_env e l1 l2

let val_to_bool v = match !v with
  | Cp 0 | Int 0 -> false
  | Cp _ | Int _ -> true
  | _ -> assert false

let val_to_int i = match !i with
  | Int i -> i
  | _ -> assert false

let val_unit = ref ( Cp 0)

let rec tlambda (funs:fun_table) (env:env) = function
  | Tend i -> get_env env i
  | Tlet { te_id; te_lam; te_in }-> 
    let v = tcontrol funs env te_lam in
    let env = set_env env te_id v in
    tlambda funs env te_in
  | Trec _ -> failwith "TODO: rec"

and structured_constant sc =
  ref ( match sc with
  | Const_base c ->
    let open Asttypes in
    begin
      match c with
      | Const_int i -> Int i
      | Const_char c -> Int (Char.code c)
      | Const_string s -> String s
      | Const_float f -> (* Float f *) failwith "TODO: float conversion"
      | Const_int32 i -> Int32 i
      | Const_int64 i -> Int64 i
      | Const_nativeint i -> Intn i
    end
  | Const_pointer i -> Cp i
  | Const_block (i, l) -> Block ( i, List.map structured_constant l)
  | Const_float_array l -> failwith "TODO: float_array const"
  | Const_immstring s -> String s
  )

and tcontrol funs env = function
  | Tvar i -> get_env env i
  | Tconst sc -> structured_constant sc
  | Tapply ( f, x, _) -> call_fun funs (get_env env f) (get_env env x)
  | Tprim ( p, l) -> call_prim funs p (List.map (get_env env) l)
  | Tswitch ( i, s) -> failwith "TODO: switch"
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
      let env = set_env env id (ref ( Int i)) in
      ignore ( tlambda funs env body)
    in
    begin
      if direction = Asttypes.Upto
      then for i =  start to stop do f i done
      else for i =  start downto stop do f i done
    end;
    val_unit
  | Tassign ( id, x) ->
    ( get_env env id) := !( get_env env x);
    val_unit
    
and call_fun funs f x =
  match !f with
  | Block ( 0, ({ contents = Cp i})::tl) ->
    begin
      let body = funs.(i) in
      let e =
	env_empty
	  |> Idm.add id_fun f
	  |> Idm.add id_arg x
      in
      tlambda funs e body
    end
  | _ -> assert false

and call_prim funs p l =
  match p, l with
  | Pidentity, [x] -> x
  | Pignore, _::[] -> val_unit
  | Prevapply _, [x;f] | Pdirapply _, [f;x] -> call_fun funs f x
  | Pmakeblock (i,_), l -> ref ( Block ( i, l))
  | Pfield i, [{ contents = Block ( _, l)}]
  | Pfloatfield i, [{ contents = Block ( _, l)}] ->
    List.nth l i
  | Psetfield ( i, _), [{ contents = Block (_,l)};v]
  | Psetfloatfield i, [{ contents = Block (_,l)};v] ->
    (List.nth l i) := !v; val_unit
  | Pduprecord _, [r] -> ref !r
  | Plazyforce,  _ -> failwith "TODO: lazy" (* not that I'm being lazy *)
  | Pccall _, _ -> failwith "TODO: ccall"

  | Paddint, [{ contents = Int x}; { contents = Int y}] -> ref ( Int ( x + y))

  | _ -> failwith "TODO: primitives"
