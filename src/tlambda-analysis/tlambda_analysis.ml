open Common_types
open Lambda
open Tlambda
open Tlambda_to_hgraph
module G = G

open Data
open Int_interv


type v = Vertex.t
type h = Hedge.t
type e = environment
type ha = ( id * hinfo ) list

let intop2_of_prim o =
  let open Int_interv in
  match o with
  | TPaddint -> add
  | TPsubint -> sub
  | TPmulint -> mul
  | TPdivint -> div
  | TPmodint -> modulo
  | TPandint -> band
  | TPorint -> bor
  | TPxorint -> bxor
  | TPlslint -> blsl
  | TPlsrint -> blsr
  | TPasrint -> basr
  | _ -> assert false

let rev_comp = function
  | Ceq -> Cneq | Cneq -> Ceq | Clt -> Cge | Cgt -> Cle | Cle -> Cgt | Cge -> Clt

let may_rev_comp c cp =
  if cp = 0 then rev_comp c else c

let rec constraint_env_cp_var id cp env =
  let d = get_env id env in
  let l = d.expr in
  if has_cp v d
  then
    if is_one_cp d
    then env
    else
      begin
	constraint_env_cp_list l cp env
	|> set_env id (restrict_cp ~v d)
      end
  else bottom_env

and constraint_env_cp_list l cp env =
  List.fold_left
    (fun e expr -> join_env e ( constraint_env_cp expr c env ) )
    bottom_env l

and constraint_env_cp expr cp env =
  match expr with
  | Var x -> constraint_env_cp_var x cp env
  | Prim ( p, l,exnid) ->
    begin
      match p, l with
      | TPintcomp c, [x;y]  ->
	if cp > 1
	then bottom_env
	else
	  let c = may_rev_comp c cp in
	  let x' = get_env x env
	  and y' = get_env y env in
	  let (x',y') = int_make_comp c x' y' in
	  set_env x x' env
      |> set_env y y'
      | TPmakeblock _, _ -> bottom_env
    end
  | _ -> env

let rec constraint_env_tag_var id cp env =
  let d = get_env id env in
  let l = d.expr in
  if has_tag tag d
  then
    if is_one_tag d
    then env
    else
      begin
	constraint_env_tag_list l tag env
	|> set_env id (restrict_block ~tag d)
      end
  else bottom_env

and constraint_env_tag_list l tag env =
  List.fold_left
    (fun e expr -> join_env e ( constraint_env_tag expr tag env ) )
    bottom_env l

and constraint_env_tag expr tag env =
  match expr with
  | Var x -> constraint_env_tag_var x tag env
  | _ -> env

module type Entry =
sig
  val inv : v
  val outv : v
  val exnv : v
  val g : ( unit, ( id * hinfo ) list, unit ) G.graph 
  val funs : ( F.t, Tlambda_to_hgraph.fun_desc ) Hashtbl.t
  val mk_vertex : unit -> v
  val mk_hedge : unit -> Hedge.t
  val return_id : Ident.t
end

module M : functor ( E : Entry ) ->
  Hgraph.Manager with module T := T and module H = G =
			functor ( E : Entry ) ->
struct

  module H = Tlambda_to_hgraph.G

  open H

  type hedge = h
  type vertex = v
  type abstract = e

  let bottom _ = bottom_env
  let is_bottom _ = is_bottom_env
  let is_leq _ = is_leq_env
  let join_list _ = List.fold_left join_env bottom_env
  let abstract_init v = if v = E.inv then empty_env else bottom_env

  type hedge_attribute = ha
  type vertex_attribute = unit
  type graph_attribute = unit

  type function_id = F.t
  module Function_id = F
  let find_function fid =
    let f = Hashtbl.find E.funs fid in
    f.f_graph, {
      sg_input = f.f_in;
      sg_output = f.f_out;
      sg_vertex = f.f_vertex;
      sg_hedge = f.f_hedge;
    }

  let clone_vertex _ = E.mk_vertex ()
  let clone_hedge _ = E.mk_hedge ()


  let apply (_ :hedge ) ( l : hedge_attribute ) ( envs : abstract array ) =
  let constant _ = failwith "TODO: constant !" in
  let in_apply ( id, action) env =
    let set x = set_env id x env
    and get x = get_env x env
    and vunit = cp_singleton 0
    and act d = set_expression d action
    in
    match action with
    | App _ -> assert false
    | Var i -> set ( get i)
    | Const c -> set ( act (constant c) )
    | Prim ( p, l,exnid) ->
      begin
	match p, l with
	| TPidentity, [i] -> set ( get i)
	| TPignore, _ -> set vunit
	  (* Operations on heap blocks *)
	| TPmakeblock ( tag, _), _ ->
	  let a = Array.of_list l in
	  set ( act ( block_singleton tag a ))
	| TPfield i, [b] ->
	  let env = set_env b ( restrict_block ~has_field:i ( get b)) env in
	  set ( get_field i ( get_env b env) env) (* must restrict b to a block with field at least i *)
	  (*	  | TPsetfield ( i, _), [b;v] ->
		  let env = set_env b ( set_field i v ( get b)) env in
		  set_env id vunit env *)
	| TPfloatfield i, [b] -> failwith "TODO: floatfield"
	| TPsetfloatfield i, [b;v] -> failwith "TODO: setfloatfield"
	| TPduprecord (trepr,i), [r] -> failwith "TODO: duprecord"
	  (* Force lazy values *)
	| TPlazyforce, _ -> failwith "TODO: Force lazy"
	  (* External call *)
	| TPccall prim, _ -> failwith "TODO: C-call"
	  (* Boolean operations *)
	| TPnot, [i] -> set ( not_bool ( get i))
	  (* Integer operations *)
	| TPnegint, [i] -> set ( int_op1 uminus ( get i))
	| TPaddint, [x;y]
	| TPsubint, [x;y]
	| TPmulint, [x;y]
	| TPdivint, [x;y]
	| TPmodint, [x;y]
	| TPandint, [x;y]
	| TPorint, [x;y]
	| TPxorint, [x;y]
	| TPlslint, [x;y]
	| TPlsrint, [x;y]
	| TPasrint, [x;y] -> set ( act ( int_op2 ( intop2_of_prim p) (get x) (get y)))
	| TPintcomp c, [x;y] -> 
	  let res, x', y' = int_comp c ( get x) ( get y) in
	  set ( act res)
	|> set_env x x'
	|> set_env y y'
	(*
	  | TPoffsetint of int
	  | TPoffsetref of int
	  (* Float operations *)
	  | TPintoffloat | TPfloatofint
	  | TPnegfloat | TPabsfloat
	  | TPaddfloat | TPsubfloat | TPmulfloat | TPdivfloat
	  | TPfloatcomp of comparison
	  (* String operations *)
	  | TPstringlength | TPstringrefu | TPstringsetu | TPstringrefs | TPstringsets
	  (* Array operations *)
	  | TPmakearray of array_kind
	  | TParraylength of array_kind
	  | TParrayrefu of array_kind
	  | TParraysetu of array_kind
	  | TParrayrefs of array_kind
	  | TParraysets of array_kind
	  (* Test if the argument is a block or an immediate integer *)
	  | TPisint
	  (* Test if the (integer) argument is outside an interval *)
	  | TPisout
	  (* Bitvect operations *)
	  | TPbittest
	  (* Operations on boxed integers (Nativeint.t, Int32.t, Int64.t) *)
	  | TPbintofint of boxed_integer
	  | TPintofbint of boxed_integer
	  | TPcvtbint of boxed_integer (*source*) * boxed_integer (*destination*)
	  | TPnegbint of boxed_integer
	  | TPaddbint of boxed_integer
	  | TPsubbint of boxed_integer
	  | TPmulbint of boxed_integer
	  | TPdivbint of boxed_integer
	  | TPmodbint of boxed_integer
	  | TPandbint of boxed_integer
	  | TPorbint of boxed_integer
	  | TPxorbint of boxed_integer
	  | TPlslbint of boxed_integer
	  | TPlsrbint of boxed_integer
	  | TPasrbint of boxed_integer
	  | TPbintcomp of boxed_integer * comparison
	  (* Operations on big arrays: (unsafe, #dimensions, kind, layout) *)
	  | TPbigarrayref of bool * int * bigarray_kind * bigarray_layout
	  | TPbigarrayset of bool * int * bigarray_kind * bigarray_layout
	  (* size of the nth dimension of a big array *)
	  | TPbigarraydim of int
	  (* load/set 16,32,64 bits from a string: (unsafe)*)
	  | TPstring_load_16 of bool
	  | TPstring_load_32 of bool
	  | TPstring_load_64 of bool
	  | TPstring_set_16 of bool
	  | TPstring_set_32 of bool
	  | TPstring_set_64 of bool
	  (* load/set 16,32,64 bits from a
	  (char, int8_unsigned_elt, c_layout) Bigarray.Array1.t : (unsafe) *)
	  | TPbigstring_load_16 of bool
	  | TPbigstring_load_32 of bool
	  | TPbigstring_load_64 of bool
	  | TPbigstring_set_16 of bool
	  | TPbigstring_set_32 of bool
	  | TPbigstring_set_64 of bool
	  (* Compile time constants *)
	  | TPctconst of compile_time_constant
	  (* byte swap *)
	  | TPbswap16
	  | TPbbswap of boxed_integer
	  (* method call *)
	  | Method_send of Lambda.meth_kind * Location.t (* moved from lambda to primitive *)
	*)	      
	| _ -> failwith "TODO: primitives !"
      end
    | Constraint c ->
      begin
	match c with
	| Ccp v  ->
	  constraint_env_cp_var id cp env
	| Ctag tag ->
	  constraint_env_tag_var id cp env
      end
  in	
  let env = Array.fold_left join_env bottom_env envs in
  let rec aux e l =
    match l with
    | [] -> e
    | h :: t -> aux (in_apply h e) t
  in 
  match l with
  | [ id, ( App ( f, x ) ) ] ->
    ( [| set_env id (get_env E.return_id env) env; env |], ( fun_ids f env ) )
  | _ -> [|aux env l|], []

end
