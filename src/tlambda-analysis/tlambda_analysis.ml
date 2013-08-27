open Lambda
open Tlambda
open Tlambda_to_hgraph

open Data
open Int_interv

type v = Vertex.t
type h = Hedge.t
type e = environment
type ha = ( id * hinfo ) list

let intop2_of_prim o =
  let open Int_interv in
  match o with
  | Paddint -> add
  | Psubint -> sub
  | Pmulint -> mul
  | Pdivint -> div
  | Pmodint -> modulo
  | Pandint -> band
  | Porint -> bor
  | Pxorint -> bxor
  | Plslint -> blsl
  | Plsrint -> blsr
  | Pasrint -> basr
  | _ -> assert false

module type Entry =
sig
  val inv : v
  val outv : v
  val exnv : v
end

module M ( E : Entry ) =
struct
  type hedge = h
  type vertex = v
  type abstract = e
  type hedge_atribute = ha

  let bottom _ = bottom_env
  let is_bottom _ = is_bottom_env
  let is_leq _ = is_leq_env
  let join_list _ = List.fold_left join_env bottom_env
  let abstract_init v = if v = E.inv then empty_env else bottom_env


  let apply _ l envs =
  let constant _ = failwith "TODO: constant !" in
    let in_apply ( id, action) env =
      let set x = set_env id x env
      and get x = get_env x env
      and vunit = cp_singleton 0
      and vtrue = cp_singleton 1
      and vfalse = cp_singleton 0
      and vbool_any = cp_any 2
      in
      match action with
      | Var i -> set ( get i)
      | Const c -> set ( constant c)
      | Prim ( p, l) ->
	begin
	  match p, l with
	  | TPidentity, [i] -> set ( get i)
	  | TPignore, _ -> set vunit
	  (* Operations on heap blocks *)
(*	  | TPmakeblock ( tag, _), _ ->
	    let a = Array.of_list l in
	    set ( block_singleton tag ( Ids.singleton ( Array.length a ) a ) ) *) (* SORT IT OUT ! *)
(*	  | TPfield i, [b] ->
	    let env = set_env b ( restrict_block ~has_field:i ( get b)) env in
	    set ( get_field i ( get_env b env) env) (* must restrict b to a block with field at least i *) *)
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
(*	  | TPaddint as op, [x;y]
	  | TPsubint as op, [x;y]
	  | TPmulint as op, [x;y]
	  | TPdivint as op, [x;y]
	  | TPmodint as op, [x;y]
	  | TPandint as op, [x;y]
	  | TPorint as op, [x;y]
	  | TPxorint as op, [x;y]
	  | TPlslint as op, [x;y]
	  | TPlsrint as op, [x;y]
	  | TPasrint as op, [x;y] -> set ( int_op2 ( intop2_of_prim op) (get x) (get y)) *) (* TODO,handle that *)
(*	  | TPintcomp c, [x;y] -> set ( comp c ( get x) ( get y)) *)
(*	  | TPoffsetint of int
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
      | Constraint c -> failwith "TO CORRECT CONSTRAINT"
	(* begin *)
	(*   match c with *)
	(*   | Ccp i  -> set ( restrict_cp (get_env id env) i) *)
	(*   | Ctag t -> set ( restrict_block ( get_env id env) t) *)
	(* end *)
    in	
    let env = Array.fold_left join_env bottom_env envs in
    let rec aux e l =
    match l with
    | [] -> e
    | h :: t -> aux (in_apply h e) t
    in aux env l


end
