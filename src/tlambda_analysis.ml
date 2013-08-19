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
	  | Pidentity, [i] -> set ( get i)
	  | Pignore, _ -> set vunit
	  (* Operations on heap blocks *)
	  | Pmakeblock ( tag, _), _ -> set ( block_singleton tag ( Array.of_list l))
	  | Pfield i, [b] ->
	    let env = set_env b ( restrict_block ~has_field:i ( get b)) env in
	    set ( get_field i ( get_env b env) env) (* must restrict b to a block with field at least i *)
	  | Psetfield ( i, _), [b;v] ->
	    let env = set_env b ( set_field i v ( get b)) env in
	    set_env id vunit env
	  | Pfloatfield i, [b] -> failwith "TODO: floatfield"
	  | Psetfloatfield i, [b,v] -> failwith "TODO: setfloatfield"
	  | Pduprecord (trepr,i), [r] -> failwith "TODO: duprecord"
	  (* Force lazy values *)
	  | Plazyforce -> failwith "TODO: Force lazy"
	  (* External call *)
	  | Pccall prim, _ -> failwith "TODO: C-call"
	  (* Boolean operations *)
	  | Pnot, [x] -> set ( not_bool ( get i))
	  (* Integer operations *)
	  | Pnegint, [i] -> set ( int_op1 uminus ( get i))
	  | Paddint as op, [x;y]
	  | Psubint as op, [x;y]
	  | Pmulint as op, [x;y]
	  | Pdivint as op, [x;y]
	  | Pmodint as op, [x;y]
	  | Pandint as op, [x;y]
	  | Porint as op, [x;y]
	  | Pxorint as op, [x;y]
	  | Plslint as op, [x;y]
	  | Plsrint as op, [x;y]
	  | Pasrint as op, [x;y] -> set ( int_op2 ( intop2_of_prim op) (get x) (get y))
	  | Pintcomp c, [x;y] -> set ( comp c ( get x) ( get y))
(*	  | Poffsetint of int
	  | Poffsetref of int
	  (* Float operations *)
	  | Pintoffloat | Pfloatofint
	  | Pnegfloat | Pabsfloat
	  | Paddfloat | Psubfloat | Pmulfloat | Pdivfloat
	  | Pfloatcomp of comparison
	  (* String operations *)
	  | Pstringlength | Pstringrefu | Pstringsetu | Pstringrefs | Pstringsets
	  (* Array operations *)
	  | Pmakearray of array_kind
	  | Parraylength of array_kind
	  | Parrayrefu of array_kind
	  | Parraysetu of array_kind
	  | Parrayrefs of array_kind
	  | Parraysets of array_kind
	  (* Test if the argument is a block or an immediate integer *)
	  | Pisint
	  (* Test if the (integer) argument is outside an interval *)
	  | Pisout
	  (* Bitvect operations *)
	  | Pbittest
	  (* Operations on boxed integers (Nativeint.t, Int32.t, Int64.t) *)
	  | Pbintofint of boxed_integer
	  | Pintofbint of boxed_integer
	  | Pcvtbint of boxed_integer (*source*) * boxed_integer (*destination*)
	  | Pnegbint of boxed_integer
	  | Paddbint of boxed_integer
	  | Psubbint of boxed_integer
	  | Pmulbint of boxed_integer
	  | Pdivbint of boxed_integer
	  | Pmodbint of boxed_integer
	  | Pandbint of boxed_integer
	  | Porbint of boxed_integer
	  | Pxorbint of boxed_integer
	  | Plslbint of boxed_integer
	  | Plsrbint of boxed_integer
	  | Pasrbint of boxed_integer
	  | Pbintcomp of boxed_integer * comparison
	  (* Operations on big arrays: (unsafe, #dimensions, kind, layout) *)
	  | Pbigarrayref of bool * int * bigarray_kind * bigarray_layout
	  | Pbigarrayset of bool * int * bigarray_kind * bigarray_layout
	  (* size of the nth dimension of a big array *)
	  | Pbigarraydim of int
	  (* load/set 16,32,64 bits from a string: (unsafe)*)
	  | Pstring_load_16 of bool
	  | Pstring_load_32 of bool
	  | Pstring_load_64 of bool
	  | Pstring_set_16 of bool
	  | Pstring_set_32 of bool
	  | Pstring_set_64 of bool
	  (* load/set 16,32,64 bits from a
	     (char, int8_unsigned_elt, c_layout) Bigarray.Array1.t : (unsafe) *)
	  | Pbigstring_load_16 of bool
	  | Pbigstring_load_32 of bool
	  | Pbigstring_load_64 of bool
	  | Pbigstring_set_16 of bool
	  | Pbigstring_set_32 of bool
	  | Pbigstring_set_64 of bool
	  (* Compile time constants *)
	  | Pctconst of compile_time_constant
	  (* byte swap *)
	  | Pbswap16
	  | Pbbswap of boxed_integer
	  (* method call *)
	  | Method_send of Lambda.meth_kind * Location.t (* moved from lambda to primitive *)
*)	      
	  | _ -> failwith "TODO: primitives !"
	end env
      | Constraint c ->
	begin
	  match c with
	  | Ccp i  -> set ( restrict_cp (get_env id env) i)
	  | Ctag t -> set ( restrict_block ( get_env id env) t)
	end
	
    let env = Array.fold_left join_env bottom_env envs in
    let rec aux e l =
    match l with
    | [] -> e
    | h :: t -> aux (in_apply h e) t
    in aux env l


end
