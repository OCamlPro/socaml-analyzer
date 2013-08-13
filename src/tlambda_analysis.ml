open Lambda
open Tlambda
open Tlambda_to_hgraph

open Data

type v = Vertex.t
type h = Hedge.t
type e = environment
type ha = ( id * hinfo ) list

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
      match action with
      | Var i -> set_env id (get_env i env) env
      | Const c -> set_env id (constant c) env
      | Prim ( p, l) ->
	set_env id
	begin
	  match p, l with
	  | Pidentity, [i] -> get_env i env
	  | Pignore, _ -> cp_singleton 0
	  (* Operations on heap blocks *)
	  | Pmakeblock ( tag, _), _ -> block_singleton tag ( Array.of_list l)
	  | Pfield i, [b] -> get_field i ( get_env b env) env
	  | Psetfield ( i, _), [b;v] ->
	  | Pfloatfield of int
	  | Psetfloatfield of int
	  | Pduprecord of Types.record_representation * int
	  (* Force lazy values *)
	  | Plazyforce
	  (* External call *)
	  | Pccall of Primitive.description
	  (* Boolean operations *)
	  | Pnot
	  (* Integer operations *)
	  | Pnegint | Paddint | Psubint | Pmulint | Pdivint | Pmodint
	  | Pandint | Porint | Pxorint
	  | Plslint | Plsrint | Pasrint
	  | Pintcomp of comparison
	  | Poffsetint of int
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
	      
	  | _ -> failwith "TODO: primitives !"
	end env
      | Constraint c ->
	begin
	  match c with
	  | Ccp i  -> set_env id ( restrict_cp (get_env id env) i) env
	  | Ctag t -> set_env id ( restrict_block ( get_env id env) t) env
	end
	
    let env = Array.fold_left join_env bottom_env envs in
    let rec aux e l =
    match l with
    | [] -> e
    | h :: t -> aux (in_apply h e) t
    in aux env l


end
