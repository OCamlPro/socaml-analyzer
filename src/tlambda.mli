type comparison = Lambda.comparison
and array_kind = Lambda.array_kind
and boxed_integer = Lambda.boxed_integer
and bigarray_kind = Lambda.bigarray_kind
and bigarray_layout = Lambda.bigarray_layout
and compile_time_constant = Lambda.compile_time_constant
and direction_flag = Asttypes.direction_flag


type primitive =
    Pidentity
  | Pignore
  | Prevapply of Location.t
  | Pdirapply of Location.t
  (*   (\* Globals *\) *)
  (* | Pgetglobal of Ident.t *)
  (* | Psetglobal of Ident.t *)
  (* Operations on heap blocks *)
  | Pmakeblock of int * Asttypes.mutable_flag
  | Pfield of int
  | Psetfield of int * bool
  | Pfloatfield of int
  | Psetfloatfield of int
  | Pduprecord of Types.record_representation * int
  (* Force lazy values *)
  | Plazyforce
  (* External call *)
  | Pccall of Primitive.description
  (* (\* Exceptions *\) *) (* moved from Primitive to lambda *)
  (* | Praise *)
  (* Boolean operations *)
  (* | Psequand | Psequor  *)| Pnot
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



type id = Ident.t

type tlambda =
| Tlet of tlet
| Trec of trec
| Tend of id

and tlet =
{
  te_id : id;
  te_lam : tcontrol;
  te_kind : Lambda.let_kind;
  te_in : tlambda;
}

and trec =
{
  tr_decls : ( id * tcontrol) list;
  tr_in : tlambda;
}

and tcontrol =
| Tvar of id
| Tconst of Lambda.structured_constant
| Tapply of id * id * Location.t
| Tprim of primitive * id list
| Tswitch of id * tswitch
| Tstaticraise of int * id list
| Tstaticcatch of tlambda * (int * id list) * tlambda
| Traise of id
| Ttrywith of tlambda * id * tlambda
| Tifthenelse of id * tlambda * tlambda
| Twhile of tlambda * tlambda
| Tfor of id * id * id * direction_flag * tlambda
| Tassign of id * id

and tswitch =
{
  t_numconsts: int;
  t_consts: (int * tlambda) list;
  t_numblocks: int;
  t_blocks: (int * tlambda) list;
  t_failaction: tlambda option;
}
