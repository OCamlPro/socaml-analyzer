type comparison = Lambda.comparison
and array_kind = Lambda.array_kind
and boxed_integer = Lambda.boxed_integer
and bigarray_kind = Lambda.bigarray_kind
and bigarray_layout = Lambda.bigarray_layout
and compile_time_constant = Lambda.compile_time_constant
and direction_flag = Asttypes.direction_flag


type primitive =
| TPidentity
| TPignore
| TPrevapply
| TPdirapply
(* Operations on heap blocks *)
| TPmakeblock of int * Asttypes.mutable_flag
| TPfield of int
| TPsetfield of int * bool
| TPfloatfield of int
| TPsetfloatfield of int
| TPduprecord of Types.record_representation * int
(* Force lazy values *)
| TPlazyforce
(* External call *)
| TPccall of Primitive.description
(* Boolean operations *)
| TPnot
(* Integer operations *)
| TPnegint | TPaddint | TPsubint | TPmulint | TPdivint | TPmodint
| TPandint | TPorint | TPxorint
| TPlslint | TPlsrint | TPasrint
| TPintcomp of comparison
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
| TPmethod_send of Lambda.meth_kind (* moved from lambda to primitive *)
(* function operations *)
| TPfun of int
| TPfunfield of int



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
  tr_decls : ( id * primitive * id list ) list;
  tr_in : tlambda;
}

and tcontrol =
| Tvar of id
| Tconst of Lambda.structured_constant
| Tapply of id * id
| Tprim of primitive * id list
| Tswitch of id * tswitch
| Tstaticraise of int * id list
| Tstaticcatch of tlambda * (int * id list) * tlambda
| Traise of id
| Ttrywith of tlambda * id * tlambda
| Tifthenelse of id * tlambda * tlambda
| Twhile of tlambda * tlambda
| Tfor of id * id * id * direction_flag * tlambda
(* | Tassign of id * id *)

and tswitch =
{
  t_numconsts: int;
  t_consts: (int * tlambda) list;
  t_numblocks: int;
  t_blocks: (int * tlambda) list;
  t_failaction: tlambda option;
}
