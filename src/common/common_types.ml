open Utils

module Id =
struct
  open Ident
  type t = Ident.t
  let compare = compare
  let name x = Some x.name
  let to_string x = Printf.sprintf "%s/%d" x.name x.stamp
  let output o x = Printf.fprintf o "%s/%d" x.name x.stamp
  let print = print
  let idref = ref 0
  let create ?(name="") () =
    decr idref;
    { stamp = !idref; name = ( "$$" ^ name ); flags = 0; }
end

let builtin id =
  let s = id.Ident.stamp in
  s > 0 && s < 1000

module F = MakeId(struct end)

type id = Id.t

type tid = string * id
(* a tlambda id is a module * an id *)
(* empty module means builtin or special *)
(* "Module", { 0; "#self"; 0 } means a global module *)

module TId =
struct
  open Ident
  type t = tid
  let compare = compare
  let name (m,x) = Some ( m ^ x.name )
  let to_string (m,x) = Printf.sprintf "%s.%s/%d" m x.name x.stamp
  let output o (m,x) = Printf.fprintf o "%s.%s/%d" m x.name x.stamp
  let print = print
  let idref = ref 0
  let create ?(name="") () =
    decr idref;
    ( "", { stamp = !idref; name = ( "$$" ^ name ); flags = 0; } )
end


type comparison = Lambda.comparison
and array_kind = Lambda.array_kind
and boxed_integer = Lambda.boxed_integer
and bigarray_kind = Lambda.bigarray_kind
and bigarray_layout = Lambda.bigarray_layout
and compile_time_constant = Lambda.compile_time_constant
and direction_flag = Asttypes.direction_flag


type primitive =
  | TPbuiltin
(* Operations on heap blocks *)
| TPmakeblock of int * Asttypes.mutable_flag
| TPfield of int
| TPsetfield of int * bool
| TPfloatfield of int
| TPsetfloatfield of int
| TPduprecord of Types.record_representation * int
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
(* function operations *)
| TPfun of F.t
| TPgetfun of F.t
| TPfunfield of int
| TPgetarg


type hinfo =
| Var of tid
| Const of Lambda.structured_constant
| Prim of primitive * tid list
| Constraint of constr
| App of tid * tid (* function, argument *)
| Return of tid | Retexn of tid (* the function exit *)
| Lazyforce of tid
| Ccall of Primitive.description * tid list
| Send of tid * tid
and constr = Ccp of int | Ctag of int
