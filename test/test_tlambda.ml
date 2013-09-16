open Common_types
open Lambda
open Asttypes
open Ident
open Tlambda

let tlet te_id te_lam te_in =
  Tlet { te_id; te_lam; te_in; te_kind = Alias }

let last_id = ref (-1)
let mk_id s =
  incr last_id;
  { stamp = !last_id; name = s; flags = 0 }

let ret_id = mk_id "return"

let tend = Tend ret_id

let tconst_int i = Tconst ( Const_base ( Const_int i))
let tadd a b = Tprim ( TPaddint, [a;b])

(* simple test *)
let t1 =
  tlet ret_id ( tconst_int 42) tend

let a = mk_id "a"
let b = mk_id "b"

(* addition test *)
let t2 =
  tlet a ( tconst_int 1605)
    ( tlet b ( tconst_int 1666)
	( tlet ret_id ( tadd a b) tend )
    )

    
open Tlambda_interpret

let nofuns = Hashtbl.create 1

let () =
  assert ( ( tlambda nofuns env_empty t1) = Int 42);
  assert ( ( tlambda nofuns env_empty t2) = Int (1605+1666))
