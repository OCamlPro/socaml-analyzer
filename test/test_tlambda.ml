open Lambda
open Asttypes
open Ident
open Tlambda

let last_id = ref (-1)
let mk_id s =
  incr last_id;
  { stamp = !last_id; name = s; flags = 0 }

let ret_id = mk_id "return"
let t1 =
  Tlet
    {
      te_id = ret_id;
      te_lam = Tconst ( Const_base ( Const_int 42));
      te_kind = Alias;
      te_in = Tend ret_id;
    }

let a = mk_id "a"
let b = mk_id "b"

let t2 =
  Tlet
    {
      te_id = a;
      te_lam = Tconst ( Const_base ( Const_int 1605));
      te_kind = Alias;
      te_in =
	Tlet
	  {
	    te_id = b;
	    te_lam = Tconst ( Const_base ( Const_int 1666));
	    te_kind = Alias;
	    te_in =
	      Tlet
		{
		  te_id = ret_id;
		  te_lam = Tprim ( Paddint, [a;b]);
		  te_kind = Alias;
		  te_in = Tend ret_id;
		};
	  };
    }
    
open Tlambda_interpret

let () =
  assert ( !( tlambda [||] env_empty t1) = Int 42);
  assert ( !( tlambda [||] env_empty t2) = Int (1605+1666))
