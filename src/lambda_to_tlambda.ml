open Lambda
open Tlambda

let lambda_to_tlambda last_id code =
  let ir = ref last_id in
  let tlet ?(kind = Strict) id body cont =
    Tlet {
      te_id = id;
      te_lam = body;
      te_kind = kind;
      te_in = cont;
    }
  in
  let mk_id () =
    incr ir;
    Ident.({ name = ""; stamp = !ir; flags = 0 })
  in
  let mk_ids = List.rev_map (fun _ -> mk_id ()) in
  let rec mk_args l ids continuation =
    match l with
      [] -> continuation
    | hd :: tl ->
      let i = List.hd ids in
      let continuation = transl continuation i code in
      mk_args tl (List.tl ids) continuation
  in
  let rec transl continuation id code =
  match code with
    Lvar v ->
      Tlet {
	te_id = id;
	te_lam = Tvar v;
	te_kind = Alias;
	te_in = continuation;
      }
  | Lconst c ->
    Tlet {
      te_id = id;
      te_lam = Tconst c;
      te_kind = Alias;
      te_in = continuation;
    }
  | Lapply ( lam, args, _) ->
    begin
      let lams = lam :: args in
      let ids = List.rev_map (fun _ -> mk_id ()) lams in
      let rec mk_idapps res = function
	  _ :: [] -> res
	| _ :: tl -> mk_idapps tl ( ( mk_id () ) :: res )
	| [] -> assert false
      in
      let idapps = (List.hd ids) :: ( mk_idapps [id] args) in
      let idargs = List.tl ids in
      let rec mk_app args idapps cont =
	match args,idapps with
	| [], _ :: [] -> cont
	| arg::tl,f::(res::_ as tlapps) ->
	  mk_app tl tlapps
	    (
	      Tlet {
		te_id = res;
		te_lam = Tapply ( f, arg);
		te_kind = Strict;
		te_in = cont;
	      }
	    )
	| _,_ -> assert false
      in
      mk_args lams ids
	(
	  mk_app idargs idapps continuation
	)
    end
  | Lfunction _ -> failwith "TODO: function"
  | Llet _ -> failwith "TODO: let"
  | Lletrec _ -> failwith "TODO: letrec"

  (* extracted primitives *)
  | Lprim Praise, [e] ->
    let i = mk_id () in
    transl (
      Tlet {
	te_id = id;
	te_lam = Traise i;
	te_kind = Strict;
	te_in = continuation;
      }
    ) i e
  | Lprim Psequand, [a;b] -> transl continuation id ( Lifthenelse ( a, b, Lconst (Const_pointer 0) ) )
  | Lprim Psequor, [a;b] -> transl continuation id ( Lifthenelse ( a, Lconst (Const_pointer 1), b ) )
  | Lprim p, l ->
    let ids = mk_ids l in
    mk_args l ids (
      Tlet {
	te_id = id;
	te_lam = Tprim ( prim_translate p, ids );
	te_kind = Strict;
	te_in = continuation;
      })
  | Lswitch ( x, s ) ->
    let idx = mk_id () in
    Tlet {
      te_id = idx;
      te_lam = transl (Tend idx) idx x;
      te_kind = Strict;
      te_in = 
	(
	  Tlet {
	    te_id = id;
	    te_lam = Tswitch ( idx, switch_translate s);
	    te_kind = Strict;
	    te_in = continuation;
	  }
	)
    }
  | Lstaticraise ( i, l) ->
    let ids = mk_ids l in
    mk_args l ids (
      Tlet {
	te_id = id;
	te_lam = Tstaticraise (i, ids);
	te_kind = Strict;
	te_in = continuation;
      })
  | Lstaticcatch ( lam, cl, lam2) -> (* try lam with cl -> lam2 *)
    let i = mk_id () in
    let i2 = mk_id () in
    Tlet {
      te_id = id;
      te_lam =
	Tstaticcatch ( transl (Tend i) i lam, cl, transl (Tend i2) i2 lam2 );
      te_kind = Strict;
      te_in = continuation;
    }
  | Ltrywith ( ltry, iexn, lwith) ->
    let i = mk_id () in
    let i2 = mk_id () in
    Tlet {
      te_id = id;
      te_lam =
	Ttrywith ( transl (Tend i) i ltry, iexn, transl (Tend i2) i2 lwith);
      te_kind = Strict;
      te_in = continuation;
    }
  | Lifthenelse ( li, lt, le ) ->
    let i = mk_id () in
    let i1 = mk_id () in
    let i2 = mk_id () in
    Tlet {
      te_id = i;
      te_lam = transl (Tend i) i li;
      te_kind = Strict;
      te_in = 
	Tlet {
	  te_id = id;
	  te_lam = Tifthenelse ( i, transl (Tend i1) i1 lt, transl (Tend i2) i2 le);
	  te_kind = Strict;
	  te_in = continuation;
	};
    }
  | Lsequence ( one, two ) ->
    let i = mk_id () in
    Tlet {
      te_id = i;
      te_lam = transl (Tend i) i one;
      te_kind = Strict;
      te_in = transl continuation id two;
    }
  | Lwhile ( cond, body) ->
    let ic = mk_id () and ib = mk_id () in
    Tlet {
      te_id = i;
      te_lam = Twhile ( transl ( Tend ic) ic cond, transl ( Tend ib) ib body);
      te_kind = Strict;
      te_in = continuation;
    }
  | Lfor ( i, init, final, d, body ) ->
    let i1 = mk_id () in
    let i2 = mk_id () in
    let i3 = mk_id () in
    transl 
      ( transl
	  ( Tlet {
	    te_id = id;
	    te_lam = Tfor 
	      ( i, i1, i2, d,
		transl ( Tend i3) i3 body 
	      );
	    te_kind = Strict;
	    te_in = continuation;
	    }
	  ) i2 final
      ) i1 init
  | Lassign _ -> assert false
  | Lsend ( k, obj, meth, [], _ ) ->
    let ido = mk_id ()
    and idm = mk_id () in
    transl (
      transl ( 
	tlet id ( Tprim ( TPmethod_send k), [ido;idm]) continuation
      )
	idm meth
    ) ido obj
  | Lsend ( k, obj, meth, args, _) ->
    transl continuation id ( Lapply ( Lsend ( k, obj, meth, []), args))
  | Levent ( l, _ )
  | Lifused (_,l) -> transl continuation id l
