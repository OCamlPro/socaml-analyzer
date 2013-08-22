open Lambda
open Tlambda

module Id =
struct type t = Ident.t let compare = compare end

module Imap = Map.Make ( Id )
module Iset = Set.Make ( Id )

let prim_translate = function
  | Pidentity -> TPidentity
  | Pignore -> TPignore
  (* Operations on heap blocks *)
  | Pmakeblock ( i, m) -> TPmakeblock ( i, m)
  | Pfield i -> TPfield i
  | Psetfield ( i, b) -> TPsetfield ( i, b)
  | Pfloatfield i -> TPfloatfield i
  | Psetfloatfield i -> TPsetfloatfield i
  | Pduprecord ( t, i ) -> TPduprecord ( t, i )
  (* Force lazy values *)
  | Plazyforce -> TPlazyforce
  (* External call *)
  | Pccall p -> TPccall p
  (* Boolean operations *)
  | Pnot -> TPnot
  (* Integer operations *)
  | Pnegint -> TPnegint
  | Paddint -> TPaddint
  | Psubint -> TPsubint
  | Pmulint -> TPmulint
  | Pdivint -> TPdivint
  | Pmodint -> TPmodint
  | Pandint -> TPandint
  | Porint -> TPorint
  | Pxorint -> TPxorint
  | Plslint -> TPlslint
  | Plsrint -> TPlsrint
  | Pasrint -> TPasrint
  | Pintcomp c -> TPintcomp c
  | Poffsetint i -> TPoffsetint i
  | Poffsetref i -> TPoffsetref i
  (* Float operations *)
  | Pintoffloat -> TPintoffloat
  | Pfloatofint -> TPfloatofint
  | Pnegfloat -> TPnegfloat
  | Pabsfloat -> TPabsfloat
  | Paddfloat -> TPaddfloat
  | Psubfloat -> TPsubfloat
  | Pmulfloat -> TPmulfloat
  | Pdivfloat -> TPdivfloat
  | Pfloatcomp c -> TPfloatcomp c
  (* String operations *)
  | Pstringlength -> TPstringlength
  | Pstringrefu -> TPstringrefu
  | Pstringsetu -> TPstringsetu
  | Pstringrefs -> TPstringrefs
  | Pstringsets -> TPstringsets
  (* Array operations *)
  | Pmakearray k -> TPmakearray k
  | Parraylength k -> TParraylength k
  | Parrayrefu k -> TParrayrefu k
  | Parraysetu k -> TParraysetu k
  | Parrayrefs k -> TParrayrefs k
  | Parraysets k -> TParraysets k
  (* Test if the argument is a block or an immediate integer *)
  | Pisint -> TPisint
  (* Test if the (integer) argument is outside an interval *)
  | Pisout -> TPisout
  (* Bitvect operations *)
  | Pbittest -> TPbittest
  (* Operations on boxed integers (Nativeint.t, Int32.t, Int64.t) *)
  | Pbintofint k -> TPbintofint k
  | Pintofbint k -> TPintofbint k
  | Pcvtbint ( ksource, kdest ) -> TPcvtbint ( ksource, kdest )
  | Pnegbint k -> TPnegbint k
  | Paddbint k -> TPaddbint k
  | Psubbint k -> TPsubbint k
  | Pmulbint k -> TPmulbint k
  | Pdivbint k -> TPdivbint k
  | Pmodbint k -> TPmodbint k
  | Pandbint k -> TPandbint k
  | Porbint k -> TPorbint k
  | Pxorbint k -> TPxorbint k
  | Plslbint k -> TPlslbint k
  | Plsrbint k -> TPlsrbint k
  | Pasrbint k -> TPasrbint k
  | Pbintcomp ( k, c ) -> TPbintcomp ( k, c )
  (* Operations on big arrays: (unsafe, #dimensions, kind, layout) *)
  | Pbigarrayref ( b, i, k, l ) -> TPbigarrayref ( b, i, k, l )
  | Pbigarrayset ( b, i, k, l ) -> TPbigarrayset ( b, i, k, l )
  (* size of the nth dimension of a big array *)
  | Pbigarraydim i -> TPbigarraydim i
  (* load/set 16,32,64 bits from a string: (unsafe)*)
  | Pstring_load_16 b -> TPstring_load_16 b
  | Pstring_load_32 b -> TPstring_load_32 b
  | Pstring_load_64 b -> TPstring_load_64 b
  | Pstring_set_16 b -> TPstring_set_16 b
  | Pstring_set_32 b -> TPstring_set_32 b
  | Pstring_set_64 b -> TPstring_set_64 b
  (* load/set 16,32,64 bits from a
     (char, int8_unsigned_elt, c_layout) Bigarray.Array1.t : (unsafe) *)
  | Pbigstring_load_16 b -> TPbigstring_load_16 b
  | Pbigstring_load_32 b -> TPbigstring_load_32 b
  | Pbigstring_load_64 b -> TPbigstring_load_64 b
  | Pbigstring_set_16 b -> TPbigstring_set_16 b
  | Pbigstring_set_32 b -> TPbigstring_set_32 b
  | Pbigstring_set_64 b -> TPbigstring_set_64 b
  (* Compile time constants *)
  | Pctconst c -> TPctconst c
  (* byte swap *)
  | Pbswap16 -> TPbswap16
  | Pbbswap k -> TPbbswap k
  | _ -> assert false

let lambda_to_tlambda last_id code =

  let fid = ref 0 in
  let funs : ( int, tlambda) Hashtbl.t
      = Hashtbl.create 100
  in
  let register_function body =
    let f = !fid in
    Hashtbl.add funs f body;
    incr fid;
    f
  in
  let ir = ref last_id in
  let tlet ?(kind = Strict) id body cont =
    Tlet {
      te_id = id;
      te_lam = body;
      te_kind = kind;
      te_in = cont;
    }
  in
  let mk () =
    incr ir;
    Ident.({ name = ""; stamp = !ir; flags = 0 })
  in
  let remk i =
    incr ir;
    Ident.( { i with stamp = !ir; } )
  in

  let rec map_idents f = function
    | Tlet l ->
      let te_lam = map_idents_control f l.te_lam in
      let te_in = map_idents f l.te_in in
      Tlet { l with te_lam; te_in; }
    | Trec r ->
      let tr_decls = List.rev_map
  	(fun (i,p,ids) -> ( i, p, List.map f ids))
  	r.tr_decls in
      let tr_in = map_idents f r.tr_in in
      Trec { tr_decls; tr_in; }
    | Tend i -> Tend ( f i)

  and map_idents_control f = function
    | Tvar v -> Tvar ( f v )
    | Tapply ( i1, i2 ) -> Tapply ( f i1, f i2)
    | Tprim ( p, l ) -> Tprim ( p, List.map f l)
    | Tswitch ( i, s) ->
      let smap (a,lam) = ( a, map_idents f lam ) in
      let omap o = match o with
  	| None -> o
  	| Some lam -> Some ( map_idents f lam )
      in
      Tswitch
  	( f i,
  	  { s with
  	    t_consts = List.rev_map smap s.t_consts;
  	    t_blocks = List.rev_map smap s.t_blocks;
  	    t_failaction = omap s.t_failaction;
  	  } )
    | Tstaticraise ( i, ids) -> Tstaticraise ( i, List.map f ids )
    | Tstaticcatch ( lam, ( i, l), lam2 ) ->
      Tstaticcatch ( map_idents f lam,
  		     ( i, List.map f l),
  		     map_idents f lam2 )
    | Traise i -> Traise ( f i)
    | Ttrywith ( lam, i, lam2 ) ->
      Ttrywith ( map_idents f lam,
  		f i,
  		map_idents f lam2 )
    | Tifthenelse ( i, lam, lam2 ) ->
      Tifthenelse ( f i, map_idents f lam, map_idents f lam2 )
    | Twhile ( c, b) ->
      Twhile ( map_idents f c, map_idents f b)
    | Tfor ( i, ib, ie, d, b ) ->
      Tfor ( f i, f ib, f ie, d, map_idents f b )
    | c -> c
  in

  let free_vars nfv =
    let check nfv fv i =
      if Iset.mem i nfv
      then fv
      else Iset.add i fv
    in
    let rec aux nfv fv = function
      | Tlet { te_id; te_lam; te_in; _ } ->
	let fv = auxc nfv fv te_lam in
	aux fv (Iset.add te_id nfv) te_in
      | Trec { tr_decls; tr_in } ->
	let fv, nfv = List.fold_left
	  (fun (fv, nfv) (v,p,l) ->
	    let fv = Iset.remove v fv
	    and nfv = Iset.add v nfv in
	    ( List.fold_left (check nfv) fv l, nfv)
	  ) (fv, nfv) tr_decls in
	aux nfv fv tr_in
      | Tend i -> check nfv fv i
    and auxc nfv fv = function
      | Tvar v -> check nfv fv v
      | Tconst _ -> fv
      | Tapply ( f, x ) -> check nfv (check nfv fv f) x
      | Tprim ( _, l) -> List.fold_left ( check nfv) fv l
      | Tswitch ( i, s) ->
	let fv = check nfv fv i in
	let fold = List.fold_left
	  (fun fv (_,lam) -> aux nfv fv lam) in
	let fv = fold (fold fv s.t_consts) s.t_blocks in
	begin
	  match s.t_failaction with
	  | None -> fv
	  | Some l -> aux nfv fv l
	end
      | Tstaticraise ( _, l ) -> List.fold_left (check nfv) fv l
      | Tstaticcatch ( lt, ( _, l), lc) ->
	let fv = aux nfv fv lt in
	let nfv = List.fold_left
	  (fun nfv v -> Iset.add v nfv) nfv l in
	aux nfv fv lc
      | Traise i -> check nfv fv i
      | Ttrywith ( lt, i, lw ) ->
	let fv = aux nfv fv lt in
	let nfv = Iset.add i nfv in
	aux nfv fv lw
      | Tifthenelse ( i, t, e) ->
	let fv = check nfv fv i in
	let fv = aux nfv fv t in
	aux nfv fv e
      | Twhile ( c, b) ->
	let fv = aux nfv fv c in
	aux nfv fv b
      | Tfor ( i, b, e, _, body) ->
	let nfv = Iset.add i nfv in
	let fv = check nfv fv b in
	let fv = check nfv fv e in
	aux nfv fv body
    in aux nfv Iset.empty
  in

  (* adding lets *)
  (* puts an identifier on every expression *)

  let rec add_let = function
    | Llet ( k, i, e, cont ) -> Llet ( k, i, add_in_let e, add_let cont )
    | Lletrec ( decls, cont ) ->
      Lletrec ( List.rev_map (fun (i,lam) -> (i, add_in_let lam) ) decls, add_let cont )
    | Lvar _ as lam -> lam
    | lam ->
      let i = mk () in
      Llet ( Strict, i, add_in_let lam, Lvar i )
  and add_in_let = function
    | Llet _
    | Lletrec _ as lam -> add_let lam
    | Lvar _
    | Lconst _ as lam -> lam
    | Lapply ( lam, args, loc ) -> Lapply ( add_let lam, List.map add_let args, loc )
    | Lfunction ( k, args, body ) -> Lfunction ( k, args, add_let body )
    | Lprim ( p, l ) -> Lprim ( p, List.map add_let l )
    | Lswitch ( i, s ) ->
      let aux = List.rev_map (fun (i,lam) -> (i, add_let lam) ) in
      let sw_consts = aux s.sw_consts in
      let sw_blocks = aux s.sw_blocks in
      let sw_failaction =
	match s.sw_failaction with
	  None -> s.sw_failaction
	| Some l -> Some ( add_let l )
      in
      Lswitch ( add_let i, { s with sw_consts; sw_blocks; sw_failaction; } )
    | Lstaticraise ( i, l) -> Lstaticraise ( i, List.map add_let l )
    | Lstaticcatch ( lt, c, lw ) -> Lstaticcatch ( add_let lt, c, add_let lw )
    | Ltrywith ( lt, c, lw ) -> Ltrywith ( add_let lt, c, add_let lw )
    | Lifthenelse ( i, t, e ) -> Lifthenelse ( add_let i, add_let t, add_let e )
    | Lsequence ( a, b ) ->
      let i = mk () in
      Llet ( Strict, i, a, add_in_let b)
    | Lwhile ( c, b ) -> Lwhile ( add_let c, add_let b )
    | Lfor ( i, start, stop, d, body ) ->
      Lfor ( i, add_let start, add_let stop, d, add_let body )
    | Lassign _ -> assert false
    | Lsend ( k, obj, meth, args, loc ) -> Lsend ( k, add_let obj, add_let meth, List.map add_let args, loc )
    | Levent ( l, _ ) -> add_in_let l
    | Lifused _ -> assert false

  in

  (* end adding lets *)

  (* organizing lets *)
  (* assumes every expression is either a let, a letrec, or a var *)

  let llet k i lam cont = Llet ( k, i, lam, cont ) in

  let rec get_nonvars to_add vars = function
    | [] -> to_add, vars
    | ( Lvar _ ) as v :: tl -> get_nonvars to_add ( v :: vars ) tl
    | ( Llet ( k, i, e, cont )) :: tl -> get_nonvars ( ( k, i, e ) :: to_add ) ( cont :: vars ) tl
    | _ -> assert false
  in

  let rec to_add_to_lets cont = function
    | [] -> cont
    | ( k, i, e ) :: tl -> llet k i e ( to_add_to_lets cont tl )
  in

  let rec organize_list k i lam cont mklam l =
    let to_add, vars = get_nonvars [] [] l in
    match to_add with
    | [] -> llet k i lam ( organize_let cont )
    | _ ->
      organize_let
	( to_add_to_lets
	    ( llet k i ( mklam ( List.rev vars ) ) cont )
	    to_add
	)

  and organize_in_let k i cont lam =
    let ll = llet k i in
    match lam with
    | Llet ( k2, i2, e2, cont2) ->
      organize_in_let k2 i2 ( ll cont2 cont ) e2
    | Lletrec ( defs, cont2) ->
      organize_let ( Lletrec ( defs, ll cont2 cont ) )
    | Lapply ( Llet ( ka, ia, ea, ca ), ( [Lvar _] as arg ), l) ->
      organize_let ( llet ka ia ea ( ll ( Lapply ( ca, arg, l )) cont ) )
    | Lapply ( lam, [Llet ( ka,ia,ea,ca)], l)->
      organize_let ( llet ka ia ea ( ll ( Lapply ( lam, [ca], l ) ) cont ) )
    | Lprim ( p, l ) ->
      organize_list k i lam cont (fun l -> Lprim ( p, l) ) l
    | Lswitch ( Lvar _ as arg, s ) ->
      ll ( Lswitch ( arg, organize_let_switch s)) ( organize_let cont )
    | Lswitch ( Llet ( ka, ia, ea, ca), s ) ->
      organize_let ( llet ka ia ea ( ll ( Lswitch ( ca, s ) ) cont ) )
    | Lstaticraise ( is, l ) ->
      organize_list k i lam cont (fun l -> Lstaticraise ( is, l ) ) l
    | Lstaticcatch ( lt, c, lw ) -> ll ( Lstaticcatch ( organize_let lt, c, organize_let lw )) ( organize_let cont )
    | Ltrywith ( lt, c, lw ) -> ll ( Ltrywith ( organize_let lt, c, organize_let lw )) ( organize_let cont )
    | Lifthenelse ( Llet ( kc, ic, ec, cc ), lt, le ) ->
      organize_let ( llet kc ic ec ( ll ( Lifthenelse ( cc, lt, le ) ) cont ) )
    | Lifthenelse ( Lvar _ as c, lt, le ) -> ll ( Lifthenelse ( c, organize_let lt, organize_let le ) ) ( organize_let cont )
    | Lwhile ( c, b ) -> ll ( Lwhile ( organize_let c, organize_let b ) ) ( organize_let cont )
    | Lfor ( x, ( Lvar _ as start ) , ( Lvar _ as  stop ), d, body ) ->
      ll ( Lfor ( x, start, stop, d, organize_let body ) ) ( Lvar i )
    | Lfor ( x, Llet ( ks, is, es, cs ), stop, d, body ) ->
      organize_in_let ks is ( ll ( Lfor ( x, cs, stop, d, body ) ) cont ) es
    | Lfor ( x, ( Lvar _ as start ), Llet ( ks, is, es, cs), d, body ) ->
      organize_in_let ks is ( ll ( Lfor ( x, start, cs, d, body ) ) cont ) es
    | Lsend ( _, Lvar _, Lvar _, [], _ ) -> ll lam ( organize_let cont )
    | Lsend ( mk, Llet ( ko, io, eo, co ), ( Lvar _ as meth ), [], loc ) ->
      organize_in_let ko io ( ll ( Lsend ( mk, co, meth, [], loc ) ) cont ) eo
    | Lsend ( mk, obj, Llet ( km, im, em, cm ), [], loc ) ->
      organize_in_let km im ( ll ( Lsend ( mk, obj, cm, [], loc ) ) cont ) em
    | Lsend ( mk, obj, meth, args, loc ) ->
      organize_in_let k i cont ( Lapply ( Lsend ( mk, obj, meth, [], loc ), args, loc ) )
    | Lassign _
    | Levent _
    | Lifused _ -> assert false
    | expr -> ll expr ( organize_let cont )
  and organize_let = function
    | Llet ( k, i, lam, cont ) ->
      organize_in_let k i cont lam
    | Lletrec _ -> failwith "TODO: letrec"
    | Lvar _  as lam -> lam
    | _ -> assert false
  and organize_let_switch s =
    let aux = List.rev_map (fun (id, lam) -> (id, organize_let lam ) ) in
    let sw_blocks = aux s.sw_blocks in
    let sw_consts = aux s.sw_consts in
    let sw_failaction =
      match s.sw_failaction with
      |	None -> s.sw_failaction
      | Some lam -> Some ( organize_let lam )
    in
    { s with sw_blocks; sw_consts; sw_failaction; }
  in

  (* end organizing lets *)

  let normalize lambda = organize_let ( add_let lambda ) in

  let extract_var = function
    | Lvar v -> v
    | _ -> assert false
  in
  let extract_vars = List.map extract_var in

  let rec tlambda = function
    | Llet ( k, i, e, cont ) ->
      Tlet { te_kind = k; te_id = i; te_lam = tcontrol e; te_in = tlambda cont; }
    | Lletrec ( decls, cont ) ->
      Trec { tr_decls = List.rev_map
	  (fun (id, lam ) ->
	    match lam with
	    | Lprim (p,l) ->
	      ( id, prim_translate p, extract_vars l )
	    | _ -> assert false
	  ) decls;
	     tr_in = tlambda cont; }
    | Lvar v -> Tend v
    | _ -> assert false
  and tcontrol = function
    | Lvar v -> Tvar v
    | Lconst c -> Tconst c
    | Lapply ( Lvar f, [Lvar x], _ ) -> Tapply ( f, x )
    | Lfunction ( _, [arg], body ) ->
      let body = tlambda body in
      let fv = free_vars ( Iset.singleton arg ) body in
      let fvl = Iset.fold (fun v acc -> (v,remk v)::acc) fv [] in
      let body =
	map_idents
	  ( fun i -> try List.assoc i fvl with Not_found -> i)
	  body
      in
      let body,_ = List.fold_left
	(fun (cont,idx) (_,i) ->
	  ( tlet
	      ~kind:Alias
	      i
	      ( Tprim ( TPfunfield idx, [](*, [f_ident]*) ))
	      (* TODO: f_ident (I'm not sure we need it) *)
	      cont
	  ), succ idx) (body,0) fvl
      in
      let idf = register_function body in
      Tprim ( TPfun idf, fst ( List.split fvl ) )
    | Lprim ( Praise, [Lvar e] ) -> Traise e (* sequand and sequor should not be there already *)
    | Lprim ( p, l ) -> Tprim ( prim_translate p, extract_vars l ) (* TODO: RAISE AND STUFF *)
    | Lswitch ( Lvar x, s ) ->
      let aux = List.rev_map (fun (a,b) -> (a, tlambda b) ) in
      Tswitch ( x,
		{
		  t_numconsts = s.sw_numconsts;
		  t_consts = aux s.sw_consts;
		  t_numblocks = s.sw_numblocks;
		  t_blocks = aux s.sw_blocks;
		  t_failaction =
		    (
		      match s.sw_failaction with
		      | None -> None
		      | Some l -> Some ( tlambda l )
		    );
		} )
    | Lstaticraise ( i, l ) -> Tstaticraise ( i, extract_vars l )
    | Lstaticcatch ( lt, i, lw ) -> Tstaticcatch ( tlambda lt, i, tlambda lw )
    | Ltrywith ( lt, i, lw ) -> Ttrywith ( tlambda lt, i, tlambda lw )
    | Lifthenelse ( Lvar v, t, e ) -> Tifthenelse ( v, tlambda t, tlambda e )
    | Lwhile ( c, b ) -> Twhile ( tlambda c, tlambda b )
    | Lfor ( i, Lvar start, Lvar stop, d, body ) -> Tfor ( i, start, stop, d, tlambda body )
    | Lsend ( k, Lvar obj, Lvar meth, [], _ ) -> Tprim ( TPmethod_send k, [ obj; meth ] )
    | _ -> assert false
  in

  let code = tlambda ( normalize code ) in
  funs, code
