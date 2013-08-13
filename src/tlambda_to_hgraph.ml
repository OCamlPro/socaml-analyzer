type id = Ident.t

module Vertex =
struct
  type t = int
  let compare (x:int) y = compare x y
  let equal (x:int) y = x = y
  let hash (x:int) = Hashtbl.hash x

  let c = ref 0
  let mk () = incr c; !c
end

module Hedge =
struct
  type t = id
  let compare = compare
  let equal = (=)
  let hash x = Hashtbl.hash x.Ident.stamp
end

module Desc =
struct
  type vertex = Vertex.t
  type hedge = Hedge.t

  module VertexSet = Set.Make (Vertex)
  module HedgeSet = Set.Make (Hedge)
  module VertexTbl = Hashtbl.Make (Vertex)
  module HedgeTbl = Hashtbl.Make (Hedge)

  let print_vertex _ _ = () 		(* TODO *)
  let print_hedge _ _ = ()
end

module G = Hgraph.Make (Desc)

type hinfo =
| Var of id
| Const of Lambda.structured_constant
| Prim of Tlambda.primitive * id list
| Constraint of constr
and constr = Ccp of int | Ctag of int

let ctrue = Constraint (Ccp 1)
let cfalse = Constraint (Ccp 0)

let const_unit = Lambda.Const_pointer 0

module Is = Set.Make ( struct type t = int let compare (a:int) b = compare a b end )

open Tlambda

let mk_graph ~last_id ~funs main =
  let stampr = ref last_id in
  let mk_id s =
    incr stampr;
    Ident.({ stamp = !stampr; name = s; flags = 0; })
  in
  let open G in
  let g = create () in
  let nv () =
    let v = Vertex.mk () in
    add_vertex g v (); v in
  let nf = Array.length funs in
  let fun_id = mk_id "$f" in
  let f_arg_id = mk_id "$x" in
  let f_ret_id = mk_id "$ans" in
  let f_exn_id = mk_id "$exn" in
  let fun_in = Array.init nf (fun _ -> nv ())
  and fun_out = Array.init nf (fun _ -> nv ())
  and fun_exn = Array.init nf (fun _ -> nv ())
  and statics : ( int, Vertex.t * id list ) Hashtbl.t = Hashtbl.create 32 in

  let dummy = nv () in
  let one_id = mk_id "$1" in

  let rec tlambda ~outv ~ret_id ~exn_id ~inv ~exnv code =
    match code with
    | Tlet d -> tlet inv outv exnv ret_id exn_id d
    | Trec d -> trec inv outv exnv ret_id exn_id d
    | Tend id -> add_hedge g ret_id ( Var id) ~pred:[|inv|] ~succ:[|outv|]

  and tlet inv outv exnv ret_id exn_id d =
    (* add_vertex g d.te_id (); *)
    let in_out = nv () in
    tcontrol inv in_out exnv d.te_id ret_id exn_id d.te_lam;
    tlambda ~outv ~ret_id ~inv:in_out ~exnv ~exn_id d.te_in

  and trec entry outv exnv ret_id exn_id d = failwith "TODO: rec"

  and tcontrol inv outv exnv id ret_id exn_id c =
    match c with
    | Tvar i -> add_hedge g id ( Var i) ~pred:[|inv|] ~succ:[|outv|]

    | Tconst c -> add_hedge g id ( Const c) ~pred:[|inv|] ~succ:[|outv|]

    | Tapply ( f, x, _) ->
      let fv = nv () in
      add_hedge g fun_id ( Var f) ~pred:[|inv|] ~succ:[|fv|];
      add_hedge g f_arg_id ( Var x) ~pred:[|fv|] ~succ:fun_in;
      add_hedge g exn_id ( Var f_exn_id) ~pred:fun_exn ~succ:[|exnv|];
      add_hedge g id ( Var f_ret_id) ~pred:fun_out ~succ:[|outv|]

    | Tprim ( p, args) -> add_hedge g id ( Prim ( p, args)) ~pred:[|inv|] ~succ:[|outv|]

    | Tswitch ( si_id, s) ->
      let switch_handle is_cp (i,lam) =
	let inc = nv () in
	add_hedge g si_id ( Constraint ( if is_cp then Ccp i else Ctag i)) ~pred:[|inv|] ~succ:[|inc|];
	tlambda ~outv ~ret_id ~inv:inc ~exnv ~exn_id lam
      in
      let () = List.iter ( switch_handle true) s.t_consts
      and () = List.iter ( switch_handle false) s.t_blocks
      in
      begin
	match s.t_failaction with
	  None -> ()
	| Some lam ->
	  let get_not_used n l =
	    let rec aux n res =
	      if n = 0
	      then res
	      else
		let n = pred n in
		aux n ( Is.add n res)
	    in
	    List.fold_left ( fun s (i,_) -> Is.remove i s) (aux n Is.empty) l
	  in
	  let cps = get_not_used s.t_numconsts s.t_consts
	  and bs = get_not_used s.t_numblocks s.t_blocks in
	  let inf = nv () in
	  Is.iter (fun cp -> add_hedge g si_id (Constraint (Ccp cp)) ~pred:[|inv|] ~succ:[|inf|]) cps;
	  Is.iter (fun tag -> add_hedge g si_id (Constraint (Ctag tag)) ~pred:[|inv|] ~succ:[|inf|]) bs;
	  tlambda ~outv ~ret_id ~inv:inf ~exnv ~exn_id lam
      end

    | Tstaticraise ( i, args) ->
      let ( catchv, cargs) = Hashtbl.find statics i in
      let endv =
	List.fold_left2 
	  (fun v carg arg ->
	    let v2 = nv () in
	    add_hedge g carg ( Var arg) ~pred:[|v2|] ~succ:[|v|];
	    v2)
	  inv ( List.tl cargs) ( List.tl args)
      in
      add_hedge g ( List.hd cargs) (Var ( List.hd args)) ~pred:[|endv|] ~succ:[|catchv|]

    | Tstaticcatch ( ltry, ( i, args), lwith) ->
      let catchv = nv () in
      Hashtbl.add statics i (catchv,args);
      tlambda ~outv ~ret_id ~inv ~exnv ~exn_id ltry;
      tlambda ~outv ~ret_id ~inv:catchv ~exnv ~exn_id lwith
      
    | Traise i -> add_hedge g exn_id (Var i) ~pred:[|inv|] ~succ:[|exnv|]

    | Ttrywith ( ltry, exni, lwith)  ->
      let exnv2 = nv () in
      tlambda ~outv ~ret_id ~exn_id ~inv ~exnv:exnv2 ltry;
      tlambda ~outv ~ret_id ~exn_id ~inv:exnv2 ~exnv lwith

    | Tifthenelse ( i, t, e) ->
      let int = nv ()
      and ine = nv () in
      add_hedge g i ctrue ~pred:[|inv|] ~succ:[|int|];
      add_hedge g i cfalse ~pred:[|inv|] ~succ:[|ine|];
      tlambda ~ret_id ~exn_id ~inv:int ~outv ~exnv t;
      tlambda ~ret_id ~exn_id ~inv:ine ~outv ~exnv e

    | Twhile ( lcond, lbody) ->
      let outc = nv () in
      let inb = nv () in
      add_hedge g ret_id cfalse ~pred:[|outc|] ~succ:[|outv|];
      add_hedge g ret_id ctrue ~pred:[|outc|] ~succ:[|inb|];
      tlambda ~outv:outc ~ret_id ~exn_id ~inv ~exnv lcond;
      tlambda ~outv:inv ~ret_id ~exn_id ~inv:inb ~exnv lbody

    | Tfor ( i, start, stop, dir, lbody) ->
      let test_id = mk_id "$test" in
      let initv = nv () in
      let testv = nv () in
      let inb = nv () in
      let outb = nv () in
      add_hedge g i ( Var start) ~pred:[|inv|] ~succ:[|initv|];
      add_hedge g test_id ( Prim ( Pintcomp Lambda.Cle, [i;stop])) ~pred:[|initv|] ~succ:[|testv|];
      add_hedge g test_id ctrue ~pred:[|testv|] ~succ:[|inb|];
      add_hedge g i ( Prim ( Paddint, [i;one_id])) ~pred:[|outb|] ~succ:[|initv|];
      add_hedge g test_id cfalse ~pred:[|testv|] ~succ:[|outv|];
      tlambda ~outv:outb ~ret_id ~exn_id ~inv:inb ~exnv lbody
  in
  let inv = nv () and outv = nv () and exnv = nv () in
  let exn_id = mk_id "$exn" in
  let ret_id = mk_id "$ret" in
  tlambda ~inv ~outv ~exnv ~ret_id ~exn_id main;
  (g,inv,outv,exnv)
