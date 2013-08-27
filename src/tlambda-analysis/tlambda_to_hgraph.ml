type id = Ident.t

module type E =
sig
  type t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
  val mk : unit -> t
  val print : Format.formatter -> t -> unit
end

module Vertex : E =
struct
  type t = int
  let compare (x:int) y = compare x y
  let equal (x:int) y = x = y
  let hash (x:int) = Hashtbl.hash x

  let c = ref (-1)

  let print = Format.pp_print_int
  let mk () = incr c; !c
end

module Hedge : E =
struct
  type t = int
  let compare (x:int) y = compare x y
  let equal (x:int) y = x = y
  let hash (x:int) = Hashtbl.hash x

  let c = ref (-1)

  let print = Format.pp_print_int
  let mk () = incr c; !c
end

module Desc =
struct
  type vertex = Vertex.t
  type hedge = Hedge.t
  module Vertex = Vertex
  module Hedge = Hedge

  (* module VertexSet = Set.Make (Vertex) *)
  (* module HedgeSet = Set.Make (Hedge) *)
  (* module VertexTbl = Hashtbl.Make (Vertex) *)
  (* module HedgeTbl = Hashtbl.Make (Hedge) *)

  let print_vertex _ _ = () 		(* TODO *)
  let print_hedge _ _ = ()
end

module G = Hgraph.Make (Desc)

type hinfo =
| Var of id
| Const of Lambda.structured_constant
| Prim of Tlambda.primitive * id list
| Constraint of constr
| App of id * id (* function, argument *)
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

  let simpleh id v ~inv ~outv =
    add_hedge g ( Hedge.mk ()) [id,v] ~pred:[|inv|] ~succ:[|outv|]
  in

  let nv () =
    let v = Vertex.mk () in
    add_vertex g v (); v in
  let nf = Hashtbl.length funs in
(*  let fun_id = mk_id "$f" in
  let f_arg_id = mk_id "$x" in
  let f_ret_id = mk_id "$ans" in
  let f_exn_id = mk_id "$exn" in *)
  let fun_in = Hashtbl.create nf
  and fun_out = Hashtbl.create nf
  and fun_exn = Hashtbl.create nf
  and statics : ( int, Vertex.t * id list ) Hashtbl.t = Hashtbl.create 32 in

  Hashtbl.iter
    (fun i _ ->
      Hashtbl.add fun_in i ( nv () );
      Hashtbl.add fun_out i ( nv () );
      Hashtbl.add fun_exn i ( nv () ) )
    funs;

  (* let dummy = nv () in *)
  let one_id = mk_id "$1" in

  let rec tlambda ~outv ~ret_id ~exn_id ~inv ~exnv code =
    match code with
    | Tlet d -> tlet inv outv exnv ret_id exn_id d
    | Trec d -> trec inv outv exnv ret_id exn_id d
    | Tend id -> simpleh ret_id ( Var id) ~inv ~outv

  and tlet inv outv exnv ret_id exn_id d =
    (* add_vertex g d.te_id (); *)
    let in_out = nv () in
    tcontrol inv in_out exnv d.te_id ret_id exn_id d.te_lam;
    tlambda ~outv ~ret_id ~inv:in_out ~exnv ~exn_id d.te_in

  and trec entry outv exnv ret_id exn_id d =
    (* at this point, there are only primitives *)
    let in_out = nv () in
    add_hedge g ( Hedge.mk ()) ( List.rev_map (fun ( id, p, args ) -> id, Prim (p, args) ) d.tr_decls ) ~pred:[|entry|] ~succ:[|outv|];
    tlambda ~outv ~ret_id ~inv:in_out ~exnv ~exn_id d.tr_in

  and tcontrol inv outv exnv id ret_id exn_id c =
    match c with
    | Tvar i -> simpleh id ( Var i) ~inv ~outv

    | Tconst c -> simpleh id ( Const c) ~inv ~outv

    | Tapply ( f, x) ->
      add_hedge g ( Hedge.mk ()) [ id, App ( f, x ) ]
	~pred:[|inv|] ~succ:[| outv; exnv |]

    | Tprim ( p, args) -> simpleh id ( Prim ( p, args)) ~inv ~outv

    | Tswitch ( si_id, s) ->
      let switch_handle is_cp (i,lam) =
	let inc = nv () in
	simpleh si_id ( Constraint ( if is_cp then Ccp i else Ctag i)) ~inv ~outv:inc;
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
	  Is.iter (fun cp -> simpleh si_id (Constraint (Ccp cp)) ~inv ~outv:inf) cps;
	  Is.iter (fun tag -> simpleh si_id (Constraint (Ctag tag)) ~inv ~outv:inf) bs;
	  tlambda ~outv ~ret_id ~inv:inf ~exnv ~exn_id lam
      end

    | Tstaticraise ( i, args) ->
      let ( catchv, cargs) = Hashtbl.find statics i in
      let assigns = List.map2 (fun carg arg -> ( carg, Var arg)) cargs args in
      add_hedge g ( Hedge.mk ()) assigns ~pred:[|inv|] ~succ:[|catchv|]

    | Tstaticcatch ( ltry, ( i, args), lwith) ->
      let catchv = nv () in
      Hashtbl.add statics i (catchv,args);
      tlambda ~outv ~ret_id ~inv ~exnv ~exn_id ltry;
      tlambda ~outv ~ret_id ~inv:catchv ~exnv ~exn_id lwith
      
    | Traise i -> simpleh exn_id (Var i) ~inv ~outv:exnv

    | Ttrywith ( ltry, exni, lwith)  ->
      let exnv2 = nv () in
      tlambda ~outv ~ret_id ~exn_id ~inv ~exnv:exnv2 ltry;
      tlambda ~outv ~ret_id ~exn_id ~inv:exnv2 ~exnv lwith

    | Tifthenelse ( i, t, e) ->
      let int = nv ()
      and ine = nv () in
      simpleh i ctrue ~inv ~outv:int;
      simpleh i cfalse ~inv ~outv:ine;
      tlambda ~ret_id ~exn_id ~inv:int ~outv ~exnv t;
      tlambda ~ret_id ~exn_id ~inv:ine ~outv ~exnv e

    | Twhile ( lcond, lbody) ->
      let outc = nv () in
      let inb = nv () in
      simpleh ret_id cfalse ~inv:outc ~outv;
      simpleh ret_id ctrue ~inv:outc ~outv:inb;
      tlambda ~outv:outc ~ret_id ~exn_id ~inv ~exnv lcond;
      tlambda ~outv:inv ~ret_id ~exn_id ~inv:inb ~exnv lbody

    | Tfor ( i, start, stop, dir, lbody) ->
      let test_id = mk_id "$test" in
      let initv = nv () in
      let testv = nv () in
      let inb = nv () in
      let outb = nv () in
      simpleh i ( Var start) ~inv ~outv:initv;
      simpleh test_id ( Prim ( TPintcomp Lambda.Cle, [i;stop])) ~inv:initv ~outv:testv;
      simpleh test_id ctrue ~inv:testv ~outv:inb;
      simpleh i ( Prim ( TPaddint, [i;one_id])) ~inv:outb ~outv:initv;
      simpleh test_id cfalse ~inv:testv ~outv;
      tlambda ~outv:outb ~ret_id ~exn_id ~inv:inb ~exnv lbody
  in

      

  let exn_id = mk_id "$exn" in
  let ret_id = mk_id "$ret" in
  Hashtbl.iter (fun i lam ->
    tlambda
      ~inv:(Hashtbl.find fun_in i)
      ~outv:(Hashtbl.find fun_out i)
      ~exnv:(Hashtbl.find fun_exn i)
      ~ret_id ~exn_id
      lam ) funs;
  let inv = nv () and outv = nv () and exnv = nv () in
  tlambda ~inv ~outv ~exnv ~ret_id ~exn_id main;
  (g,inv,outv,exnv)
