open Common_types

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

module T =
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

module G = Hgraph.Make (T)
open G

type fun_desc =
  {
    f_graph : ( unit, (tid * hinfo) list, unit ) G.graph;
    f_in : Vertex.t array;
    f_out : Vertex.t array;
    f_vertex : VertexSet.t;
    f_hedge : HedgeSet.t;
    f_arg : tid;
    f_return : tid;
    f_exn : tid;
  }

type mod_desc =
  {
    m_in : Vertex.t;
    m_out : Vertex.t;
    m_exn : Vertex.t;
    m_return : tid;
  }

type hg = ( unit, ( tid * hinfo ) list, unit ) graph

let ctrue = Constraint (Ccp 1)
let cfalse = Constraint (Ccp 0)

let const_unit = Lambda.Const_pointer 0

module Is = Set.Make ( struct type t = int let compare (a:int) b = compare a b end )

open Tlambda

let nv g =
  let v = Vertex.mk () in
  add_vertex g v ();
  v

let simpleh g id v ~inv ~outv =
  add_hedge g ( Hedge.mk ()) [id,v] ~pred:[|inv|] ~succ:[|outv|]
 
let statics : ( int, Vertex.t * tid list ) Hashtbl.t = Hashtbl.create 32


let tlambda ~g ~mk_tid ~outv ~ret_id ~exn_id ~inv ~exnv code =

  let rec tlambda ~g ~outv ~ret_id ~exn_id ~inv ~exnv code =
    match code with
    | Tlet d -> tlet g inv outv exnv ret_id exn_id d
    | Trec d -> trec g inv outv exnv ret_id exn_id d
    | Tend id -> simpleh g ret_id ( Var id) ~inv ~outv

  and tlet g inv outv exnv ret_id exn_id d =
    (* add_vertex g d.te_id (); *)
    let in_out = nv g in
    tcontrol g inv in_out exnv d.te_id ret_id exn_id d.te_lam;
    tlambda ~g ~outv ~ret_id ~inv:in_out ~exnv ~exn_id d.te_in

  and trec g entry outv exnv ret_id exn_id d =
    (* at this point, there are only primitives *)
    let in_out = nv g in
    add_hedge g ( Hedge.mk ()) ( List.rev_map (fun ( id, p, args ) -> id, Prim ( p, args ) ) d.tr_decls ) ~pred:[|entry|] ~succ:[|outv;exnv|];
    tlambda ~g ~outv ~ret_id ~inv:in_out ~exnv ~exn_id d.tr_in

  and tcontrol g inv outv exnv id ret_id exn_id c =
    match c with
    | Tvar i -> simpleh g id ( Var i) ~inv ~outv

    | Tconst c -> simpleh g id ( Const c) ~inv ~outv

    | Tapply ( f, x) ->
      add_hedge g ( Hedge.mk ()) [ id, App ( f, x ) ]
        ~pred:[|inv|] ~succ:[| outv; exnv |]

    | Tprim ( p, args) ->
      simpleh g id ( Prim ( p, args ) ) ~inv ~outv

    | Tswitch ( si_id, s) ->
      let switch_handle is_cp (i,lam) =
        let inc = nv g in
        simpleh g si_id ( Constraint ( if is_cp then Ccp i else Ctag i)) ~inv ~outv:inc;
        tlambda ~g ~outv ~ret_id ~inv:inc ~exnv ~exn_id lam
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
	  let inf = nv g in
	  Is.iter (fun cp -> simpleh g si_id (Constraint (Ccp cp)) ~inv ~outv:inf) cps;
	  Is.iter (fun tag -> simpleh g si_id (Constraint (Ctag tag)) ~inv ~outv:inf) bs;
	  tlambda ~g ~outv ~ret_id ~inv:inf ~exnv ~exn_id lam
      end

    | Tstaticraise ( i, args) ->
      let ( catchv, cargs) = Hashtbl.find statics i in
      let assigns = List.map2 (fun carg arg -> ( carg, Var arg)) cargs args in
      add_hedge g ( Hedge.mk ()) assigns ~pred:[|inv|] ~succ:[|catchv|]

    | Tstaticcatch ( ltry, ( i, args), lwith) ->
      let catchv = nv g in
      Hashtbl.add statics i (catchv,args);
      tlambda ~g ~outv ~ret_id ~inv ~exnv ~exn_id ltry;
      tlambda ~g ~outv ~ret_id ~inv:catchv ~exnv ~exn_id lwith

    | Traise i -> simpleh g exn_id (Var i) ~inv ~outv:exnv

    | Ttrywith ( ltry, exni, lwith)  ->
      let exnv2 = nv g in
      tlambda ~g ~outv ~ret_id ~exn_id ~inv ~exnv:exnv2 ltry;
      tlambda ~g ~outv ~ret_id ~exn_id ~inv:exnv2 ~exnv lwith

    | Tifthenelse ( i, t, e) ->
      let int = nv g
      and ine = nv g in
      simpleh g i ctrue ~inv ~outv:int;
      simpleh g i cfalse ~inv ~outv:ine;
      tlambda ~g ~ret_id ~exn_id ~inv:int ~outv ~exnv t;
      tlambda ~g ~ret_id ~exn_id ~inv:ine ~outv ~exnv e

    | Twhile ( lcond, lbody) ->
      let outc = nv g in
      let inb = nv g in
      simpleh g ret_id cfalse ~inv:outc ~outv;
      simpleh g ret_id ctrue ~inv:outc ~outv:inb;
      tlambda ~g ~outv:outc ~ret_id ~exn_id ~inv ~exnv lcond;
      tlambda ~g ~outv:inv ~ret_id ~exn_id ~inv:inb ~exnv lbody

    | Tfor ( i, start, stop, dir, lbody) ->
      let test_id = mk_tid "$test" in
      let initv = nv g in
      let testv = nv g in
      let inb = nv g in
      let outb = nv g in
      let o = Asttypes.( match dir with Upto -> 1 | Downto -> -1 ) in
      simpleh g i ( Var start) ~inv ~outv:initv;
      simpleh g test_id ( Prim ( TPintcomp Lambda.Cle, [i;stop] )) ~inv:initv ~outv:testv;
      simpleh g test_id ctrue ~inv:testv ~outv:inb;
      simpleh g i ( Prim ( TPoffsetint o, [i])) ~inv:outb ~outv:initv;
      simpleh g test_id cfalse ~inv:testv ~outv;
      tlambda ~g ~outv:outb ~ret_id ~exn_id ~inv:inb ~exnv lbody

    | Tlazyforce i ->
      add_hedge g (Hedge.mk ()) [ id, (Lazyforce i)]
        ~pred:[|inv|] ~succ:[|outv;exnv|]
    | Tccall ( p, l ) ->
      add_hedge g (Hedge.mk ()) [ id, (Ccall (p,l))]
        ~pred:[|inv|] ~succ:[|outv;exnv|]
    | Tsend ( _, o, m ) ->
      add_hedge g (Hedge.mk ()) [ id, (Send(o,m))]
        ~pred:[|inv|] ~succ:[|outv;exnv|]
  in
  
  tlambda ~g ~outv ~ret_id ~exn_id ~inv ~exnv code

(* let mk_graph ~last_id ~funs main = *)
  
(*   let open G in *)
(*   let g = create () in *)

  

(*   let nv g = *)
(*     let v = Vertex.mk () in *)
(*     add_vertex g v (); v in *)
(*   let nf = Hashtbl.length funs in *)
(* (\*  let fun_id = mk_tid "$f" in *\) *)
(*   let f_arg_id = mk_tid "$x" in *)
(*   let f_ret_id = mk_tid "$ans" in *)
(*   let f_exn_id = mk_tid "$exn" in *)
(*   let fun_descs = Hashtbl.create nf *)
(*   and statics : ( int, Vertex.t * id list ) Hashtbl.t = Hashtbl.create 32 in *)

(*   Hashtbl.iter *)
(*     (fun i _ -> *)
(*       let f_graph = create () in *)
(*       let f_in = [| nv g |] *)
(*       and f_out = [| nv g; nv g |] in *)
(*       let f_vertex = VertexSet.empty *)
(*       and f_hedge = HedgeSet.empty in *)
(*       Hashtbl.add fun_descs i { f_graph; f_in; f_out; f_vertex; f_hedge; } *)
(*     ) *)
(*     funs; *)

(*   (\* let dummy = nv () in *\) *)
(*   let one_id = mk_tid "$1" in *)

  
(*   let exn_id = mk_tid "$exn" in *)
(*   let ret_id = mk_tid "$ret" in *)
(*   Hashtbl.iter (fun i f -> *)
(*     tlambda ~g:f.f_graph *)
(*       ~inv:f.f_in.(0) *)
(*       ~outv:f.f_out.(0) *)
(*       ~exnv:f.f_out.(1) *)
(*       ~ret_id ~exn_id *)
(*       ( Hashtbl.find funs i ); *)
(*     Hashtbl.replace fun_descs i *)
(*       { f with *)
(* 	f_vertex = *)
(* 	  VertexSet.remove f.f_in.(0) ( *)
(* 	      VertexSet.remove f.f_out.(0) ( *)
(* 		VertexSet.remove f.f_out.(1) ( *)
(* 		  ( List.fold_left *)
(* 		      (fun vs v -> VertexSet.add v vs ) *)
(* 		      VertexSet.empty *)
(* 		      ( list_vertex f.f_graph ) *)
(* 		  )))); *)
(* 	f_hedge = *)
(* 	  List.fold_left *)
(* 	    (fun hs h -> HedgeSet.add h hs ) *)
(* 	    HedgeSet.empty *)
(* 	    ( list_hedge f.f_graph ) *)
(*       } *)
(*   ) fun_descs; *)
(*   let inv = nv g and outv = nv g and exnv = nv g in *)
(*   tlambda ~g ~inv ~outv ~exnv ~ret_id ~exn_id main; *)
(*   ( g, inv, outv, exnv, fun_descs, f_arg_id, f_ret_id, f_exn_id ) *)






let init ~mk_tid funs =
  let g = create () in
  let nf = Hashtbl.length funs in
  let exn_id = mk_tid "$exn" in
  
  (*  let fun_id = mk_tid "$f" in *)
  let fun_descs = Hashtbl.create nf in
  (* and statics : ( int, Vertex.t * id list ) Hashtbl.t = Hashtbl.create 32 in *)

  Hashtbl.iter
    begin
      fun i flam ->
        let f_arg = mk_tid "$x" in
        let f_return = mk_tid "$ans" in
        let f_exn = mk_tid "$exn" in
        let f_graph = create () in
        let f_in = [| nv g |]
        and f_out = [| nv g; nv g |] in
        tlambda ~g:f_graph
          ~mk_tid
          ~inv:f_in.(0)
          ~outv:f_out.(0)
          ~exnv:f_out.(1)
          ~ret_id:f_return ~exn_id:f_exn
          flam;
        Hashtbl.add fun_descs i
          {
            f_graph; f_in; f_out;
            f_arg; f_return; f_exn;
            f_vertex =
              VertexSet.remove f_in.(0) (
                VertexSet.remove f_out.(0) (
                  VertexSet.remove f_out.(1) (
                    ( List.fold_left
                        (fun vs v -> VertexSet.add v vs )
                        VertexSet.empty
                        ( list_vertex f_graph )
                    ))));
            f_hedge = List.fold_left
                (fun hs h -> HedgeSet.add h hs )
                HedgeSet.empty
                ( list_hedge f_graph );
          }
    end
    funs;
  ( g, fun_descs, exn_id )

let mk_subgraph ~g ~mk_tid ~exn_id main =
  let inv = nv g and outv = nv g and exnv = nv g in
  let ret_id = mk_tid "$ret" in
  tlambda ~g ~mk_tid ~inv ~outv ~exnv ~ret_id ~exn_id main;
  { m_in = inv; m_out = outv; m_exn = exnv; m_return = ret_id; }

let mk_graph ~modulename funs tlam =
  let mk_tid name = ( modulename, Id.create ~name () ) in
  let ( g, fun_descs, exn_id ) = init ~mk_tid funs in
  let inv = nv g and outv = nv g and exnv = nv g in
  let ret_id = mk_tid "$ret" in
  tlambda ~g ~mk_tid ~inv ~outv ~exnv ~ret_id ~exn_id tlam;
  ( g, fun_descs, inv, outv, exnv, exn_id, ret_id )
  

let merge_graphs ~g subs =
  let mv = vertex_merge g (fun () () -> ()) in
  let l = Array.length subs in
  let first = subs.(0)
  and last = subs.(pred l) in
  for i = 1 to l - 1 do
    mv subs.(pred i).m_out subs.(i).m_in;
    mv subs.(0).m_exn subs.(i).m_exn
  done;
  ( first.m_in,
    last.m_out,
    first.m_exn,
    last.m_return )
