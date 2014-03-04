open Common_types

module type E =
sig
  type t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
  val mk : unit -> t
  val print : Format.formatter -> t -> unit
  val clone : t -> t
end

module Vertex =
struct
  type t = string * int
  let compare = compare
  let equal = (=)
  let hash x = Hashtbl.hash x

  let c = ref (-1)

  let print ppf (s,i) =
    Format.pp_print_string ppf s;
    Format.pp_print_string ppf "_";
    Format.pp_print_int ppf i

  let mk ?(modulename="no_module") () = 
    incr c;
    modulename, !c

  let clone _ = mk ()
end

module Hedge : E =
struct
  type t = int
  let compare (x:int) y = compare x y
  let equal (x:int) y = x = y
  let hash (x:int) = Hashtbl.hash x

  let c = ref (-1)

  let print ppf i = Format.fprintf ppf "v%i" i
  let mk () = incr c; !c

  let clone _ = mk ()
end

module T =
struct
  type vertex = Vertex.t
  type hedge = Hedge.t
  module Vertex = Vertex
  module Hedge = Hedge

  let print_vertex = Vertex.print
  let print_hedge = Hedge.print
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
  }

type mod_desc =
  {
    m_in : Vertex.t;
    m_out : Vertex.t;
    m_exn : Vertex.t;
    m_return : tid;
  }

type vattr = unit
type hattr = ( tid * hinfo ) list
type gattr = unit
type hg = ( vattr, hattr, gattr ) G.graph

let ctrue = Constraint (Ccp 1)
let cfalse = Constraint (Ccp 0)

let const_unit = Lambda.Const_pointer 0

module Is = Set.Make ( struct type t = int let compare (a:int) b = compare a b end )

open Tlambda

let nv ?(modulename="") g =
  let v = Vertex.mk ~modulename () in
  add_vertex g v ();
  v

let simpleh g id v ~inv ~outv =
  add_hedge g ( Hedge.mk ()) [id,v] ~pred:[|inv|] ~succ:[|outv|]

let statics : ( int, Vertex.t * tid list ) Hashtbl.t = Hashtbl.create 32


let tlambda ~g ~mk_tid ~modulename ~outv ~ret_id ~exn_id ~inv ~exnv code =
  let nv = nv ~modulename in

  let rec tlambda ~g ~outv ~ret_id ~exn_id ~inv ~exnv code =
    match code with
    | Tlet d -> tlet g inv outv exnv ret_id exn_id d
    | Trec d -> trec g inv outv exnv ret_id exn_id d
    | Tend id -> simpleh g ret_id ( Var id) ~inv ~outv

  and tlet g inv outv exnv ret_id exn_id d =
    let in_out = nv g in
    tcontrol g inv in_out exnv d.te_id ret_id exn_id d.te_lam;
    tlambda ~g ~outv ~ret_id ~inv:in_out ~exnv ~exn_id d.te_in

  and trec g entry outv exnv ret_id exn_id d =
    (* at this point, there are only primitives *)
    let in_out = nv g in
    add_hedge g ( Hedge.mk ()) ( List.rev_map (fun ( id, p, args ) -> id, Prim ( p, args ) ) d.tr_decls ) ~pred:[|entry|] ~succ:[|outv|];
    tlambda ~g ~outv ~ret_id ~inv:in_out ~exnv ~exn_id d.tr_in

  and tcontrol g inv outv exnv id ret_id exn_id c =
    match c with
    | Tvar i -> simpleh g id ( Var i) ~inv ~outv

    | Tconst c -> simpleh g id ( Const c) ~inv ~outv

    | Tapply ( f, x) ->
      let retv = nv g in
      simpleh g id (  App_prep ( f, x ) ) ~inv ~outv:retv;
      add_hedge g ( Hedge.mk ()) [ id, App ]
        ~pred:[|retv|] ~succ:[| outv; exnv |]

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


let init ~modulename funs =
  let nv = nv ~modulename in
  let mk_tid name = ( modulename, Id.create ~name () ) in
  let g = create () in
  let nf = Hashtbl.length funs in
  let exn_id = mk_tid "$exn" in

  let fun_descs = Hashtbl.create nf in

  Hashtbl.iter
    begin
      fun i flam ->
        let g = create () in
        let f_graph = g in
        let f_in = [| nv g |]
        and f_out = [| nv g; nv g |] in
        let f_return = mk_tid "#$return" and f_exn = mk_tid "#$exn" in
        let outv = nv g and exnv = nv g in
        add_hedge g (Hedge.mk () ) [ f_return, Return f_return ]
          ~pred:[|outv|] ~succ:[|f_out.(0)|];
        add_hedge g (Hedge.mk () ) [ f_exn, Retexn f_exn ]
          ~pred:[|exnv|] ~succ:[|f_out.(1)|];
        tlambda ~g
          ~mk_tid
          ~modulename
          ~inv:f_in.(0)
          ~outv:f_out.(0)
          ~exnv:f_out.(1)
          ~ret_id:f_return ~exn_id:f_exn
          flam;
        let f_vertex =
              VertexSet.remove f_in.(0) (
                VertexSet.remove f_out.(0) (
                  VertexSet.remove f_out.(1) (
                    ( List.fold_left
                        (fun vs v -> VertexSet.add v vs )
                        VertexSet.empty
                        ( list_vertex f_graph )
                    )))) in
        Array.iter (fun v -> assert(not (VertexSet.mem v f_vertex))) f_in;
        Hashtbl.add fun_descs i
          {
            f_graph; f_in; f_out;
            f_vertex;
            f_hedge = List.fold_left
                (fun hs h -> HedgeSet.add h hs )
                HedgeSet.empty
                ( list_hedge f_graph );
          }
    end
    funs;
  ( g, fun_descs, exn_id )

let mk_subgraph ~g ~modulename ~exn_id main =
  let nv = nv ~modulename in
  let inv = nv g and outv = nv g and exnv = nv g in
  let mk_tid name = ( modulename, Id.create ~name () ) in
  let ret_id = mk_tid "$ret" in
  tlambda ~g ~mk_tid ~modulename ~inv ~outv ~exnv ~ret_id ~exn_id main;
  { m_in = inv; m_out = outv; m_exn = exnv; m_return = ret_id; }

let mk_graph ~modulename funs tlam =
  let nv = nv ~modulename in
  let mk_tid name = ( modulename, Id.create ~name () ) in
  let ( g, fun_descs, exn_id ) = init ~modulename funs in
  let inv = nv g and outv = nv g and exnv = nv g in
  let ret_id = mk_tid "$ret" in
  tlambda ~g ~mk_tid ~modulename ~inv ~outv ~exnv ~ret_id ~exn_id tlam;
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
