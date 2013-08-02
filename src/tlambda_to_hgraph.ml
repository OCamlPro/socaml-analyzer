type id = Ident.t

module Vertex =
struct
  type t = int
  let compare (x:int) y = compare x y
  let equal (x:int) y = x = y
  let hash (x:int) = hash x

  let c = ref 0
  let mk () = incr c; !c
end

module Hedge =
struct
  type t = id
  let compare = compare
  let equal = (=)
  let hash x = hash x.Ident.stamp
end

module Desc : Hgraph.T =
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
| Apply
(* | Function (\* probably useless *\) *)
(* | Let of id *)
(* | Letrec of id list *)
| Prim of Tlambda.primitive * id list
| Switch of switch_info
| Sraise of id list
| Scatch of id list
| Raise (* Well, I prefer it here *)
| Trywith
| If of id
| For of id
| Assign of id
(* | Send *)
and switch_info =
  {
    si_id : id;
    si_numconsts : int;
    si_consts : ( int * int ) list;
    si_numblocks : int;
    si_blocks : ( int * int ) list;
    si_fail : bool; (* if there is a failaction, it's index 0 *)
  }
  

open Tlambda

let mk_id _ = failwith "TODO: mk_id"

let mk_graph funs main entry =
  let open G in
  let g = create () in
  let nv () =
    let v = Vertex.mk () in
    add_vertex g v (); v
  let nf = Array.length funs in
  let fun_id = mk_id "$f"
  let exn_id = mk_id "$exn"
  let ret_id = mk_id "$ret"
  let fun_in = Array.init nf ( _ -> nv ())
  and fun_out = Array.init nf ( _ -> nv ())
  and fun_exn = Array.init nf ( _ -> nv ())
  and statics : ( int, Vertex.t) Hashtbl.t = Hashtbl.create 32 in

  let dummy = nv () in
  let one_id = = mk_id "1" in

  let rec tlambda entry exnv code =
    match code with
    | Tlet d -> tlet entry exnv d
    | Trec d -> trec entry exnv d
    | Tend id -> id

  and tlet entry exnv d =
    (* add_vertex g d.te_id (); *)
    let next = tcontrol entry exnv d.te_id d.te_lam in
    tlambda next exnv d.te_in

  and trec entry exnv d = failwith "TODO: rec"

  and tcontrol inv exnv id c =
    match c with
    | Tvar i ->
      let outv = nv () in
      add_hedge g id ( Var i) ~pred:[|inv|] ~succ:[|outv|];
      outv

    | Tconst c ->
      let outv = nv () in
      add_hedge g id ( Const c) ~pred:[|inv|] ~succ:[|outv|];
      outv

    | Tapply ( f, x, _) ->
      add_hedge g fun_id ( Apply ( f, x)) ~pred:[|inv|] ~succ:fun_in; (* is that a good idea ? *)
      add_hedge g id ( Raise exn_id) ~pred:fun_exn ~succ:[|exnv|];
      let outv = nv () in
      add_hedge g id ( Var ret_id) ~pred:fun_out ~succ:[|outv|];
      outv

    | Tprim ( p, args) ->
      let outv = nv () in
      add_hedge g id ( Prim ( p, args)) ~pred:[|inv|] ~succ:[|outv|];
      outv

    | Tswitch ( si_id, s) ->
      let hswitch l = List.map ( fun ( _,lam) -> tlambda inv exnv lam) l
      let outcs = hswitch s.t_consts
      and outbs = hswitch s.t_blocks
      and fail = match s.t_failaction with None -> [| |] | Some lam -> [|tlambda inv lam|]
      in
      let pred =
	Array.concat
	  [
	    fail;
	    Array.of_list outcs;
	    Array.of_list outbs
	  ]
      in
      let idx = ref -1 in
      let si_fail = fail <> [| |] in
      ( if si_fail then incr idx else () );
      let si_consts = List.map (fun (i,_) -> incr idx; (i,!idx)) s.t_consts in
      let si_blocks = List.map (fun (i,_) -> incr idx; (i,!idx)) s.t_blocks in
      let info =
	{
	  si_id;
	  si_numconsts = s.t_numconsts;
	  si_consts;
	  si_numblocks = s.t_numblocks;
	  si_blocks;
	  si_fail;
	}
      in
      let outv = nv () in
      add_hedge g id ( Switch info) ~pred ~succ:[|outv|];
      outv

    | Tstaticcraise ( i, args) ->
      add_hedge g id (Sraise args) ~pred:[|inv|] ~succ:[|Hashtbl.find statics i|];
      nv ()

    | Tstaticcatch ( ltry, ( i, args), lwith) ->
      let catchv = nv () in
      Hashtbl.add statics i catchv;
      let outt = tlambda inv exnv ltry in
      let outw = tlambda catchv exnv lwith in
      let outv = nv () in
      add_hedge g id (Scatch args) ~pred:[|outt;outw|] ~succ:[|outv|];
      outv
      
    | Traise i ->
      add_hedge g id (Raise i) ~pred:[|inv|] ~succ:[|exnv|];
      nv ()

    | Ttrywith ( ltry, exni, lwith)  ->
      let exnv2 = nv () in
      let outt = tlambda inv exnv2 ltry in
      let outw = tlambda exnv2 lwith exnv in
      let outv = nv () in
      add_hedge g id ( Try exni) ~pred:[|outt;outw|] ~succ:[|outv|];
      outv

    | Tifthenelse ( i, t, e) ->
      let outt = tlambda inv exnv t in
      let oute = tlambda inv exnv e in
      let outv = nv () in
      add_hedge g id ( If i) ~pred:[|outt;oute|] ~succ:[|outv|];
      outv

    | Twhile ( lcond, lbody) ->
      let outc = tlambda inv exnv lcond in
      let inb = nv () in
      let outb = tlambda inb exnv lbody in
      let outv = nv () in
      add_hedge g id ( If ret_id) ~pred:[|outc|] ~succ:[|inb;outv|];
      outv

    | Tfor ( i, start, stop, dir, lbody) ->
      let initv = nv () in
      add_hedge g i ( Var start) ~pred:[|inv|] ~succ:[|initv|];
      let testv = nv () in
      let test_id = mk_id "test" in
      add_hedge g test_id ( Prim ( Pint_comp Cle, [i;stop])) ~pred:[|initv|] ~succ:[|testv|];
      let outb = tlambda testv exnv lbody in
      add_hedge g i ( Prim ( Paddint, [x;one_id])) ~pred:[|outb|] ~succ:[|initv|];
      let outv = nv () in
      add_hedge g ret_id ( If test_id) ~pred:[|dummy (* unsure *); testv|] ~succ:[|outv|];
      outv
