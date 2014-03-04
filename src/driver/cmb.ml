open Common_types

module GI =
struct
  type vattr = Tlambda_to_hgraph.vattr
  type hattr = Tlambda_to_hgraph.hattr
  type fid = F.t
  type tid = Common_types.tid
  type fun_table = ( F.t, Tlambda_to_hgraph.fun_desc ) Hashtbl.t

  type h = Tlambda_to_hgraph.Hedge.t
  let mkh = Tlambda_to_hgraph.Hedge.mk

  let vattr_merge () () = ()
end

module Exp =
  Export.Store
    (Tlambda_to_hgraph.T)
    ( Tlambda_to_hgraph.G )
    ( GI )

let map_fun f tbl =
  let open Tlambda_to_hgraph in
  Hashtbl.fold
    (fun fid fd l ->
       (f fid fd.f_graph
          fd.f_in.(0) fd.f_out.(0) fd.f_out.(1)
       )
       :: l ) tbl []


let export g funtbl vin vout vexn outputprefix =
  Exp.export ~g ~funtbl ~map_fun ~vin ~vout ~vexn
    ~file:( outputprefix ^ ".cmb")

let ext_fun funtbl fid f_graph vin vout vexn =
  let open Tlambda_to_hgraph in
  let open G in
  let f_in = [| vin |] in
  let f_out = [| vout; vexn |] in
  let f_vertex =
    List.fold_left
      (fun s v -> VertexSet.add v s)
      VertexSet.empty
      (list_vertex f_graph) in
  let f_vertex =
    VertexSet.remove f_in.(0) (
      VertexSet.remove f_out.(0) (
        VertexSet.remove f_out.(1) (f_vertex))) in
  Array.iter (fun v -> assert(not (VertexSet.mem v f_vertex))) f_in;
  Hashtbl.add funtbl fid
    {
      f_graph;
      f_in;
      f_out;
      f_vertex;
      f_hedge =
        List.fold_left
          (fun s v -> HedgeSet.add v s)
          HedgeSet.empty
          (list_hedge f_graph);
    }

let import_generic folder arg =
  let open Tlambda_to_hgraph.G in 
  let g = create () in
  let nv g =
    let v = Tlambda_to_hgraph.T.Vertex.mk () in
    add_vertex g v ();
    v
  in
  let vin = nv g in
  let vexn = nv g in
  let funtbl : ( F.t, Tlambda_to_hgraph.fun_desc ) Hashtbl.t =
    Hashtbl.create 65536
  in
  let vout =
    folder
      (fun vin file ->
         Exp.import ~g ~funtbl ~ext_fun ~vin ~vexn ~file
      ) vin arg
  in
  (g,funtbl,vin,vexn,vout)


let import = import_generic Array.fold_left


let import_list  = import_generic List.fold_left
  
