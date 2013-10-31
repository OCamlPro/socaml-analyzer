let () =
  let args = Array.sub Sys.argv 1 ( pred (Array.length Sys.argv) ) in
  let (g,funs,inv,exnv,outv) = Cmb.import args in
  
  let module E =
  struct
    let inv = inv
    let outv = outv
    let exnv = exnv
    let g = g
    let funs = funs
    let mk_vertex = Tlambda_to_hgraph.Vertex.mk
    let mk_hedge = Tlambda_to_hgraph.Hedge.mk
  end
  in
  let module Manager = Tlambda_analysis.M ( E ) in
  let module F = Hgraph.Fixpoint ( Tlambda_to_hgraph.T ) ( Manager ) in
  let result = F.kleene_fixpoint g ( Manager.H.VertexSet.singleton inv ) in
  ignore result;
  print_endline "That's all for now"
    
  
