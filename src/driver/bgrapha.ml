let () =
  let args = Array.sub Sys.argv 1 ( pred (Array.length Sys.argv) ) in
  let (g,funs,inv,exnv,outv) = Cmb.import args in
  
  (* let module E = *)
  (* struct *)
  (*   let inv = inv *)
  (*   let outv = outv *)
  (*   let exnv = exnv *)
  (*   let g = g *)
  (*   let funs = funs *)
  (*   let mk_vertex = Tlambda_to_hgraph.Vertex.mk *)
  (*   let mk_hedge = Tlambda_to_hgraph.Hedge.mk *)
  (*   (\* let return_id = return_id *\) *)
  (* end *)
  (* in *)
  (* let module Manager = Tlambda_analysis.M ( E ) in *)
  print_endline "That's all for now"
    
  
