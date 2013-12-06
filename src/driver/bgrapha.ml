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
    let mk_vertex = Tlambda_to_hgraph.Vertex.mk ~modulename:""
    let mk_hedge = Tlambda_to_hgraph.Hedge.mk
  end
  in
  let module Manager = Tlambda_analysis.M ( E ) in
  let module F = Hgraph.Fixpoint ( Tlambda_to_hgraph.T ) ( Manager ) in
  print_endline "starting the analysis";
  let result = F.kleene_fixpoint g ( Manager.H.VertexSet.singleton inv ) in
  let exn_env = Tlambda_to_hgraph.G.vertex_attrib result exnv in
  if Envs.is_bottom exn_env
  then ()
  else
    begin
      print_endline "I found something:";
      Data.print
        Format.std_formatter
        Manager.exn_tid
        exn_env;
      exit 1
    end
