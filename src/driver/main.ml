
let lambda, last_id = Mk_lambda.mk_lambda Sys.argv
let last_id, funs, tlambda =
  Lambda_to_tlambda.lambda_to_tlambda last_id lambda
 
let ( g, inv, outv, exnv, funs, arg_id, return_id, exn_id ) = Tlambda_to_hgraph.mk_graph
  ~last_id
  ~funs
  tlambda

module E =
struct
  let inv = inv
  let outv = outv
  let exnv = exnv
  let g = g
  let funs = funs
  let mk_vertex = Tlambda_to_hgraph.Vertex.mk
  let mk_hedge = Tlambda_to_hgraph.Hedge.mk
  let return_id = return_id
  let arg_id = arg_id
  let exn_id = exn_id
end

module Manager = Tlambda_analysis.M ( E )
