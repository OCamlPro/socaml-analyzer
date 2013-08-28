
let lambda, last_id = Mk_lambda.mk_lambda Sys.argv
let last_id, funs, tlambda =
  Lambda_to_tlambda.lambda_to_tlambda last_id lambda
 
let ( g, inv, outv, exnv, funs ) = Tlambda_to_hgraph.mk_graph
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
end

module Manager = Tlambda_analysis.M ( E )
