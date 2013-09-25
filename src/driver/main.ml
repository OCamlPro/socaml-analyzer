

let lambdas = Mk_lambda.mk_lambdas Sys.argv

let ir = ref (Tt_restore.last_ident ())
let mk_id () = 
  incr ir;
  Ident.({ name = ""; stamp = !ir; flags = 0 })

let funs : ( F.t, tlambda ) Hashtbl.t = Hashtbl.create 1024

let tlambdas =
  Array.map
    ( Mk_tlambda.lambda_to_tlambda
      ~mk_id ~mk_fid:Common_types.F.create ~funs )
    lambdas
 
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
