
open Common_types

let lambdas =
  Mk_lambda.mk_lambdas
    Format.std_formatter
    (Array.sub Sys.argv 1 ( pred (Array.length Sys.argv)) )

let funs : ( F.t, Tlambda.tlambda ) Hashtbl.t = Hashtbl.create 1024

let tlambdas =
  Array.map
    (fun (lam, modname) ->
       Mk_tlambda.lambda_to_tlambda
         ~modname ~funs lam )
    lambdas

let ( g, funs, exn_id ) = Tlambda_to_hgraph.init ~modulename:"" funs

let subgs =
  Array.map
    ( Tlambda_to_hgraph.mk_subgraph ~g ~modulename:"" ~exn_id )
    tlambdas

let inv,outv,exnv,return_id =
  Tlambda_to_hgraph.merge_graphs ~g subgs


(* let ( g, inv, outv, exnv, funs, arg_id, return_id, exn_id ) = *)
(*   Tlambda_to_hgraph.mk_graph *)
(*     ~last_id:!ir *)
(*     ~funs *)
(*     tlambda *)

module E =
struct
  let inv = inv
  let outv = outv
  let exnv = exnv
  let g = g
  let funs = funs
  let mk_vertex = Tlambda_to_hgraph.Vertex.mk ~modulename:""
  let mk_hedge = Tlambda_to_hgraph.Hedge.mk
  let return_id = return_id
end

module Manager = Tlambda_analysis.M ( E )
