open Common_types

let lambdas = Mk_lambda.mk_lambdas (Array.sub Sys.argv 1 ( pred (Array.length Sys.argv)) )

let ir = ref (Mk_lambda.last_id () )
let mk_id ?(n = "") () = 
  incr ir;
  Ident.({ name = n; stamp = !ir; flags = 0 })

let funs : ( F.t, Tlambda.tlambda ) Hashtbl.t = Hashtbl.create 1024

let tlambdas =
  Array.map
    (fun (lam, modname) ->
       Mk_tlambda.lambda_to_tlambda
         ~mk_id ~modname ~funs lam )
    lambdas

let mk_tid n = ( "", mk_id ~n () ) (* won't work *)

let ( g, funs, exn_id ) = Tlambda_to_hgraph.init ~mk_tid funs

let subgs =
  Array.map
    ( Tlambda_to_hgraph.mk_subgraph ~g ~mk_tid ~exn_id )
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
  let mk_vertex = Tlambda_to_hgraph.Vertex.mk
  let mk_hedge = Tlambda_to_hgraph.Hedge.mk
  let return_id = return_id
end

module Manager = Tlambda_analysis.M ( E )
