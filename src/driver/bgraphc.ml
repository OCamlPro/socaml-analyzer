open Common_types

module GI =
struct
  type vattr = unit
  type hattr = tid * hinfo list
  type fid = F.t
  type tid = tid
  type fun_table = ( F.t, fun_desc ) Hashtbl.t
end

module Exp = Export.Store ( Tlambda_to_hgraph.G ) ( GI )


let () =

  assert Array.length Sys.argv = 2;

  let fn = Sys.argv.(1) in

  let (lambda,modname) = Mk_lambda.mk_lambda (fn) in
  let funs : ( F.t, Tlambda.tlambda ) Hashtbl.t =
    Hashtbl.create 256 in

  let ir = ref (Mk_lambda.last_id () ) in
  let mk_id () = 
    incr ir;
    Ident.({ name = ""; stamp = !ir; flags = 0 })
  in

  let tlambda =
    Mk_tlambda.lambda_to_tlambda
      ~mk_id ~modname ~funs lambda
  in
  
  let mk_tid () = ( modname, mk_id () ) in

  let (g,funtbl,vin,vout,vexn,exn_id) =
    Tlambda_to_hgraph.mk_graph ~mk_tid tlambda
  in

  Exp.export ~g ~funtbl ~map_fun ~vin ~vout ~vexn
    ~file:((Filename.chop_extension fn) ^ ".cmb")
    
