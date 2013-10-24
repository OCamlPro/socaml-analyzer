open Common_types

module GI =
struct
  type vattr = unit
  type hattr = ( tid * hinfo ) list
  type fid = F.t
  type tid = Common_types.tid
  type fun_table = ( F.t, Tlambda_to_hgraph.fun_desc ) Hashtbl.t

  let vattr_merge () () = ()
end

module Exp =
  Export.Store
    (Tlambda_to_hgraph.T)
    ( Tlambda_to_hgraph.G )
    ( GI )


let () =

  assert ( Array.length Sys.argv = 2 );

  let fn = Sys.argv.(1) in

  let (lambda,modname) = Mk_lambda.mk_lambda (fn) in
  let funs : ( F.t, Tlambda.tlambda ) Hashtbl.t =
    Hashtbl.create 256 in

  let ir = ref (Mk_lambda.last_id () ) in
  let mk_id ?(n="") () = 
    incr ir;
    Ident.({ name = n; stamp = !ir; flags = 0 })
  in

  let tlambda =
    Mk_tlambda.lambda_to_tlambda
      ~mk_id ~modname ~funs lambda
  in
  
  let mk_tid n = ( modname, mk_id ~n () ) in

  let (g,funtbl,vin,vout,vexn,exn_id,return_id) =
    Tlambda_to_hgraph.mk_graph ~mk_tid funs tlambda
  in

  let map_fun f tbl =
    let open Tlambda_to_hgraph in
    Hashtbl.fold
      (fun fid fd l ->
         (f fid fd.f_graph
            fd.f_in.(0) fd.f_out.(0) fd.f_out.(1)
            fd.f_arg fd.f_return fd.f_exn)
         :: l ) tbl []
  in

  Exp.export ~g ~funtbl ~map_fun ~vin ~vout ~vexn
    ~file:((Filename.chop_extension fn) ^ ".cmb")
    
