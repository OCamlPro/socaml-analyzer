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

let map_fun f tbl =
  let open Tlambda_to_hgraph in
  Hashtbl.fold
    (fun fid fd l ->
       (f fid fd.f_graph
          fd.f_in.(0) fd.f_out.(0) fd.f_out.(1)
          fd.f_arg fd.f_return fd.f_exn)
       :: l ) tbl []


let export g funtbl vin vout vexn outputprefix =
  Exp.export ~g ~funtbl ~map_fun ~vin ~vout ~vexn
    ~file:( outputprefix ^ ".cmb")

