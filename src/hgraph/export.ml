module type Ginfo =
sig
  type vattr
  type hattr
  type fid
  type tid
end

(* exporting and importing of Hgraph as bigraphs *)
module Store ( H : Hgraph.Hgraph ) ( I : Ginfo ) =
struct
  type g = ( I.vattr, I.hattr, unit ) H.graph

  type fdescr =
  {
    f_id : I.fid
    f_graph : g;
    f_in : H.T.vertex;
    f_out : H.T.vertex;
    f_exn : H.T.vertex;
    f_arg : tid;
    f_return : tid;
    f_exn : tid;
  }

  type funs = fdescr list

  type vext = H.T.vertex * vattr
  type hext = H.T.hedge * hattr * H.T.vertex array * H.T.vertex array

  type gext = vext list * hext list

  let store_low (g:gext) (f:funs) file =
    let o = open_out file in
    Marshal.to_channel o (g,f) [];
    close_out o

  let get_low file =
    let i = open_in file in
    let ( g:gext, f:funs ) = Marshal.from_channel i in
    close_in i;
    (g,f)

  let g_to_gext g =
    ( List.rev_map
        (fun v -> v, H.vertex_attrib g v )
        ( H.list_vertex g )
    ),
    ( List.rev_map
        (fun h ->
           h, H.hedge_attrib g h,
           H.hedge_pred' g h, H.hedge_succ' g h )
        ( H.list_hedge g )
    )      

  let export ~g ~funs ~vin ~vout ~vexn ~file =
    failwith "TODO: export"

  let import ~g ~funs ~vin ~vexn ~file =
    failwith "TODO: import"

end
