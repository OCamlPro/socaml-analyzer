
module type Ginfo =
sig
  type vattr
  type hattr
  type fid
  type tid
  type fun_table

  val vattr_merge : vattr -> vattr -> vattr

  type h

  val mkh : unit -> h

end

(* exporting and importing of Hgraph as bigraphs *)
module Store (T : Hgraph.T) ( H : Hgraph.Hgraph with module T := T ) ( I : Ginfo with type h = T.hedge ) =
struct
  type g = ( I.vattr, I.hattr, unit ) H.graph

  type vext = T.vertex * I.vattr
  type hext = I.hattr * T.vertex array * T.vertex array

  type gext = vext list * hext list
              * T.vertex * T.vertex * T.vertex


  type fdescr =
    {
      f_id : I.fid;
      f_graph : gext;
    }

  type funs = fdescr list

  type fun_mapper =
    ( I.fid -> g ->
      T.vertex -> T.vertex -> T.vertex ->
      fdescr ) ->
    I.fun_table ->
    funs

  type fun_adder =
      I.fun_table ->
      I.fid -> g ->
      T.vertex -> T.vertex -> T.vertex ->
      unit

  let store_low (g:gext) (f:funs) file =
    let o = open_out file in
    Marshal.to_channel o (g,f) [];
    close_out o

  let get_low file =
    let i = open_in file in
    let ( (g, f ): gext * funs )  = Marshal.from_channel i in
    close_in i;
    (g,f)

  let g_to_gext ~g ~vin ~vout ~vexn =
    ( List.rev_map
        (fun v -> v, H.vertex_attrib g v )
        ( H.list_vertex g )
    ),
    ( List.rev_map
        (fun h ->
           H.hedge_attrib g h,
           H.hedge_pred' g h, H.hedge_succ' g h )
        ( H.list_hedge g )
    ), vin, vout, vexn


  let gext_in_g ~g (vl,hl,_,_,_) =
    let rec export_vertex = function
      | [] -> ()
      | (v,a) :: vl ->
        H.add_vertex g v a;
        export_vertex vl
    in
    let rec export_hedge = function
      | [] -> ()
      | (a,pred,succ) :: hl ->
        H.add_hedge g (I.mkh ()) a ~pred ~succ;
        export_hedge hl
    in
    export_vertex vl;
    export_hedge hl

  let gext_merge_vertices ~g ~vin ~vexn (_,_,vein,veout,veexn) =
    H.vertex_merge g I.vattr_merge vin vein;
    H.vertex_merge g I.vattr_merge vexn veexn;
    veout

  let f_to_fdescr fid fg vin vout vexn =
    {
      f_id = fid;
      f_graph = g_to_gext ~g:fg ~vin ~vout ~vexn;
    }

  let export ~g ~funtbl ~( map_fun : fun_mapper ) ~vin ~vout ~vexn ~file =
    let gext = g_to_gext ~g ~vin ~vout ~vexn in
    let funs = map_fun f_to_fdescr funtbl  in
    store_low gext funs file

  let import ~g ~funtbl ~( ext_fun : fun_adder ) ~vin ~vexn ~file =
    let gext, funext = get_low file in
    gext_in_g ~g gext;
    let vout = gext_merge_vertices ~g ~vin ~vexn gext in
    List.iter
      (fun fd ->
         let g = H.create () in
         gext_in_g ~g fd.f_graph;
         let (_,_,vin,vout,vexn) = fd.f_graph in
         ext_fun funtbl
           fd.f_id g vin vout vexn
      ) funext;
    vout
end

