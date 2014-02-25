open Hgraph_types
open Hgraph
open Fixpoint_types

let rec map_filter f l =
  match l with
  | [] -> []
  | t::q -> match f t with
    | None -> map_filter f q
    | Some t' -> t' :: (map_filter f q)

let lift_option_array a =
  try Some (Array.map (function
      | None -> raise Not_found
      | Some v -> v) a)
  with Not_found -> None

module Fixpoint(T:T)(Manager:Manager with module T := T) : sig

  type input_graph = (Manager.vertex_attribute,
                      Manager.hedge_attribute,
                      Manager.graph_attribute)
      Manager.H.graph

  val kleene_fixpoint :
    (* ?err_graph:(unit, Manager.hedge_attribute, unit) Manager.H.graph option ref -> *)
    input_graph -> Manager.H.VertexSet.t ->
    (Manager.abstract, unit, unit) Manager.H.graph

end = struct
  open Manager
  open H
  module VSet = VertexSet
  module VTbl = VertexTbl
  module HSet = HedgeSet
  module HISet = HedgeIntSet
  module HTbl = HedgeTbl
  module M = Manager
  type abstract = M.abstract

  type input_graph = (vertex_attribute, hedge_attribute, graph_attribute) graph

  type output = (abstract,unit,unit) graph

  module WorkQueue(S:Set.S) = struct
    type elt = S.elt
    type t = {
      queue : elt Queue.t;
      mutable set : S.t;
    }

    let push t v =
      if not (S.mem v t.set)
      then begin
        Queue.push v t.queue;
        t.set <- S.add v t.set
      end

    let push_set t s = S.iter (push t) s

    let pop t =
      if Queue.is_empty t.queue
      then None
      else
        let v = Queue.pop t.queue in
        t.set <- S.remove v t.set;
        Some v

    let create () =
      { queue = Queue.create (); set = S.empty }

  end

  module Vwq = WorkQueue(VertexSet)
  module Hwq = WorkQueue(HedgeSet)

  module FidSet = Set.Make(Function_id)
  module FidMap = Map.Make(Function_id)

  module HedgeFid = struct
    type t = T.hedge * function_id
    let hash (h,f) =
      Hashtbl.hash (T.Hedge.hash h, Function_id.hash f)
    let compare (h1,f1) (h2,f2) =
      let c = T.Hedge.compare h1 h2 in
      if c <> 0 then c
      else Function_id.compare f1 f2
    let equal (h1,f1) (h2,f2) =
      T.Hedge.equal h1 h2 && Function_id.equal f1 f2
  end
  module HedgeFidSet = Set.Make(HedgeFid)
  module HedgeFidTbl = Hashtbl.Make(HedgeFid)

  type call_info = {
    input : (T.vertex * HISet.t) array;
    output : (T.vertex * HISet.t) array;
  }

  type 'a hedge_info = {
    orig_attrib : 'a;
    call_stack : call_info FidMap.t;
  }

  let initialise g =
    let import_vertex _ = () in
    let import_hedge v = hedge_attrib g v in
    let all_vertex = list_vertex g in
    let all_hedge = list_hedge g in
    let g' = create ~size:(List.length all_hedge) () in
    import_subgraph g g' all_vertex all_hedge import_vertex import_hedge;
    g'

  let return_graph g vertex_table =
    let import_vertex v =
      try VTbl.find vertex_table v with
      | Not_found -> M.bottom v in
    let import_hedge _ = () in
    let all_vertex = list_vertex g in
    let all_hedge = list_hedge g in
    let g' = create ~size:(List.length all_hedge) () in
    import_subgraph g g' all_vertex all_hedge import_vertex import_hedge;
    g'


  (* (\* copy the [copied_hedge], taking predecessors from [copy_pred] and *)
  (*    successors from [copy_succ] *\) *)
  (* let copy_input_hedge g copied_hedge ~copy_pred ~copy_succ = *)
  (*   let new_hedge = clone_hedge copied_hedge in *)
  (*   let pred = hedge_pred' g copy_pred in *)
  (*   let succ = hedge_succ' g copy_succ in *)
  (*   add_hedge g new_hedge ~pred ~succ *)

  let copy_relink_function g call_hedge previous_call_info =
    let input_vert = hedge_pred' g call_hedge in
    let output_vert = hedge_succ' g call_hedge in
    let new_hedges = HedgeTbl.create 3 in
    let get_hedge h =
      try HedgeTbl.find new_hedges h with
      | Not_found ->
        let new_hedge = clone_hedge h in
        let v = hedge_pred' g h, hedge_succ' g h, new_hedge in
        HedgeTbl.add new_hedges h v;
        v
    in
    let copy_pred pos (_,hi_pred) =
      let vert = input_vert.(pos) in
      let replace_pred (i,h) =
        let pred, _, _ = get_hedge h in
        pred.(i) <- vert
      in
      HISet.iter replace_pred hi_pred
    in
    let copy_succ pos (_,hi_succ) =
      let vert = output_vert.(pos) in
      let replace_succ (i,h) =
        let _, succ, _ = get_hedge h in
        succ.(i) <- vert
      in
      HISet.iter replace_succ hi_succ
    in
    Array.iteri copy_pred previous_call_info.input;
    Array.iteri copy_succ previous_call_info.output;

    let final_import hedge (pred, succ, new_hedge) : unit =
      let attrib = hedge_attrib g hedge in
      add_hedge g new_hedge attrib ~pred ~succ
    in
    HedgeTbl.iter final_import new_hedges;

    let hedge_mapping = HedgeTbl.fold (fun old_hedge (_,_,new_hedge) acc ->
        HedgeMap.add new_hedge old_hedge acc)
        new_hedges HedgeMap.empty in

    let add_copied_hedge (i,h) set =
      try let (_,_,r) = HedgeTbl.find new_hedges h in
        HISet.add (i,r) set
      with Not_found -> assert false in
    let corresponding a i (_,hi) =
      a.(i), HISet.fold add_copied_hedge HISet.empty hi
    in
    hedge_mapping,
    { input = Array.mapi (corresponding input_vert) previous_call_info.input;
      output = Array.mapi (corresponding output_vert) previous_call_info.output }

  let kleene_fixpoint (* ?err_graph *) orig_g sinit =
    let g = initialise orig_g in

    (* (match err_graph with *)
    (*  | None -> () *)
    (*  | Some r -> r := Some g); *)

    let h_info : 'a hedge_info HTbl.t = HTbl.create 10 in
    List.iter (fun h -> HTbl.add h_info h
                  { call_stack = FidMap.empty;
                    orig_attrib = (hedge_attrib orig_g h) })
      (list_hedge g);

    (* vertex working queue *)
    let vwq = Vwq.create () in

    (* hyper edge working queue *)
    let hwq = Hwq.create () in

    Vwq.push_set vwq sinit;

    let hedge_result : abstract array HTbl.t = HTbl.create 10 in
    let vertex_result = VTbl.create 10 in

    let imported_functions : call_info HedgeFidTbl.t = HedgeFidTbl.create 10 in

    let hedge_abstract (i,h) =
      try
        Some (HTbl.find hedge_result h).(i)
      with
      | Not_found -> None
    in

    let vertex_abstract v =
      try VTbl.find vertex_result v with
      | Not_found -> M.bottom v
    in

    let update_vertex v =
      let pred = HedgeIntSet.elements (vertex_pred' g v) in
      let abstract = match pred with
        | [] -> M.abstract_init v
        | _ ->
          match map_filter hedge_abstract pred with
          | [] -> M.bottom v
          | [a] -> a
          | l -> M.join_list v l
      in
      if not (M.is_leq v abstract (vertex_abstract v))
      then begin
        (* Printf.printf "increasing\n%!"; *)
        Hwq.push_set hwq (vertex_succ g v)
      end
      (* else Printf.printf "not increasing\n%!" *)
      ;
      VTbl.replace vertex_result v abstract
    in

    let recursive_call h f call_info =
      let hedge_mapping, new_call_info = copy_relink_function g h call_info in
      let add_info new_hedge old_hedge =
        HTbl.add h_info new_hedge (HTbl.find h_info old_hedge) in
      HedgeMap.iter add_info hedge_mapping;
      new_call_info
    in

    let import_function h f call_stack =
      let (in_graph,subgraph) = Manager.find_function f in
      let input = hedge_pred' g h in
      let output = hedge_succ' g h in
      let imported_hedge = ref [] in
      let import_hedge hedge =
        let new_hedge = Manager.clone_hedge hedge in
        imported_hedge := (new_hedge,hedge) :: !imported_hedge;
        new_hedge
      in
      let subgraph, input_hedges, output_hedges = clone_subgraph
          ~in_graph
          ~out_graph:g
          ~import_vattr:(fun ~new_vertex ~old_attr -> ())
          ~import_hattr:(fun ~new_hedge ~old_attr -> old_attr)
          ~clone_vertex:Manager.clone_vertex
          ~clone_hedge:import_hedge
          ~input
          ~output
          subgraph
      in
      let input = Array.mapi (fun i hi -> (input.(i),hi)) input_hedges in
      let output = Array.mapi (fun i hi -> (output.(i),hi)) output_hedges in
      let call_stack = FidMap.add f {input; output} call_stack in
      let add_info (new_hedge,hedge) =
        HTbl.add h_info new_hedge
          { call_stack;
            orig_attrib = hedge_attrib in_graph hedge }
      in
      List.iter add_info !imported_hedge;
      { input; output }
    in
    (* import a function in the graph and returns the set of hedges to
       update after that *)
    let import_function h f =
      try
        ignore(HedgeFidTbl.find imported_functions (h,f));
        HedgeSet.empty
      with
      | Not_found ->
        let call_stack = (HTbl.find h_info h).call_stack in
        let call_info =
          try
            let call_info = FidMap.find f call_stack in
            (* recursive call *)
            (* Printf.eprintf "rec call\n%!"; *)
            recursive_call h f call_info
          with Not_found ->
            (* Printf.eprintf "normal call\n%!"; *)
            (* not recursive call *)
            import_function h f call_stack in
        HedgeFidTbl.add imported_functions (h,f) call_info;

        Array.fold_left (fun hset (_,hiset) ->
            HedgeIntSet.fold (fun (_,v) set -> HedgeSet.add v set) hiset hset)
          HedgeSet.empty call_info.input
    in

    let update_hedge h =
      let pred = hedge_pred' g h in
      let f v =
        let abstract = vertex_abstract v in
        if M.is_bottom v abstract
        then None
        else Some abstract
      in
      let pred' = Array.map f pred in
      match lift_option_array pred' with
      | None -> ()
      | Some abstract ->
        let attrib =
          try (HTbl.find h_info h).orig_attrib
          with Not_found ->
            Format.eprintf "%a@." T.Hedge.print h;
            raise Not_found
        in
        let results,functions = M.apply h attrib abstract in
        Vwq.push_set vwq (hedge_succ g h);
        List.iter (fun fid -> Hwq.push_set hwq (import_function h fid)) functions;
        HTbl.replace hedge_result h results
    in

    let rec loop () =
      match Vwq.pop vwq with
      | None ->
        begin match Hwq.pop hwq with
          | None -> ()
          | Some h ->
            update_hedge h;
            loop ()
        end
      | Some v ->
        update_vertex v;
        loop ()
    in
    loop ();
    return_graph g vertex_result

end
