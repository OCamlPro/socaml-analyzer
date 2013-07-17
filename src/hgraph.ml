module type T = sig
  type vertex (** Type of vertex identifiers *)
  type hedge  (** Type of hyperedge identifiers *)
  module VertexSet : (Set.S with type elt=vertex)
  (** Set module for vertices *)
  module HedgeSet : (Set.S with type elt=hedge)
  (** Set module for hyperedges *)
  module VertexTbl : (Hashtbl.S with type key=vertex)
  (** Hash module with vertices as keys *)
  module HedgeTbl : (Hashtbl.S with type key=hedge)
  (** Hash module with hyperedges as keys *)

  val print_vertex : Format.formatter -> vertex -> unit
  val print_hedge : Format.formatter -> hedge -> unit

end

module type Hgraph = sig
  include T

  type ('vattr, 'hattr, 'e) graph
  val create : ?size:int -> 'e -> ('vattr, 'hattr, 'e) graph
  val contains_vertex : (_, _, _) graph -> vertex -> bool
  val contains_hedge : (_, _, _) graph -> hedge -> bool
  val add_vertex : ('a, _, _) graph -> vertex -> 'a -> unit
  val add_hedge : (_, 'b, _) graph -> hedge -> 'b ->
    pred:vertex array -> succ:vertex array -> unit

  val vertex_succ : (_, _, _) graph -> vertex -> HedgeSet.t
  val vertex_pred : (_, _, _) graph -> vertex -> HedgeSet.t

  val hedge_succ : (_, _, _) graph -> hedge -> VertexSet.t
  val hedge_pred : (_, _, _) graph -> hedge -> VertexSet.t

  val hedge_succ' : (_, _, _) graph -> hedge -> vertex array
  val hedge_pred' : (_, _, _) graph -> hedge -> vertex array

  val import_hedge : (_, _, _) graph -> (_, 'b, _) graph -> hedge -> 'b -> unit
  val import_subgraph : (_, _, _) graph -> ('a, 'b, _) graph ->
    vertex list -> hedge list -> (vertex -> 'a) -> (hedge -> 'b) -> unit

  val list_vertex : (_, _, _) graph -> vertex list
  val list_hedge : (_, _, _) graph -> hedge list

  val vertex_attrib : ('a, _, _) graph -> vertex -> 'a
  val hedge_attrib : (_, 'b, _) graph -> hedge -> 'b

  val print_dot :
    ?style:string ->
    ?titlestyle:string ->
    ?vertexstyle:string ->
    ?hedgestyle:string ->
    ?fvertexstyle:(vertex -> string) ->
    ?fhedgestyle:(hedge -> string) ->
    ?title:string ->
    ?print_attrvertex:(Format.formatter -> vertex -> 'a -> unit) ->
    ?print_attrhedge:(Format.formatter -> hedge -> 'b -> unit) ->
    Format.formatter -> ('a, 'b, 'c) graph -> unit
end

module Make(T:T) :
  Hgraph with type vertex = T.vertex and type hedge = T.hedge = struct
  include T
  module VSet = VertexSet
  module VTbl = VertexTbl
  module HSet = HedgeSet
  module HTbl = HedgeTbl

  let vset_of_array a = Array.fold_right VSet.add a VSet.empty
  let hset_of_array a = Array.fold_right HSet.add a HSet.empty

  (* module T = T *)

  type 'vattr vertex_n = {
    v_attr : 'vattr;
    mutable v_pred : HSet.t;
    mutable v_succ : HSet.t;
  }

  type 'hattr hedge_n = {
    h_attr : 'hattr;
    h_pred : vertex array;
    h_succ : vertex array;
  }

  type ('vattr,'hattr,'e) graph = {
    vertex : 'vattr vertex_n VTbl.t;
    hedge : 'hattr hedge_n HTbl.t;
    info : 'e
  }

  let create ?(size=20) info = {
    vertex = VTbl.create size;
    hedge = HTbl.create size;
    info;
  }

  let contains_vertex g v = VTbl.mem g.vertex v
  let contains_hedge g h = HTbl.mem g.hedge h

  let vertex_n g v = VTbl.find g.vertex v
  let hedge_n g h = HTbl.find g.hedge h

  let add_vertex g v v_attr =
    if contains_vertex g v
    then failwith "add_vertex: the vertex is already in the graph";
    VTbl.add g.vertex v { v_attr; v_pred = HSet.empty; v_succ = HSet.empty }

  let add_hedge g h h_attr ~pred ~succ =
    if contains_hedge g h
    then failwith "add_hedge: the hedge is already in the graph";
    begin try
      Array.iter
        (fun v ->
          let vertex_n =  vertex_n g v in
          vertex_n.v_succ <- HSet.add h vertex_n.v_succ)
        pred;
      Array.iter
        (fun v ->
          let vertex_n =  vertex_n g v in
          vertex_n.v_pred <- HSet.add h vertex_n.v_pred)
        succ;
    with Not_found ->
      failwith "add_hedge: origin or destination vertex does not exists"
    end;
    HTbl.add g.hedge h
      { h_attr; h_pred = pred; h_succ = succ }

  let vertex_succ g v = (vertex_n g v).v_succ
  let vertex_pred g v = (vertex_n g v).v_pred
  let hedge_succ' g h = (hedge_n g h).h_succ
  let hedge_pred' g h = (hedge_n g h).h_pred

  let hedge_succ g h = vset_of_array (hedge_succ' g h)
  let hedge_pred g h = vset_of_array (hedge_pred' g h)

  let list_vertex g = VTbl.fold (fun k _ l -> k::l) g.vertex []
  let list_hedge g = HTbl.fold (fun k _ l -> k::l) g.hedge []

  let vertex_attrib g v = (vertex_n g v).v_attr
  let hedge_attrib g h = (hedge_n g h).h_attr

  let import_hedge g1 g2 h attr =
    let hedge_n = hedge_n g1 h in
    add_hedge g2 h attr ~pred:hedge_n.h_pred ~succ:hedge_n.h_succ

  let import_subgraph g1 g2 vl hl fv fh =
    let import_vertex v = add_vertex g2 v (fv v) in
    let import_hedge h =
      let hedge_n = hedge_n g1 h in
      add_hedge g2 h (fh h) ~pred:hedge_n.h_pred ~succ:hedge_n.h_succ in
    List.iter import_vertex vl;
    List.iter import_hedge hl

  let print_dot
      ?(style:string="")
      ?(titlestyle:string="shape=plaintext,style=bold,style=filled,fontsize=12")
      ?(vertexstyle:string="shape=box,fontsize=12")
      ?(hedgestyle:string="shape=ellipse,fontsize=12")
      ?fvertexstyle
      ?fhedgestyle
      ?(title:string="")
      ?print_attrvertex
      ?print_attrhedge
      ppf g
    =
    let open Format in
    fprintf ppf "digraph G {@.  @[<v>%s@ " style;
    if title<>"" then
      fprintf ppf "1073741823 [%s,label=\"%s\"];@ " titlestyle title;
    begin match print_attrvertex with
      | None -> ()
      | Some print_attrvertex ->
        VTbl.iter
          (begin fun vertex vertex_n ->
            fprintf ppf "%a [%s,label=\"%t\"];@ "
              print_vertex vertex
              (match fvertexstyle with
               | Some f -> f vertex
               | None -> vertexstyle)
              (fun ppf -> print_attrvertex ppf vertex vertex_n.v_attr);
          end)
          g.vertex
    end;
    begin match print_attrhedge with
      | None -> ()
      | Some print_attrhedge ->
        HTbl.iter
          (begin fun hedge hedge_n ->
            fprintf ppf "%a [%s,label=\"%t\"];@ "
              print_hedge hedge
              (match fhedgestyle with
               | Some f -> f hedge
               | None -> hedgestyle)
              (fun ppf -> print_attrhedge ppf hedge hedge_n.h_attr);
          end)
          g.hedge
    end;
    HTbl.iter
      (begin fun hedge hedge_n ->
        Array.iter
          (begin fun pred ->
            fprintf ppf "%a -> %a;@ "
              print_vertex pred print_hedge hedge
          end)
          hedge_n.h_pred;
        Array.iter
          (begin fun succ ->
            fprintf ppf "%a -> %a;@ "
              print_hedge hedge print_vertex succ
          end)
          hedge_n.h_succ
      end)
      g.hedge
    ;
    fprintf ppf "@]@.}@.";
    ()

end

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

module type Manager = sig

  type hedge
  type vertex
  type abstract
  type hedge_attribute

  val bottom : vertex -> abstract
  val is_bottom : vertex -> abstract -> bool
  val is_leq : vertex -> abstract -> abstract -> bool
  (* val join : vertex -> abstract -> abstract -> abstract *)
  val join_list : vertex -> abstract list -> abstract
  (* val widening : vertex -> abstract -> abstract -> abstract *)
  val abstract_init : vertex -> abstract


  val apply : hedge -> hedge_attribute -> abstract array ->
    abstract * ( hedge * hedge ) list

end

module Fixpoint(Hgraph:Hgraph)(Manager:Manager with type hedge := Hgraph.hedge and type vertex := Hgraph.vertex) = struct
  include Hgraph
  module VSet = VertexSet
  module VTbl = VertexTbl
  module HSet = HedgeSet
  module HTbl = HedgeTbl
  module M = Manager
  type abstract = M.abstract

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

  type state = {
    g : (unit, Manager.hedge_attribute, unit) graph;
    hedge_approx : abstract HTbl.t;
    vertex_approx : abstract VTbl.t;
    vertex_wq : Vwq.t;
    hedge_wq : Hwq.t;
  }

  let init_state g sinit =
    let g = initialise g in
    let vertex_wq = Vwq.create () in
    let hedge_wq = Hwq.create () in
    Vwq.push_set vertex_wq sinit;
    let hedge_approx = HTbl.create 10 in
    let vertex_approx = VTbl.create 10 in
    { g; hedge_approx; vertex_approx; vertex_wq; hedge_wq }

  let kleene_hedge_step state h =
    let vertex_abstract v =
      try VTbl.find state.vertex_approx v with
      | Not_found -> M.bottom v
    in

    let pred = hedge_pred' state.g h in
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
      let attrib = hedge_attrib state.g h in
      let result, links = M.apply h attrib abstract in
      Vwq.push_set state.vertex_wq (hedge_succ state.g h);
      List.iter (fun (entry,_) ->
        Hwq.push_set state.hedge_wq (HedgeSet.singleton entry)) links;
      HTbl.replace state.hedge_approx h result

  let kleene_vertex_step state v =
    let vertex_abstract v =
      try VTbl.find state.vertex_approx v with
      | Not_found -> M.bottom v
    in
    let hedge_abstract v =
      try Some (HTbl.find state.hedge_approx v) with
      | Not_found -> None
    in

    let pred = HSet.elements (vertex_pred state.g v) in
    let abstract = match pred with
      | [] -> M.abstract_init v
      | _ ->
        match map_filter hedge_abstract pred with
        | [] -> M.bottom v
        | [a] -> a
        | l -> M.join_list v l
    in
    if not (M.is_leq v abstract (vertex_abstract v))
    then Hwq.push_set state.hedge_wq (vertex_succ state.g v);
    VTbl.replace state.vertex_approx v abstract

  let rec kleene_loop state =
    match Vwq.pop state.vertex_wq with
      | None ->
        begin match Hwq.pop state.hedge_wq with
          | None -> ()
          | Some h ->
            kleene_hedge_step state h;
            kleene_loop state
        end
      | Some v ->
        kleene_vertex_step state v;
        kleene_loop state

  let kleene_fixpoint orig_g sinit =
    let g = initialise orig_g in

    (* vertex working queue *)
    let vwq = Vwq.create () in

    (* hyper edge working queue *)
    let hwq = Hwq.create () in

    Vwq.push_set vwq sinit;

    let hedge_result = HTbl.create 10 in
    let vertex_result = VTbl.create 10 in

    let hedge_abstract v =
      try Some (HTbl.find hedge_result v) with
      | Not_found -> None
    in

    let vertex_abstract v =
      try VTbl.find vertex_result v with
      | Not_found -> M.bottom v
    in

    let update_vertex v =
      let pred = HSet.elements (vertex_pred g v) in
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
        let attrib = hedge_attrib orig_g h in
        let result,links = M.apply h attrib abstract in
        Vwq.push_set vwq (hedge_succ g h);
        List.iter (fun (entry,_) ->
          Hwq.push_set hwq (HedgeSet.singleton entry)) links;
        HTbl.replace hedge_result h result
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
