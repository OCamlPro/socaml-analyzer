open Format
open Data
open Utils

module Vertex = struct
  type t = string
  let compare (i:string) j = Pervasives.compare i j
  let hash (i:string) = Hashtbl.hash i
  let equal (i:string) j = i = j

  let print ppf s = Format.pp_print_string ppf s
end

module Hedge = struct
  type t = int
  let compare (i:int) j = Pervasives.compare i j
  let hash i = Hashtbl.hash i
  let equal (i:int) j = i = j

  let print ppf i = Format.pp_print_int ppf i

  let counter = ref (-1)
  let new_hedge () = incr counter; !counter
end

module T = struct

  type vertex = Vertex.t
  type hedge = Hedge.t
  module Vertex = Vertex
  module Hedge = Hedge

  (* module VertexSet = Set.Make(Vertex) *)
  (* module HedgeSet = Set.Make(Hedge) *)
  (* module VertexTbl = Hashtbl.Make(Vertex) *)
  (* module HedgeTbl = Hashtbl.Make(Hedge) *)

  let print_vertex = Vertex.print
  let print_hedge = Hedge.print

end

module H = Hgraph.Make(T)

module Env = struct

  type prepared_call =
    { closure : Data.id;
      param : Data.id }

  type t =
    { env : Data.environment;
      prepared_call : prepared_call option }

  let get_union ids env =
    Ids.fold (fun id v -> union (get_env id env) v) ids bottom

  let any_int =
    { bottom with int = Top }

  let func_val func_id closure =
    { bottom with f = Fm.singleton func_id
                      (Array.map Ids.singleton closure) }

  let func_val' func_id closure =
    { bottom with f = Fm.singleton func_id closure }

  let env_v env = { env; prepared_call = None }

  let set_env id data env =
    env_v (set_env id data env.env)

  let set_constraint id constr { env } =
    let env, value = intersect_noncommut env (get_env id env) constr in
    env_v (Data.set_env id value env)

  let set_int env dst cst =
    set_env dst (int_singleton cst) env

  let add_int env ~src1 ~src2 ~dst =
    env
    |> set_constraint src1 any_int
    |> set_constraint src2 any_int
    |> set_env dst any_int

  let set_closure env dst func_id vars =
    set_env dst (func_val func_id vars) env

  let prepare_call env closure param =
    { env with prepared_call = Some {closure; param} }

  let bottom = env_v bottom_env
  let empty = env_v empty_env
  let join e1 e2 = env_v (join_env e1.env e2.env)
  let is_bottom_env e = is_bottom_env e.env
  let is_leq e1 e2 = is_leq_env e1.env e2.env

  let call abs =
    match abs.prepared_call with
    | None -> failwith "malformed call sequence"
    | Some { closure; param } ->
      let f = (get_env closure abs.env).f in
      let functions = List.map fst (Fm.bindings f) in
      [|bottom|], functions

  let enforce_closure abs func_id =
    match abs.prepared_call with
    | None -> failwith "malformed call sequence"
    | Some { closure; param } ->
      let f = (get_env closure abs.env).f in
      let used_closure = Fm.find func_id f in
      set_env closure (func_val' func_id used_closure) abs, used_closure

  let access_closure abs dst func_id pos =
    match abs.prepared_call with
    | None -> failwith "malformed call sequence"
    | Some { closure; param } as prepared_call ->
      let abs, used_closure = enforce_closure abs func_id in
      { (set_env dst (get_union used_closure.(pos) abs.env) abs) with
        prepared_call }

  let access_param abs dst =
    match abs.prepared_call with
    | None -> failwith "malformed call sequence"
    | Some { closure; param } as prepared_call ->
      { (set_env dst (get_env param abs.env) abs) with
        prepared_call }

end

let g = H.create ()

let v0 = "v0"
let v1 = "v1"
let v2 = "v2"
let v3 = "v3"
let v4 = "v4"
let v5 = "v5"

let () =
  let vert = [v0;v1;v2;v3;v4;v5] in
  List.iter (fun v -> H.add_vertex g v ()) vert;
  H.add_hedge g 01 () ~pred:[|v0|] ~succ:[|v1|];
  H.add_hedge g 12 () ~pred:[|v1|] ~succ:[|v2|];
  H.add_hedge g 23 () ~pred:[|v2|] ~succ:[|v3|];
  H.add_hedge g 34 () ~pred:[|v3|] ~succ:[|v4|];
  H.add_hedge g 45 () ~pred:[|v4|] ~succ:[|v5|]

let g_func = H.create ()

let v_in = "v_in"
let v7 = "v7"
let v8 = "v8"
let v_out = "v_out"

let () =
  let vert = [v_in; v_out; v7; v8] in
  List.iter (fun v -> H.add_vertex g_func v ()) vert;
  H.add_hedge g_func 67 () ~pred:[|v_in|] ~succ:[|v7|];
  H.add_hedge g_func 78 () ~pred:[|v7|] ~succ:[|v8|];
  H.add_hedge g_func 89 () ~pred:[|v8|] ~succ:[|v_out|]

let vset_list l = List.fold_right H.VertexSet.add l H.VertexSet.empty
let hset_list l = List.fold_right H.HedgeSet.add l H.HedgeSet.empty

let func_subgraph =
  { H.sg_input = [|v_in|];
    H.sg_output = [|v_out|];
    H.sg_vertex = vset_list [v7;v8];
    H.sg_hedge = hset_list [67;78;89] }

module Manager = struct
  module H = H
  type abstract = Env.t

  type vertex_attribute = unit
  type hedge_attribute = unit
  type graph_attribute = unit

  let a_id = Id.create ~name:"a" ()
  let b_id = Id.create ~name:"b" ()
  let c_id = Id.create ~name:"c" ()
  let f_id = Id.create ~name:"f" ()
  let closure_id = Id.create ~name:"f" ()

  let n_cst = Constant.create ~name:"n" ()
  let m_cst = Constant.create ~name:"m" ()

  let func = F.create ~name:"func" ()

  let apply hedge () tabs =
    let abs = tabs.(0) in
    match hedge with
    | 01 -> [|Env.set_int abs a_id n_cst|], [] (* a <- n *)
    | 12 -> [|Env.set_int abs b_id m_cst|], [] (* b <- m *)


    | 23 -> [|Env.set_closure abs f_id func [|a_id|]|], [] (* f <- closure{a} *)
    | 34 -> [|Env.prepare_call abs f_id b_id|], [] (* prepare call f b *)
    | 45 -> Env.call abs (* call f b *)

    | 67 -> [|Env.access_closure abs a_id func 0|], [] (* a <- closure.(0) *)
    | 78 -> [|Env.access_param abs b_id|], [] (* b <- param *)
    | 89 -> [|Env.add_int abs a_id b_id c_id|], [] (* c <- a + b *)
    | _ -> failwith ""

  let abstract_init i =
    if i = "v0"
    then Env.empty
    else Env.bottom

  let bottom _ = Env.bottom
  let is_bottom _ = Env.is_bottom_env
  let join_list _ l =
    List.fold_left Env.join Env.bottom l

  let is_leq _ = Env.is_leq

  type function_id = Data.f

  let find_function id =
    assert(F.equal id func);
    g_func, func_subgraph

  (* baaad: but we don't have attributes on vertex and hedge... *)
  let clone_vertex v = v
  let clone_hedge h = h

end

module FP = Hgraph.Fixpoint(Manager)
let r = FP.kleene_fixpoint g (Manager.H.VertexSet.singleton v0)

let print_env ppf attr =
  let open Format in
  let open Env in
  match attr.env with
  | Bottom -> fprintf ppf "Bottom"
  | Env env ->
    fprintf ppf "{@[ ";
    Idm.iter (fun id _ -> fprintf ppf "%a " Id.print id) env;
    fprintf ppf "@]}"

let print_attrvertex ppf vertex attr =
  Format.fprintf ppf "%s %a" vertex print_env attr

(* let print_attrhedge ppf hedge attr = *)
(*   Format.pp_print_int ppf hedge *)

let () =
  H.print_dot
    ~print_attrvertex
    (* ~print_attrhedge *)
    Format.std_formatter r
