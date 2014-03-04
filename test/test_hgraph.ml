open Format
open Data
open Utils
open Common_types

module Vertex = struct
  type t = string
  let compare (i:string) j = Pervasives.compare i j
  let hash (i:string) = Hashtbl.hash i
  let equal (i:string) j = i = j

  let print ppf s = Format.pp_print_string ppf s

  let c = ref 0
  let clone v = Printf.sprintf "%s_%i" v (incr c; !c)
end

type hedge_attr =
  | Set_int of tid * int
  | Set_closure of tid * F.t * tid array
  | Prepare_call of tid * tid
  | Call
  | Access_closure of tid * F.t * int
  | Access_param of tid
  | Add_int of tid * tid * tid
  | Ignore
  | Tee of bool

let hedge_attr_to_string = function
  | Set_int _ -> "Set_int"
  | Set_closure _ -> "Set_closure"
  | Prepare_call _ -> "Prepare_call"
  | Call -> "Call"
  | Access_closure _ -> "Access_closure"
  | Access_param _ -> "Access_param"
  | Add_int _ -> "Add_int"
  | Ignore -> "Ignore"
  | Tee _ -> "Tee"

module Hedge = struct
  type t = string
  let compare (i:t) j = Pervasives.compare i j
  let hash (i:t) = Hashtbl.hash i
  let equal (i:t) j = i = j

  let print ppf s = Format.pp_print_string ppf s

  let c = ref 0
  let clone v = Printf.sprintf "%s_%i" v (incr c; !c)
end

module T = struct

  type vertex = Vertex.t
  type hedge = Hedge.t
  module Vertex = Vertex
  module Hedge = Hedge

  let print_vertex = Vertex.print
  let print_hedge = Hedge.print

end

module H = Hgraph.Make(T)

module Env = struct

  type prepared_call =
    { closure : Data.Ids.t;
      param : Data.Ids.t }

  type t =
    { env : Data.environment;
      prepared_call : prepared_call option }

  let get_union ids env =
    Ids.fold (fun id v -> union (get_env id env) v) ids bottom

  let any_int =
    { bottom with int = Int_interv.top }

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
    set_env dst (Int.singleton cst) env

  let add_int env ~src1 ~src2 ~dst =
    env
    |> set_constraint src1 any_int
    |> set_constraint src2 any_int
    |> set_env dst any_int

  let set_closure env dst func_id vars =
    set_env dst (func_val func_id vars) env

  let prepare_call env closure param =
    { env with prepared_call = Some {closure = Ids.singleton closure;
                                     param = Ids.singleton param} }

  let bottom = env_v Envs.bottom
  let empty = env_v Envs.empty
  let join e1 e2 =
    let prepared_call =
      match e1.prepared_call, e2.prepared_call with
      | None, None -> None
      | Some p, None
      | None, Some p -> Some p
      | Some p1, Some p2 ->
        Some { closure = Data.Ids.union p1.closure p2.closure;
               param = Data.Ids.union p1.param p2.param } in
    { env = (Envs.join e1.env e2.env);
      prepared_call }

  let is_bottom_env e = Envs.is_bottom e.env
  let is_leq e1 e2 = Envs.is_leq e1.env e2.env

  let call abs =
    match abs.env with
    | Bottom -> [|bottom|], []
    | Env _ ->
      match abs.prepared_call with
      | None -> failwith "malformed call sequence"
      | Some { closure; param } ->
        let f = (get_union closure abs.env).f in
        let functions = List.map fst (Fm.bindings f) in
        [|bottom|], functions

  let enforce_closure abs func_id =
    match abs.prepared_call with
    | None -> failwith "malformed call sequence"
    | Some { closure; param } ->
      let f = (get_union closure abs.env).f in
      let used_closure = Fm.find func_id f in
      let closure_val = func_val' func_id used_closure in
      let abs = Ids.fold (fun id acc -> set_env id closure_val acc) closure abs in
      abs, used_closure

  let access_closure abs dst func_id pos =
    match abs.env with
    | Bottom -> bottom
    | Env _ ->
      match abs.prepared_call with
      | None -> failwith "malformed call sequence"
      | Some { closure; param } as prepared_call ->
        let abs, used_closure = enforce_closure abs func_id in
        { (set_env dst (get_union used_closure.(pos) abs.env) abs) with
          prepared_call }

  let access_param abs dst =
    match abs.env with
    | Bottom -> bottom
    | Env _ ->
      match abs.prepared_call with
      | None -> failwith "malformed call sequence"
      | Some { closure; param } as prepared_call ->
        { (set_env dst (get_union param abs.env) abs) with
          prepared_call }

end

let g = H.create ()

let tid s = ( "", ( Id.create ~name:s () ) )

let a_id = tid "a"
let b_id = tid "b"
let c_id = tid "c"
let f_id = tid "f"
let closure_id = tid "f"

let n_cst = 1
let m_cst = 42

let func = F.create ~name:"func" ()

let v0 = "v0"
let v1 = "v1"
let v2 = "v2"
let v3 = "v3"
let v4 = "v4"
let v5 = "v5"

let () =
  let vert = [v0;v1;v2;v3;v4;v5] in
  List.iter (fun v -> H.add_vertex g v ()) vert;
  H.add_hedge g "0.1" (Set_int (a_id,n_cst)) ~pred:[|v0|] ~succ:[|v1|];
  H.add_hedge g "1.2" (Set_int (b_id,m_cst)) ~pred:[|v1|] ~succ:[|v2|];
  H.add_hedge g "2.3" (Set_closure (f_id, func, [|a_id|])) ~pred:[|v2|] ~succ:[|v3|];
  H.add_hedge g "3.4" (Prepare_call (f_id, b_id)) ~pred:[|v3|] ~succ:[|v4|];
  H.add_hedge g "4.5" Call ~pred:[|v4|] ~succ:[|v5|]


let g_func = H.create ()

let v_in = "v.in"
let v7 = "v7"
let v8 = "v8"
let v9 = "v9"
let v10 = "v10"
let v11 = "v11"
let v12 = "v12"
let v13 = "v13"
let v_out = "v.out"

let in_vert = [v7; v8; v9; v10; v11; v12; v13] 

let () =
  let vert = [v_in; v_out] @ in_vert in
  List.iter (fun v -> H.add_vertex g_func v ()) vert;
  H.add_hedge g_func "6.7" (Access_closure (a_id, func, 0)) ~pred:[|v_in|] ~succ:[|v7|];
  H.add_hedge g_func "7.8" (Access_param b_id) ~pred:[|v7|] ~succ:[|v8|];
  H.add_hedge g_func "8.9" (Add_int (a_id, b_id, c_id)) ~pred:[|v8|] ~succ:[|v9|];

  H.add_hedge g_func "9.10" (Set_closure (f_id, func, [|b_id|])) ~pred:[|v9|] ~succ:[|v10|];
  H.add_hedge g_func "10.11" (Prepare_call (f_id, c_id)) ~pred:[|v10|] ~succ:[|v11|];

  H.add_hedge g_func "11.12" Call ~pred:[|v11|] ~succ:[|v12|];

  H.add_hedge g_func "12.13" (Add_int (c_id, b_id, c_id)) ~pred:[|v12|] ~succ:[|v13|];
  H.add_hedge g_func "9.13" (Set_int (c_id,m_cst)) ~pred:[|v9|] ~succ:[|v13|];
  H.add_hedge g_func "13.14" (Tee true) ~pred:[|v13|] ~succ:[|v_out;v_in|]

let vset_list l = List.fold_right H.VertexSet.add l H.VertexSet.empty
let hset_list l = List.fold_right H.HedgeSet.add l H.HedgeSet.empty

let func_subgraph =
  { H.sg_input = [|v_in|];
    H.sg_output = [|v_out|];
    H.sg_vertex = vset_list in_vert;
    H.sg_hedge = hset_list ["6.7";"7.8";"8.9";"9.10";"10.11";"11.12";"12.13";"13.14";"9.13"] }

module Manager = struct
  module H = H
  type abstract = Env.t

  type vertex_attribute = unit
  type hedge_attribute = hedge_attr
  type graph_attribute = unit

  let apply hedge attr tabs =
    let abs = tabs.(0) in
    Printf.eprintf "%s %s\n%!" hedge (hedge_attr_to_string attr);
    match attr with
    | Set_int (id,cst) ->
      [|Env.set_int abs id cst|], [] (* id <- cst *)
    | Set_closure (fun_id, funct, id_arr ) ->
      [|Env.set_closure abs fun_id funct id_arr|], [] (* f <- closure{a} *)
    | Prepare_call (fun_id, clos_id) ->
      [|Env.prepare_call abs fun_id clos_id|], [] (* prepare call f b *)
    | Call ->
      Env.call abs
    | Access_closure(id, funct, n) ->
      [|Env.access_closure abs id funct n|], [] (* a <- closure.(0) *)
    | Access_param(id) ->
      [|Env.access_param abs id|], [] (* b <- param *)
    | Add_int (id1,id2,id3) ->
      [|Env.add_int abs id1 id2 id3|], [] (* c <- a + b *)
    | Ignore -> [|abs|], []

    | Tee b ->
      if b
      then [|abs;Env.bottom|], []
      else [|Env.bottom;abs|], []

  let abstract_init i =
    if i = "v0"
    then Env.empty
    else Env.bottom

  let bottom _ = Env.bottom
  let is_bottom _ = Env.is_bottom_env
  let join_list _ l =
    List.fold_left Env.join Env.bottom l

  let widening _ a1 a2 = Env.join a1 a2

  let narrowing = None

  let is_leq _ = Env.is_leq

  type function_id = F.t
  module Function_id = F

  let find_function id =
    assert(F.equal id func);
    g_func, func_subgraph

  module Stack = Abstract_stack.TwoLevels ( Function_id )

end

let print_env ppf attr =
  let open Format in
  let open Env in
  match attr.env with
  | Bottom -> fprintf ppf "Bottom"
  | Env env ->
    fprintf ppf "{@[ ";
    Idm.iter (fun (_,id) _ -> fprintf ppf "%a " Id.print id) env;
    fprintf ppf "@]}"

let print_attrvertex ppf vertex attr =
  Format.fprintf ppf "%s %a" vertex print_env attr

let ouput_dot g =
  H.print_dot
    ~print_attrvertex
    (* ~print_attrhedge *)
    Format.std_formatter g

module FP = Fixpoint.Fixpoint(T)(Manager)
(* let err_graph = ref None *)
let r, map =
  try FP.kleene_fixpoint (* ~err_graph *) g (Manager.H.VertexSet.singleton v0)
  with e ->
    (* (match !err_graph with *)
    (*  | None -> assert false *)
    (*  | Some g -> *)
    (*    let print_attrvertex ppf vertex attr = Format.pp_print_string ppf vertex in *)
    (*    H.print_dot *)
    (*      ~print_attrvertex *)
    (*      (\* ~print_attrhedge *\) *)
    (*      Format.std_formatter g); *)
    raise e

let () = ouput_dot r
