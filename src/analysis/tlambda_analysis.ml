open Common_types
open Lambda
open Tlambda
open Tlambda_to_hgraph
module G = G

open Data

type v = Vertex.t
type h = Hedge.t
type e = environment
type ha = hattr

let intop2_of_prim o =
  let open Int_interv in
  match o with
  | TPaddint -> add
  | TPsubint -> sub
  | TPmulint -> mul
  | TPdivint -> div
  | TPmodint -> modulo
  | TPandint -> band
  | TPorint -> bor
  | TPxorint -> bxor
  | TPlslint -> blsl
  | TPlsrint -> blsr
  | TPasrint -> basr
  | _ -> assert false

let rev_comp = function
  | Ceq -> Cneq | Cneq -> Ceq | Clt -> Cge | Cgt -> Cle | Cle -> Cgt | Cge -> Clt

let may_rev_comp c cp =
  if cp = 0 then rev_comp c else c

let list_of_one f = function
  | [x] -> f x
  | _ -> assert false


let rec constraint_env_cp_var id cp env =
  let d = get_env id env in
  let l = d.expr in
  if Cps.has cp d
  then
    if Cps.is_one d env
    then env
    else
      begin
	constraint_env_cp_list l cp env
	|> set_env id (Cps.restrict ~v:cp d)
      end
  else Envs.bottom

and constraint_env_cp_list l cp env =
  List.fold_left
    (fun e expr -> Envs.join e ( constraint_env_cp expr cp env ) )
    Envs.bottom l

and constraint_env_cp expr cp env =
  match expr with
  | Var x -> constraint_env_cp_var x cp env
  | Prim ( p, l ) ->
    begin
      match p, l with
      | TPintcomp c, [x;y]  ->
	if cp > 1
	then Envs.bottom
	else
	  let c = may_rev_comp c cp in
	  let x' = get_env x env
	  and y' = get_env y env in
	  let (x',y') = Int.make_comp c x' y' in
	  set_env x x' env
      |> set_env y y'
      | TPsetfield _, _::_::[]
      | TPsetfloatfield _, _::_::[]
	  when cp = 0 -> env
      | TPfield i, [b] ->
	let ids = Blocks.get_field i (get_env b env) in
	Ids.fold
	    (fun id acc ->
	      Envs.join acc
		( constraint_env_cp_var id cp env)
	    ) ids Envs.bottom
      | TPnot, [x] when cp < 2 ->
	constraint_env_cp_var x (1-cp) env
      | TPisint, [x] when cp = 0 ->
	set_env x ( Int.restrict_not_intcp (get_env x env) ) env
      | TPisint, [x] when cp = 1 ->
	set_env x ( Int.restrict_intcp (get_env x env) ) env
      | TPisout, [x;y;z] when cp = 0 -> failwith "TODO: isout"
      | TPisout, [x;y;z] when cp = 1 -> failwith "TODO: isout"
      | TPbittest, [x] when cp = 0 -> failwith "TODO: bittest"
      | TPbittest, [x] when cp = 1 -> failwith "TODO: bittest"
      | TPctconst Lambda.Word_size, [] -> Envs.bottom
      | TPctconst _, [] when cp < 2 -> env (* to correct ? *)
      | _, _ -> Envs.bottom
    end
  | _ -> env

let rec constraint_env_tag_var id tag env =
  let d = get_env id env in
  let l = d.expr in
  if Blocks.has_tag tag d
  then
    if Blocks.is_one_tag d env
    then env
    else
      begin
	constraint_env_tag_list l tag env
	|> set_env id (Blocks.restrict ~tag d)
      end
  else Envs.bottom

and constraint_env_tag_list l tag env =
  List.fold_left
    (fun e expr -> Envs.join e ( constraint_env_tag expr tag env ) )
    Envs.bottom l

and constraint_env_tag expr tag env =
  match expr with
  | Var x -> constraint_env_tag_var x tag env
  | Const _
  | App ( _, _ )
  | Return _| Retexn _
  | Lazyforce _
  | Ccall (_, _)
  | Send (_, _) -> env
  | Prim ( p, l ) ->
    begin
      match p with
      | TPmakeblock (t, _) when t = tag ->  env
      | TPfield i ->
	list_of_one (fun b ->
	  let b = get_env b env in
	  Ids.fold
	    (fun id acc ->
	      Envs.join acc
		( constraint_env_tag_var id tag env)
	    ) ( Blocks.get_field i b) Envs.bottom
	) l
      | TPduprecord _ when tag = 0 ->
	list_of_one (fun b -> constraint_env_tag_var b tag env ) l
      | _ -> Envs.bottom
    end
  | Constraint _ -> assert false

module type Entry =
sig
  val inv : v
  val outv : v
  val exnv : v
  val g : hg
  val funs : ( F.t, Tlambda_to_hgraph.fun_desc ) Hashtbl.t
  val mk_vertex : unit -> v
  val mk_hedge : unit -> Hedge.t
end

module M : functor ( E : Entry ) ->
  Hgraph.Manager
  with module T := T
   and module H = G 
   and type hedge_attribute = hattr
   and type vertex_attribute = vattr
   and type graph_attribute = gattr
   = functor ( E : Entry ) ->
   struct

     module H = Tlambda_to_hgraph.G

     open H

     type hedge = h
     type vertex = v
     type abstract = e

     let bottom _ = Envs.bottom
     let is_bottom _ = Envs.is_bottom
     let is_leq _ = Envs.is_leq
     let join_list _ = List.fold_left Envs.join Envs.bottom
     let abstract_init v = if v = E.inv then Envs.empty else Envs.bottom

     type hedge_attribute = hattr
     type vertex_attribute = vattr
     type graph_attribute = gattr

     type function_id = F.t
     module Function_id = F
     let find_function fid =
       let f = Hashtbl.find E.funs fid in
       f.f_graph, {
         sg_input = f.f_in;
         sg_output = f.f_out;
         sg_vertex = f.f_vertex;
         sg_hedge = f.f_hedge;
       }

     let clone_vertex _ = E.mk_vertex ()
     let clone_hedge _ = E.mk_hedge ()

     let fun_tid = TId.create ()
     let ret_tid = TId.create ()
     let exn_tid = TId.create ()
     let arg_tid = TId.create ()

     let rec constant env = function
       | Const_base c ->
         let open Asttypes in
         env,
         begin
           match c with
           | Const_int i -> Int.singleton i
           | Const_char c -> Int.singleton (Char.code c)
           | Const_string s -> Strings.singleton s
           (* Data.singleton_string s *)
           | Const_float s -> Floats.singleton s
           | Const_int32 i -> Otherints.( singleton I32 i )
           | Const_int64 i -> Otherints.( singleton I64 i )
           | Const_nativeint i -> Otherints.( singleton IN i )
         end
       | Const_pointer i -> env, Cps.singleton i
       | Const_block (t,l) ->
         let (env, ids) =
           List.fold_left
             (fun (env,ids) c ->
                let env,d = constant env c in
                let env,i = reg_env d env in
                env,i::ids )
             (env,[]) l
         in
         let a = Array.of_list (List.rev ids) in
         env, ( Blocks.singleton t a )
       | Const_float_array l -> env, Floats.array l
       | Const_immstring s -> env, Strings.singleton s
     (* Data.singleton_string s *)


     let apply (_ :hedge ) ( l : hedge_attribute ) ( envs : abstract array ) =
       let in_apply ( id, action) env =
         let set x = set_env id x env
         and get x = get_env x env
         (* and vunit = Cp.singleton 0 *)
         and act d = Exprs.set d action
         in
         let sa x = set ( act x ) in
         match action with
         | App _ | Lazyforce _ | Ccall _ | Send _ -> assert false
         | Var i -> set ( act (get i) )
         | Const c ->
           let env,d = constant env c in
           set_env id ( act d ) env
         | Prim ( p, l ) ->
           begin
	     match p, l with
	     (* Operations on heap blocks *)
	     | TPmakeblock ( tag, _), _ ->
	       let a = Array.of_list l in
	       sa ( Blocks.singleton tag a )
	     | TPfield i, [b] ->
	       let env =
	         set_env b
	           ( Blocks.restrict ~has_field:i ( get b))
	           env in
	       let ids = Blocks.get_field i ( get_env b env) in
	       set_env id
	         ( act
		     ( Ids.fold
		         (fun id d -> union (get_env id env) d )
		         ids Data.bottom )
	         )
	         env
   (*
| TPsetfield ( i, _), [b;v] ->
let env = set_env b ( Blocks.set_field i v ( get b)) env in
set_env id vunit env *)
	     | TPfloatfield i, [b] -> failwith "TODO: floatfield"
	     | TPsetfloatfield i, [b;v] -> failwith "TODO: setfloatfield"
	     | TPduprecord (trepr,i), [r] -> failwith "TODO: duprecord"
	     (* Force lazy values *)
	     | TPnot, [i] -> set ( Bools.notb ( get i))
	     (* Integer operations *)
	     | TPnegint, [i] -> sa ( Int.op1 Int_interv.uminus ( get i))
	     | TPaddint, [x;y]
	     | TPsubint, [x;y]
	     | TPmulint, [x;y]
	     | TPdivint, [x;y]
	     | TPmodint, [x;y]
	     | TPandint, [x;y]
	     | TPorint, [x;y]
	     | TPxorint, [x;y]
	     | TPlslint, [x;y]
	     | TPlsrint, [x;y]
	     | TPasrint, [x;y] -> sa ( Int.op2 ( intop2_of_prim p) (get x) (get y))
             | TPintcomp c, [x;y] -> 
	       let res, x', y' = Int.comp c ( get x) ( get y) in
	       sa res
	       |> set_env x x'
	       |> set_env y y'
             | TPoffsetint i, [x] ->
               sa ( Int.op1 (Int_interv.addcst i) ( get x) )
             | TPoffsetref i, [x] ->
               let b = get x in
               let b = Blocks.restrict ~tag:0 ~size:1 b in
               set ( Blocks.fieldn_map (fun _ _ v -> failwith "TODO: offsetref") 0 b )
            (*
(* Float operations *)
| TPintoffloat | TPfloatofint
| TPnegfloat | TPabsfloat
| TPaddfloat | TPsubfloat | TPmulfloat | TPdivfloat
| TPfloatcomp of comparison
(* String operations *)
| TPstringlength | TPstringrefu | TPstringsetu | TPstringrefs | TPstringsets *)
             (* Array operations *)
             | TPmakearray Pfloatarray, _ ->
               sa ( { bottom with floata = Top }
             | TParraylength Pfloatarray, _ ->
               sa ( Int.any )
             | TParrayrefu Pfloatarray, _ -> Data.top
             | TPmakearray _, l ->
               sa ( Blocks.singleton 0 (Array.of_list l) )
             | TParraylength k, [a] -> sa ( Block.sizes ~tag:0 (get a) )
             | TParrayrefu k, [a;i] -> sa ( Block.field )
             | TParraysetu k, [a;i;x] ->
(* Test if the argument is a block or an immediate integer *)
             | TPisint, [x] -> sa ( Int.is_int env (get x) )
(* Test if the (integer) argument is outside an interval *)
             | TPisout, [x;y;z] -> sa ( Int.is_out (get x) (get y) (get z) )
(*
(* Bitvect operations *)
| TPbittest
(* Operations on boxed integers (Nativeint.t, Int32.t, Int64.t) *)
| TPbintofint of boxed_integer
| TPintofbint of boxed_integer
| TPcvtbint of boxed_integer (*source*) * boxed_integer (*destination*)
| TPnegbint of boxed_integer
| TPaddbint of boxed_integer
| TPsubbint of boxed_integer
| TPmulbint of boxed_integer
| TPdivbint of boxed_integer
| TPmodbint of boxed_integer
| TPandbint of boxed_integer
| TPorbint of boxed_integer
| TPxorbint of boxed_integer
| TPlslbint of boxed_integer
| TPlsrbint of boxed_integer
| TPasrbint of boxed_integer
| TPbintcomp of boxed_integer * comparison
(* Operations on big arrays: (unsafe, #dimensions, kind, layout) *)
| TPbigarrayref of bool * int * bigarray_kind * bigarray_layout
| TPbigarrayset of bool * int * bigarray_kind * bigarray_layout
(* size of the nth dimension of a big array *)
| TPbigarraydim of int
(* load/set 16,32,64 bits from a string: (unsafe)*)
| TPstring_load_16 of bool
| TPstring_load_32 of bool
| TPstring_load_64 of bool
| TPstring_set_16 of bool
| TPstring_set_32 of bool
| TPstring_set_64 of bool
(* load/set 16,32,64 bits from a
(char, int8_unsigned_elt, c_layout) Bigarray.Array1.t : (unsafe) *)
| TPbigstring_load_16 of bool
| TPbigstring_load_32 of bool
| TPbigstring_load_64 of bool
| TPbigstring_set_16 of bool
| TPbigstring_set_32 of bool
| TPbigstring_set_64 of bool *)
(* Compile time constants *)
             | TPctconst c, [] ->
               let open Lambda in
               begin
                 match c with
                 | Big_endian -> sa ( Bools.of_bool Sys.big_endian )
                 | Word_size -> sa ( Int.singleton Sys.word_size )
                 | Ostype_unix -> sa ( Bools.of_bool Sys.unix )
                 | Ostype_win32 -> sa ( Bools.of_bool Sys.win32 )
                 | Ostype_cygwin -> sa ( Bools.of_bool Sys.cygwin )
               end
(*
(* byte swap *)
| TPbswap16
| TPbbswap of boxed_integer
*)	      
             | TPfunfield i, [f] ->
               (* at this point, f is unique *)
               let x = Funs.field i (get f) env in
               sa x

             | TPgetfun fid, [] -> sa ( Funs.fid fid (get fun_tid ) )
             | TPfun fid, _ -> sa ( Funs.mk fid l )
             | TPgetarg, [] -> sa ( get arg_tid )
	     | _ -> failwith "TODO: primitives !"
           end
         | Constraint c ->
           begin
	     match c with
	     | Ccp cp  -> constraint_env_cp_var id cp env
	     | Ctag tag -> constraint_env_tag_var id tag env
           end
         | Return id -> set_env ret_tid ( get_env id env ) env
         | Retexn id -> set_env exn_tid ( get_env id env ) env
       in
       assert ( Array.length envs = 1 );
       let env = envs.(0) in
       (* Array.fold_left Envs.join Envs.bottom envs in *)
       let rec aux e l =
         match l with
         | [] -> e
         | h :: t -> aux (in_apply h e) t
       in 
       match l with
       | [ _, App ( f, x ) ] ->
         let env =
           env
           |> set_env fun_tid ( get_env f env )
           |> set_env arg_tid ( get_env x env )
         in
         ( [| env; env |], ( fun_ids f env ) )
       | [ id, ( Lazyforce _ as a )]
       | [ id, ( Ccall (_, _) as a ) ]
       | [ id, ( Send (_, _) as a ) ] ->
         [|set_env id ( Exprs.set Data.top a ) env; env |], []
       | _ -> [|aux env l|], []

   end
