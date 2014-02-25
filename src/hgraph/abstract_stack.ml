open Hgraph_types
open Stack_types

let equal_option eq v1 v2 =
  match v1,v2 with
  | None, None -> true
  | Some v1, Some v2 -> eq v1 v2
  | _ -> false

let compare_option comp v1 v2 =
  match v1,v2 with
  | None, None -> 0
  | None, Some _ -> -1
  | Some _, None -> 1
  | Some v1, Some v2 -> comp v1 v2

let hash_option hash = function
  | None -> 0
  | Some v -> hash v

module OneLevel (T:OrderedHashedType) : Stack with type elt = T.t =
struct
  type elt = T.t
  type t = elt option

  let empty = None

  let push _ e = Some e
  let equal = equal_option T.equal
  let compare = compare_option T.compare
  let hash = hash_option T.hash
  let print ppf = function
    | None -> Format.fprintf ppf "empty stack"
    | Some s -> T.print ppf s
end

module TwoLevels (T:OrderedHashedType) : Stack with type elt = T.t =
struct
  type elt = T.t
  type t =
    { context : elt option;
      current : elt option }

  let empty = { context = None; current = None }

  let push { current } e =
    { context = current;
      current = Some e }

  let equal t1 t2 =
    equal_option T.equal t1.current t2.current &&
    equal_option T.equal t1.context t2.context

  let compare t1 t2 =
    let c = compare_option T.compare t1.current t2.current in
    if c = 0
    then compare_option T.compare t1.context t2.context
    else c

  let hash t =
    Hashtbl.hash (hash_option T.hash t.current,
                  hash_option T.hash t.context)

  let print ppf = function
    | { current = None } -> Format.fprintf ppf "empty stack"
    | { context = None; current = Some s } ->
      Format.fprintf ppf "(%a)" T.print s
    | { context = Some s2; current = Some s1 } ->
      Format.fprintf ppf "(%a::%a)" T.print s1 T.print s2
end

module Local (Top:Stack) : Stack with type elt = Top.elt =
struct

  module TopSet = Set.Make(Top)

  type elt = Top.elt

  type t =
    { env : TopSet.t;
      top : Top.t }

  let empty = { env = TopSet.singleton Top.empty; top = Top.empty }

  let push { env; top = stack } elt =
    let top = Top.push stack elt in
    let env = TopSet.add top env in
    { env; top }

  let equal s1 s2 =
    Top.equal s1.top s2.top &&
    TopSet.equal s1.env s2.env

  let compare s1 s2 =
    let c = Top.compare s1.top s2.top in
    if c = 0
    then TopSet.compare s1.env s2.env
    else c

  let hash { env; top } =
    Hashtbl.hash (Top.hash top, TopSet.cardinal env)

  let print ppf { top } = Top.print ppf top

end
