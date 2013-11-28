type fun_applyer = Def_c_fun.fun_applyer

let register = Def_c_fun.add_def

let default = Def_c_fun.default

type basetype =
  Int | IntN | Int32 | Int64 | Float | Floata | Const | Block | String | Func

let rec aux d res = function
  | [] -> res
  |  h :: tl ->    
    aux d
      (
        let open Data in
        match h with
        | Int -> { res with int = d.int }
        | IntN -> { res with inat = d.inat }
        | Int32 -> { res with i32 = d.i32 }
        | Int64 -> { res with i64 = d.i64 }
        | Float -> { res with float = d.float }
        | Floata -> { res with floata = d.floata }
        | Const -> { res with cp = d.cp }
        | Block -> { res with blocks = d.blocks }
        | String -> { res with string = d.string }
        | Func -> { res with f = d.f }
      )
      tl


let restrain l d =
  in aux d Data.bottom l

let remove l d =
  aux Data.bottom d l

        

let is_only
