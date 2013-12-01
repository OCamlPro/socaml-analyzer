open Format
open Common_types
open Tlambda

let boxed_integer_mark name bi =
  let open Lambda in
  match bi with
  | Pnativeint -> Printf.sprintf "Nativeint.%s" name
  | Pint32 -> Printf.sprintf "Int32.%s" name
  | Pint64 -> Printf.sprintf "Int64.%s" name

let boxed_integer_name n =
  let open Lambda in match n with
  | Pnativeint -> "nativeint"
  | Pint32 -> "int32"
  | Pint64 -> "int64"

let print_boxed_integer name ppf bi =
  fprintf ppf "%s" (boxed_integer_mark name bi)

let print_boxed_integer_conversion ppf bi1 bi2 =
  fprintf ppf "%s_of_%s" (boxed_integer_name bi2) (boxed_integer_name bi1)

let record_rep ppf r =
  match r with
  | Types.Record_regular -> fprintf ppf "regular"
  | Types.Record_float -> fprintf ppf "float"

let print_bigarray name unsafe kind ppf layout =
  let open Lambda in
  fprintf ppf "Bigarray.%s[%s,%s]"
    (if unsafe then "unsafe_"^ name else name)
    (match kind with
     | Pbigarray_unknown -> "generic"
     | Pbigarray_float32 -> "float32"
     | Pbigarray_float64 -> "float64"
     | Pbigarray_sint8 -> "sint8"
     | Pbigarray_uint8 -> "uint8"
     | Pbigarray_sint16 -> "sint16"
     | Pbigarray_uint16 -> "uint16"
     | Pbigarray_int32 -> "int32"
     | Pbigarray_int64 -> "int64"
     | Pbigarray_caml_int -> "camlint"
     | Pbigarray_native_int -> "nativeint"
     | Pbigarray_complex32 -> "complex32"
     | Pbigarray_complex64 -> "complex64")
    (match layout with
    |  Pbigarray_unknown_layout -> "unknown"
     | Pbigarray_c_layout -> "C"
     | Pbigarray_fortran_layout -> "Fortran")

let id_list ppf ids = List.iter (fun l -> fprintf ppf "@ %a" TId.print l) ids

let rec tlambda ppf = function
  | Tlet t -> tlet ppf t
  | Trec t -> trec ppf t
  | Tend tid -> tend ppf tid

and tlet ppf tl =
  let rec aux ppf = function
    | Tlet t -> one ppf t
    | Trec t -> trec ppf t
    | Tend tid -> tend ppf tid
  and one ppf { te_id; te_lam; te_kind; te_in } =
    fprintf ppf "@[@[<2>let %a@ =@ @[<2>%a@]@] in@ %a@]"
      TId.print te_id
      tcontrol te_lam
      aux te_in
  in
  fprintf ppf "@[<2>%a@]" one tl

and trec trec = failwith "todo print trec"

and tend ppf tid = fprintf ppf "%a" TId.print tid

and tcontrol ppf = function
  | Tvar id -> TId.print ppf id
  | Tconst c -> Printlambda.structured_constant ppf c
  | Tapply (f, arg) ->
    fprintf ppf "apply@ %a@ %a" TId.print f TId.print arg
  | Tprim (p,args) ->
    fprintf ppf "%a%a" primitive p id_list args
  | Tswitch (case, sw) ->
    let switch ppf sw =
      let spc = ref false in
      List.iter
        (fun (n, l) ->
           if !spc then fprintf ppf "@ | " else spc := true;
           fprintf ppf "@[<hv 1>%i ->@ %a@]" n tlambda l)
        sw.t_consts;
      List.iter
        (fun (n, l) ->
           if !spc then fprintf ppf "@ | " else spc := true;
           fprintf ppf "@[<hv 1>%ip ->@ %a@]" n tlambda l)
        sw.t_blocks ;
      begin match sw.t_failaction with
        | None  -> ()
        | Some l ->
          if !spc then fprintf ppf "@ | " else spc := true;
          fprintf ppf "@[<hv 1>_ ->@ %a@]" tlambda l
      end in
    fprintf ppf
      "@[<1>switch %a with@ %a@]" TId.print case switch sw
  | Tstaticraise (i, ids) ->
    fprintf ppf "staticraise %i %a" i id_list ids
  | Tstaticcatch (body, (n, ids), handler) ->
    fprintf ppf "@[<2>catch@ %a@ with@ %i@ (%a)@ -> %a@]"
      tlambda body n id_list ids tlambda handler
  | Traise id ->
    fprintf ppf "raise@ %a" TId.print id
  | Ttrywith (body, id, handler) ->
    fprintf ppf "@[<2>try@ %a@ with@ %a@ -> %a@]"
      tlambda body TId.print id tlambda handler
  | Tifthenelse (cond, ifso, ifnot) ->
    fprintf ppf "@[<2>if@ %a@ then@ %a@ else@ %a@]"
      TId.print cond tlambda ifso tlambda ifnot
  | Twhile (cond, body) ->
    fprintf ppf "@[<2>while@ %a@ do@ %a@ done@]"
      tlambda cond tlambda body
  | Tfor _ -> failwith "print Tfor"
  | Tlazyforce id ->
    fprintf ppf "lazyforce@ %a" TId.print id
  | Tccall _ -> failwith "print Tccall"
  | Tsend _ -> failwith "print Tsend"

and primitive ppf p =
  let open Asttypes in
  let open Lambda in
  match p with
  | TPfun f -> fprintf ppf "fun %a" F.print f
  | TPgetfun f -> fprintf ppf "getfun %a" F.print f
  | TPfunfield i -> fprintf ppf "funfield %i" i
  | TPgetarg -> fprintf ppf "getarg"
  | TPbuiltin -> fprintf ppf "builtin"

  | TPnegint -> fprintf ppf "~"
  | TPaddint -> fprintf ppf "+"
  | TPsubint -> fprintf ppf "-"
  | TPmulint -> fprintf ppf "*"
  | TPdivint -> fprintf ppf "/"
  | TPmodint -> fprintf ppf "mod"
  | TPandint -> fprintf ppf "and"
  | TPorint -> fprintf ppf "or"
  | TPxorint -> fprintf ppf "xor"
  | TPlslint -> fprintf ppf "lsl"
  | TPlsrint -> fprintf ppf "lsr"
  | TPasrint -> fprintf ppf "asr"
  | TPmakeblock (tag, Immutable) ->
    fprintf ppf "makeblock(%i)" tag
  | TPmakeblock (tag, Mutable) ->
    fprintf ppf "makemutable(%i)" tag
  | TPintcomp(Ceq) -> fprintf ppf "=="
  | TPintcomp(Cneq) -> fprintf ppf "!="
  | TPintcomp(Clt) -> fprintf ppf "<"
  | TPintcomp(Cle) -> fprintf ppf "<="
  | TPintcomp(Cgt) -> fprintf ppf ">"
  | TPintcomp(Cge) -> fprintf ppf ">="
  | TPintoffloat -> fprintf ppf "int_of_float"
  | TPfloatofint -> fprintf ppf "float_of_int"
  | TPnegfloat -> fprintf ppf "~."
  | TPabsfloat -> fprintf ppf "abs."
  | TPaddfloat -> fprintf ppf "+."
  | TPsubfloat -> fprintf ppf "-."
  | TPmulfloat -> fprintf ppf "*."
  | TPdivfloat -> fprintf ppf "/."
  | TPfloatcomp(Ceq) -> fprintf ppf "==."
  | TPfloatcomp(Cneq) -> fprintf ppf "!=."
  | TPfloatcomp(Clt) -> fprintf ppf "<."
  | TPfloatcomp(Cle) -> fprintf ppf "<=."
  | TPfloatcomp(Cgt) -> fprintf ppf ">."
  | TPfloatcomp(Cge) -> fprintf ppf ">=."
  | TPstringlength -> fprintf ppf "string.length"
  | TPstringrefu -> fprintf ppf "string.unsafe_get"
  | TPstringsetu -> fprintf ppf "string.unsafe_set"
  | TPstringrefs -> fprintf ppf "string.get"
  | TPstringsets -> fprintf ppf "string.set"
  | TParraylength _ -> fprintf ppf "array.length"
  | TPmakearray _ -> fprintf ppf "makearray "
  | TParrayrefu _ -> fprintf ppf "array.unsafe_get"
  | TParraysetu _ -> fprintf ppf "array.unsafe_set"
  | TParrayrefs _ -> fprintf ppf "array.get"
  | TParraysets _ -> fprintf ppf "array.set"
  | TPisint -> fprintf ppf "isint"
  | TPisout -> fprintf ppf "isout"
  | TPbittest -> fprintf ppf "testbit"
  | TPstring_load_16(unsafe) ->
     if unsafe then fprintf ppf "string.unsafe_get16"
     else fprintf ppf "string.get16"
  | TPstring_load_32(unsafe) ->
     if unsafe then fprintf ppf "string.unsafe_get32"
     else fprintf ppf "string.get32"
  | TPstring_load_64(unsafe) ->
     if unsafe then fprintf ppf "string.unsafe_get64"
     else fprintf ppf "string.get64"
  | TPstring_set_16(unsafe) ->
     if unsafe then fprintf ppf "string.unsafe_set16"
     else fprintf ppf "string.set16"
  | TPstring_set_32(unsafe) ->
     if unsafe then fprintf ppf "string.unsafe_set32"
     else fprintf ppf "string.set32"
  | TPstring_set_64(unsafe) ->
     if unsafe then fprintf ppf "string.unsafe_set64"
     else fprintf ppf "string.set64"
  | TPbigstring_load_16(unsafe) ->
     if unsafe then fprintf ppf "bigarray.array1.unsafe_get16"
     else fprintf ppf "bigarray.array1.get16"
  | TPbigstring_load_32(unsafe) ->
     if unsafe then fprintf ppf "bigarray.array1.unsafe_get32"
     else fprintf ppf "bigarray.array1.get32"
  | TPbigstring_load_64(unsafe) ->
     if unsafe then fprintf ppf "bigarray.array1.unsafe_get64"
     else fprintf ppf "bigarray.array1.get64"
  | TPbigstring_set_16(unsafe) ->
     if unsafe then fprintf ppf "bigarray.array1.unsafe_set16"
     else fprintf ppf "bigarray.array1.set16"
  | TPbigstring_set_32(unsafe) ->
     if unsafe then fprintf ppf "bigarray.array1.unsafe_set32"
     else fprintf ppf "bigarray.array1.set32"
  | TPbigstring_set_64(unsafe) ->
     if unsafe then fprintf ppf "bigarray.array1.unsafe_set64"
     else fprintf ppf "bigarray.array1.set64"
  | TPbswap16 -> fprintf ppf "bswap16"
  | TPbbswap(bi) -> print_boxed_integer "bswap" ppf bi
  | TPfield n -> fprintf ppf "field %i" n
  | TPsetfield(n, ptr) ->
      let instr = if ptr then "setfield_ptr " else "setfield_imm " in
      fprintf ppf "%s%i" instr n
  | TPfloatfield n -> fprintf ppf "floatfield %i" n
  | TPsetfloatfield n -> fprintf ppf "setfloatfield %i" n
  | TPduprecord (rep, size) -> fprintf ppf "duprecord %a %i" record_rep rep size
  | TPnot -> fprintf ppf "not"
  | TPoffsetint n -> fprintf ppf "%i+" n
  | TPoffsetref n -> fprintf ppf "+:=%i"n
  | TPbintofint bi -> print_boxed_integer "of_int" ppf bi
  | TPintofbint bi -> print_boxed_integer "to_int" ppf bi
  | TPcvtbint (bi1, bi2) -> print_boxed_integer_conversion ppf bi1 bi2
  | TPnegbint bi -> print_boxed_integer "neg" ppf bi
  | TPaddbint bi -> print_boxed_integer "add" ppf bi
  | TPsubbint bi -> print_boxed_integer "sub" ppf bi
  | TPmulbint bi -> print_boxed_integer "mul" ppf bi
  | TPdivbint bi -> print_boxed_integer "div" ppf bi
  | TPmodbint bi -> print_boxed_integer "mod" ppf bi
  | TPandbint bi -> print_boxed_integer "and" ppf bi
  | TPorbint bi -> print_boxed_integer "or" ppf bi
  | TPxorbint bi -> print_boxed_integer "xor" ppf bi
  | TPlslbint bi -> print_boxed_integer "lsl" ppf bi
  | TPlsrbint bi -> print_boxed_integer "lsr" ppf bi
  | TPasrbint bi -> print_boxed_integer "asr" ppf bi
  | TPbintcomp(bi, Ceq) -> print_boxed_integer "==" ppf bi
  | TPbintcomp(bi, Cneq) -> print_boxed_integer "!=" ppf bi
  | TPbintcomp(bi, Clt) -> print_boxed_integer "<" ppf bi
  | TPbintcomp(bi, Cgt) -> print_boxed_integer ">" ppf bi
  | TPbintcomp(bi, Cle) -> print_boxed_integer "<=" ppf bi
  | TPbintcomp(bi, Cge) -> print_boxed_integer ">=" ppf bi
  | TPbigarrayref(unsafe, n, kind, layout) ->
      print_bigarray "get" unsafe kind ppf layout
  | TPbigarrayset(unsafe, n, kind, layout) ->
      print_bigarray "set" unsafe kind ppf layout
  | TPbigarraydim(n) -> fprintf ppf "Bigarray.dim_%i" n
  | TPctconst c ->
     let const_name = match c with
       | Big_endian -> "big_endian"
       | Word_size -> "word_size"
       | Ostype_unix -> "ostype_unix"
       | Ostype_win32 -> "ostype_win32"
       | Ostype_cygwin -> "ostype_cygwin" in
     fprintf ppf "sys.constant_%s" const_name

