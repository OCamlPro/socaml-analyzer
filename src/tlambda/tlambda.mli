open Common_types

type tlambda =
| Tlet of tlet
| Trec of trec
| Tend of id

and tlet =
{
  te_id : id;
  te_lam : tcontrol;
  te_kind : Lambda.let_kind;
  te_in : tlambda;
}

and trec =
{
  tr_decls : ( id * primitive * id list ) list;
  tr_in : tlambda;
}

and tcontrol =
| Tvar of id
| Tconst of Lambda.structured_constant
| Tapply of id * id
| Tprim of primitive * id list
| Tswitch of id * tswitch
| Tstaticraise of int * id list
| Tstaticcatch of tlambda * (int * id list) * tlambda
| Traise of id
| Ttrywith of tlambda * id * tlambda
| Tifthenelse of id * tlambda * tlambda
| Twhile of tlambda * tlambda
| Tfor of id * id * id * direction_flag * tlambda
(* | Tassign of id * id *)
| Tlazyforce of id
| Tccall of Primitive.description * id list
| Tsend of Lambda.meth_kind * id * id

and tswitch =
{
  t_numconsts: int;
  t_consts: (int * tlambda) list;
  t_numblocks: int;
  t_blocks: (int * tlambda) list;
  t_failaction: tlambda option;
}
