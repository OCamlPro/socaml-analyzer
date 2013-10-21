open Common_types

type tlambda =
| Tlet of tlet
| Trec of trec
| Tend of tid

and tlet =
{
  te_id : tid;
  te_lam : tcontrol;
  te_kind : Lambda.let_kind;
  te_in : tlambda;
}

and trec =
{
  tr_decls : ( tid * primitive * tid list ) list;
  tr_in : tlambda;
}

and tcontrol =
| Tvar of tid
| Tconst of Lambda.structured_constant
| Tapply of tid * tid
| Tprim of primitive * tid list
| Tswitch of tid * tswitch
| Tstaticraise of int * tid list
| Tstaticcatch of tlambda * (int * tid list) * tlambda
| Traise of tid
| Ttrywith of tlambda * tid * tlambda
| Tifthenelse of tid * tlambda * tlambda
| Twhile of tlambda * tlambda
| Tfor of tid * tid * tid * direction_flag * tlambda
(* | Tassign of tid * tid *)
| Tlazyforce of tid
| Tccall of Primitive.description * tid list
| Tsend of Lambda.meth_kind * tid * tid

and tswitch =
{
  t_numconsts: int;
  t_consts: (int * tlambda) list;
  t_numblocks: int;
  t_blocks: (int * tlambda) list;
  t_failaction: tlambda option;
}
