open Lambda

class mapper =
object (self)

  method lambda = function
  | Lvar i -> self#var i
  | Lconst c -> self#const c
  | Lapply ( f, a, l) -> self#apply f a l
  | Lfunction ( k, l, b) -> self#func k l b
  | Llet ( k, i, e, b) -> self#letin k i e b
  | Lletrec ( l, b) -> self#letrec l b
  | Lprim ( p, l) -> self#prim p l
  | Lswitch ( l, s) -> self#switch l s
  | Lstaticraise ( i, l) -> self#staticraise i l
  | Lstaticcatch ( l, (i,ids), l2) -> self#staticcatch l i ids l2
  | Ltrywith ( l, i, l2) -> self#trywith l i l2
  | Lifthenelse ( i, t, e) -> self#ifthenelse i t e
  | Lsequence ( l1, l2) -> self#sequence l1 l2
  | Lwhile ( h, b) -> self#whiledo h b
  | Lfor ( i, min, max, dir, b) -> self#fordo i min max dir b
  | Lassign ( i, l) -> self#assign i l
  | Lsend ( k, o, m, a, loc) -> self#send k o m a loc
  | Levent ( l, e) -> self#event l e
  | Lifused ( i, l)  -> self#ifused i l

  method private map = List.map self#lambda
  method private map2 :
  type t. (t * lambda) list -> (t * lambda) list =
    List.map (fun ( i, lam) -> ( i, self#lambda lam))
  method var x = Lvar x
  method const c = Lconst c
  method apply func args loc =
    (* the order is unspecified, i do as i want *)
    let func = self#lambda func in
    let args = self#map args in
    Lapply ( func, args, loc)

  (* named this way because of reserved keyword *)
  method func kind l body =
    Lfunction ( kind, l, self#lambda body)
    
  (* named this way because of reserved keyword *)
  method letin kind id expr body =
    let expr = self#lambda expr in
    Llet ( kind, id, expr, self#lambda body)

  method letrec l body =
    let l = self#map2 l in
    Lletrec ( l, (self#lambda body))

  method prim p l = Lprim ( p, self#map l)

  method switch lam s =
    let lam = self#lambda lam in
    let consts = self#map2 s.sw_consts in
    let blocks = self#map2 s.sw_blocks in
    let fail = match s.sw_failaction with None -> None | Some l -> Some (self#lambda l) in
    Lswitch (lam,
	     { s with
	       sw_consts = consts;
	       sw_blocks = blocks;
	       sw_failaction = fail;
	     })

  method staticraise i l = Lstaticraise ( i, self#map l)
  method staticcatch lam i ids lam2 =
    let lam = self#lambda lam in
    Lstaticcatch ( lam, (i, ids), self#lambda lam2)

  method trywith lam id lam2 =
    let lam = self#lambda lam in
    Ltrywith ( lam, id, self#lambda lam2)
  method ifthenelse lif lthen lelse =
    let lif = self#lambda lif in
    let lthen = self#lambda lthen in
    Lifthenelse ( lif, lthen, self#lambda lelse)

  method sequence l1 l2 =
    let l1 = self#lambda l1 in
    Lsequence ( l1, self#lambda l2)

  (* named this way because of reserved keyword *)
  method whiledo lam ldo =
    let lam = self#lambda lam in
    Lwhile ( lam, self#lambda ldo)
  (* named this way because of reserved keyword *)
  method fordo id linit lmax direction body =
    let linit = self#lambda linit in
    let lmax = self#lambda lmax in
    Lfor ( id, linit, lmax, direction, self#lambda body)

  method assign i lam = Lassign ( i, self#lambda lam)

  method send kind obj meth args loc =
    let obj = self#lambda obj in
    let meth = self#lambda meth in
    Lsend ( kind, obj, meth, self#map args, loc)

  method event lam ev = Levent ( self#lambda lam, ev)

  method ifused id lam = Lifused ( id, self#lambda lam)
end
