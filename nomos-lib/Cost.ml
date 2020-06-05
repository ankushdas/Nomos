module R = Arith
module A = Ast
module F = NomosFlags

let rec cost_tick_aug {A.func_data = d; A.func_structure = e} = {A.func_data = d; A.func_structure = A.Tick(A.Arith (R.Float 1.), {A.func_data = d; A.func_structure = cost_tick e})}

and cost_tick fexp = match fexp with
    A.If(e1,e2,e3) -> A.If(cost_tick_aug e1, cost_tick_aug e2, cost_tick_aug e3)
  | A.LetIn(x,e1,e2) -> A.LetIn(x, cost_tick_aug e1, cost_tick_aug e2)
  | A.Bool _ | A.Int _ | A.Str _ | A.Addr _ | A.Var _ -> fexp
  | A.ListE(l) -> A.ListE(List.map cost_tick_aug l)
  | A.App(es) -> A.App(List.map cost_tick_aug es)
  | A.Cons(e1,e2) -> A.Cons(cost_tick_aug e1, cost_tick_aug e2)
  | A.Match(e1,e2,x,xs,e3) -> A.Match(cost_tick_aug e1, cost_tick_aug e2, x, xs, cost_tick_aug e3)
  | A.Lambda(xs,e) -> A.Lambda(xs, cost_tick_aug e)
  | A.Op(e1,op,e2) -> A.Op(cost_tick_aug e1, op, cost_tick_aug e2)
  | A.CompOp(e1,cop,e2) -> A.CompOp(cost_tick_aug e1, cop, cost_tick_aug e2)
  | A.EqAddr(e1,e2) -> A.EqAddr(cost_tick_aug e1, cost_tick_aug e2)
  | A.RelOp(e1,rop,e2) -> A.RelOp(cost_tick_aug e1, rop, cost_tick_aug e2)
  | A.Tick(pot,e) -> A.Tick(pot, cost_tick_aug e)
  | A.GetTxnNum | A.GetTxnSender -> fexp
  | A.Command(p) -> A.Command(cost_work !F.work p)

and cost_work flag exp = match flag with
    F.Nil -> exp
  | F.Free -> exp
  | F.Recv -> cost_recv_aug exp
  | F.RecvSend -> cost_send_aug (cost_recv_aug exp)
  | F.Send -> cost_send_aug exp
  | F.Flip -> cost_flip_aug exp

and work d p = A.Work(A.Arith (R.Float 1.), {A.st_data = d; A.st_structure = p})

and cost_recv d exp = match exp with
    A.Fwd(x,y) -> A.Fwd(x,y)
  | A.Spawn(x,g,xs,q) -> A.Spawn(x,g,xs, cost_recv_aug q)
  | A.ExpName(x,f,xs) -> A.ExpName(x,f,xs)

  | A.Lab(x,k,p) -> A.Lab(x, k, cost_recv_aug p)
  | A.Case(x,branches) -> work d (A.Case(x, cost_recv_branches branches))

  | A.PLab(x,k,p) -> A.PLab(x, k, cost_recv_aug p)
  | A.PCase(x,branches) -> work d (A.PCase(x, cost_recv_branches branches))
  | A.Flip(pr,p1,p2) -> work d (A.Flip(pr, cost_recv_aug p1, cost_recv_aug p2))

  | A.Send(x,w,p) -> A.Send(x,w,cost_recv_aug p)
  | A.Recv(x,y,p) -> work d (A.Recv(x,y, cost_recv_aug p))

  | A.Close(x) -> A.Close(x)
  | A.Wait(x,p) -> work d (A.Wait(x, cost_recv_aug p))

  | A.Work(pot,p) -> A.Work(pot,cost_recv_aug p)
  | A.Pay(x,pot,p) -> A.Pay(x,pot,cost_recv_aug p)
  | A.Get(x,pot,p) -> A.Get(x,pot,cost_recv_aug p)

  | A.Acquire(x,y,p) -> A.Acquire(x,y,cost_recv_aug p)
  | A.Accept(x,y,p) -> A.Accept(x,y,cost_recv_aug p)
  | A.Release(x,y,p) -> A.Release(x,y,cost_recv_aug p)
  | A.Detach(x,y,p) -> A.Detach(x,y,cost_recv_aug p)

  | A.RecvF(x,y,p) -> work d (A.RecvF(x,y,cost_recv_aug p))
  | A.SendF(x,e,p) -> A.SendF(x, cost_tick_aug e, cost_recv_aug p)

  | A.Let(x,e,p) -> work d (A.Let(x, cost_tick_aug e, cost_recv_aug p))
  | A.IfS(e,p1,p2) -> work d (A.IfS(cost_tick_aug e, cost_recv_aug p1, cost_recv_aug p2))

  | A.MakeChan(x,a,n,p) -> work d (A.MakeChan(x, a, n, cost_recv_aug p))
  | A.Abort -> work d (A.Abort)
  (* TODO: add ticks to argument list of spawn and print *)
  | A.Print(l,arg,p) -> work d (A.Print(l,arg, cost_recv_aug p))

and cost_recv_aug {A.st_data = d; A.st_structure = p} = {A.st_data = d; A.st_structure = cost_recv d p}

and cost_recv_branches bs = match bs with
    [] -> []
  | (l,p)::branches ->
      (l, cost_recv_aug p)::(cost_recv_branches branches)

and cost_send d exp = match exp with
    A.Fwd(x,y) -> A.Fwd(x,y)
  | A.Spawn(x,g,xs,p) -> A.Spawn(x,g,xs, cost_send_aug p)
  | A.ExpName(x,f,xs) -> A.ExpName(x,f,xs)

  | A.Lab(x,k,p) -> work d (A.Lab(x, k, cost_send_aug p))
  | A.Case(x,branches) -> A.Case(x, cost_send_branches branches)

  | A.PLab(x,k,p) -> work d (A.PLab(x, k, cost_send_aug p))
  | A.PCase(x,branches) -> A.PCase(x, cost_send_branches branches)
  | A.Flip(pr,p1,p2) -> work d (A.Flip(pr, cost_send_aug p1, cost_send_aug p2))

  | A.Send(x,w,p) -> work d (A.Send(x,w, cost_send_aug p))
  | A.Recv(x,y,p) -> A.Recv(x,y, cost_send_aug p)

  | A.Close(x) -> work d (A.Close(x))
  | A.Wait(x,p) -> A.Wait(x, cost_send_aug p)

  | A.Work(pot,p) -> A.Work(pot, cost_send_aug p)
  | A.Pay(x,pot,p) -> A.Pay(x,pot, cost_send_aug p)
  | A.Get(x,pot,p) -> A.Get(x,pot, cost_send_aug p)

  | A.Acquire(x,y,p) -> A.Acquire(x,y,cost_send_aug p)
  | A.Accept(x,y,p) -> A.Accept(x,y,cost_send_aug p)
  | A.Release(x,y,p) -> A.Release(x,y,cost_send_aug p)
  | A.Detach(x,y,p) -> A.Detach(x,y,cost_send_aug p)

  | A.RecvF(x,y,p) -> A.RecvF(x,y,cost_send_aug p)
  | A.SendF(x,e,p) -> work d (A.SendF(x, cost_tick_aug e, cost_send_aug p))

  | A.Let(x,e,p) -> work d (A.Let(x, cost_tick_aug e, cost_send_aug p))
  | A.IfS(e,p1,p2) -> work d (A.IfS(cost_tick_aug e, cost_send_aug p1, cost_send_aug p2))

  | A.MakeChan(x,a,n,p) -> work d (A.MakeChan(x, a, n, cost_send_aug p))
  | A.Abort -> work d (A.Abort)
  (* TODO: add ticks to argument list of spawn and print *)
  | A.Print(l,arg,p) -> work d (A.Print(l, arg, cost_send_aug p))

and cost_send_aug {A.st_data = d; A.st_structure = p} = {A.st_data = d; A.st_structure = cost_send d p}

and cost_send_branches bs = match bs with
    [] -> []
  | (l,p)::branches ->
      (l, cost_send_aug p)::(cost_send_branches branches)

and cost_flip d exp = match exp with
    A.Fwd(x,y) -> A.Fwd(x,y)
  | A.Spawn(x,g,xs,p) -> A.Spawn(x,g,xs, cost_flip_aug p)
  | A.ExpName(x,f,xs) -> A.ExpName(x,f,xs)

  | A.Lab(x,k,p) -> A.Lab(x, k, cost_flip_aug p)
  | A.Case(x,branches) -> A.Case(x, cost_flip_branches branches)

  | A.PLab(x,k,p) -> A.PLab(x, k, cost_flip_aug p)
  | A.PCase(x,branches) -> A.PCase(x, cost_flip_branches branches)
  | A.Flip(pr,p1,p2) -> work d (A.Flip(pr, cost_flip_aug p1, cost_flip_aug p2))

  | A.Send(x,w,p) -> A.Send(x,w, cost_flip_aug p)
  | A.Recv(x,y,p) -> A.Recv(x,y, cost_flip_aug p)

  | A.Close(x) -> A.Close(x)
  | A.Wait(x,p) -> A.Wait(x, cost_flip_aug p)

  | A.Work(pot,p) -> A.Work(pot, cost_flip_aug p)
  | A.Pay(x,pot,p) -> A.Pay(x,pot, cost_flip_aug p)
  | A.Get(x,pot,p) -> A.Get(x,pot, cost_flip_aug p)

  | A.Acquire(x,y,p) -> A.Acquire(x,y,cost_flip_aug p)
  | A.Accept(x,y,p) -> A.Accept(x,y,cost_flip_aug p)
  | A.Release(x,y,p) -> A.Release(x,y,cost_flip_aug p)
  | A.Detach(x,y,p) -> A.Detach(x,y,cost_flip_aug p)

  | A.RecvF(x,y,p) -> A.RecvF(x,y,cost_flip_aug p)
  | A.SendF(x,e,p) -> work d (A.SendF(x, cost_tick_aug e, cost_flip_aug p))

  | A.Let(x,e,p) -> work d (A.Let(x, cost_tick_aug e, cost_flip_aug p))
  | A.IfS(e,p1,p2) -> work d (A.IfS(cost_tick_aug e, cost_flip_aug p1, cost_flip_aug p2))

  | A.MakeChan(x,a,n,p) -> work d (A.MakeChan(x, a, n, cost_flip_aug p))
  | A.Abort -> A.Abort
  (* TODO: add ticks to argument list of spawn and print *)
  | A.Print(l,arg,p) -> A.Print(l, arg, cost_flip_aug p)

and cost_flip_aug {A.st_data = d; A.st_structure = p} = {A.st_data = d; A.st_structure = cost_flip d p}

and cost_flip_branches bs = match bs with
    [] -> []
  | (l,p)::branches ->
      (l, cost_flip_aug p)::(cost_flip_branches branches);;

let apply_cost {A.func_data = d; A.func_structure = fexp} =
  {A.func_data = d; A.func_structure = cost_tick fexp};;
