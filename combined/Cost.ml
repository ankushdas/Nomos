module R = Arith
module A = Ast

let rec cost_recv f exp = match exp with
    A.Fwd(x,y) -> A.Fwd(x,y)
  | A.Spawn(x,g,xs,q) -> A.Spawn(x,g,xs, cost_recv_aug f q)
  | A.ExpName(x,f,xs) -> A.ExpName(x,f,xs)

  | A.Lab(x,k,p) -> A.Lab(x,k, cost_recv_aug f p)
  | A.Case(x,branches) -> f (A.Case(x, cost_recv_branches f branches))

  | A.Send(x,w,p) -> A.Send(x,w,cost_recv_aug f p)
  | A.Recv(x,y,p) -> f (A.Recv(x,y, cost_recv_aug f p))

  | A.Close(x) -> A.Close(x)
  | A.Wait(x,p) -> f (A.Wait(x, cost_recv_aug f p))

  | A.Work(pot,p) -> A.Work(pot,cost_recv_aug f p)
  | A.Pay(x,pot,p) -> A.Pay(x,pot,cost_recv_aug f p)
  | A.Get(x,pot,p) -> A.Get(x,pot,cost_recv_aug f p)

  | A.Acquire(x,y,p) -> A.Acquire(x,y,cost_recv_aug f p)
  | A.Accept(x,y,p) -> A.Accept(x,y,cost_recv_aug f p)
  | A.Release(x,y,p) -> A.Release(x,y,cost_recv_aug f p)
  | A.Detach(x,y,p) -> A.Detach(x,y,cost_recv_aug f p)

  | A.RecvF(x,y,p) -> f (A.RecvF(x,y,cost_recv_aug f p))
  | A.SendF(x,e,p) -> A.SendF(x, cost_tick_aug e, cost_recv_aug f p)

  | A.Let(x,e,p) -> A.Let(x, cost_tick_aug e, cost_recv_aug f p)
  | A.IfS(e,p1,p2) -> A.IfS(cost_tick_aug e, cost_recv_aug f p1, cost_recv_aug f p2)

and cost_recv_aug f {A.st_data = d; A.st_structure = p} = {A.st_data = d; A.st_structure = cost_recv f p}

and cost_recv_branches f bs = match bs with
    [] -> []
  | (l,p)::branches ->
      (l, cost_recv_aug f p)::(cost_recv_branches f branches)

and cost_tick_aug {A.func_data = d; A.func_structure = e} = {A.func_data = d; A.func_structure = cost_tick e}

and cost_tick fexp = match fexp with
    A.If(e1,e2,e3) -> tick (A.If(cost_tick_aug e1, cost_tick_aug e2, cost_tick_aug e3))
  | A.LetIn(x,e1,e2) -> tick (A.LetIn(x, cost_tick_aug e1, cost_tick_aug e2))
  | A.Bool _ | A.Int _ | A.Var _ -> tick (fexp)
  | A.ListE(l) -> tick (A.ListE(List.map cost_tick_aug l))
  | A.App(es) -> tick (A.App(List.map cost_tick_aug es))
  | A.Cons(e1,e2) -> tick (A.Cons(cost_tick_aug e1, cost_tick_aug e2))
  | A.Match(e1,e2,x,xs,e3) -> tick (A.Match(cost_tick_aug e1, cost_tick_aug e2, x, xs, cost_tick_aug e3))
  | A.Lambda(xs,e) -> tick (A.Lambda(xs, cost_tick_aug e))
  | A.Op(e1,op,e2) -> tick (A.Op(cost_tick_aug e1, op, cost_tick_aug e2))
  | A.CompOp(e1,cop,e2) -> tick (A.CompOp(cost_tick_aug e1, cop, cost_tick_aug e2))
  | A.RelOp(e1,rop,e2) -> tick (A.RelOp(cost_tick_aug e1, rop, cost_tick_aug e2))
  | A.Tick(pot,e) -> A.Tick(pot, cost_tick_aug e)
  | A.Command(p) -> tick (A.Command(cost_tick_aug p));;

let rec cost_send f exp = match exp with
    A.Fwd(x,y) -> A.Fwd(x,y)
  | A.Spawn(x,g,xs,p) -> A.Spawn(x,g,xs, cost_send_aug f p)
  | A.ExpName(x,f,xs) -> A.ExpName(x,f,xs)

  | A.Lab(x,k,p) -> f (A.Lab(x,k, cost_send_aug f p))
  | A.Case(x,branches) -> A.Case(x, cost_send_branches f branches)

  | A.Send(x,w,p) -> f (A.Send(x,w, cost_send_aug f p))
  | A.Recv(x,y,p) -> A.Recv(x,y, cost_send_aug f p)

  | A.Close(x) -> f (A.Close(x)) (* no continuation here to delay *)
  | A.Wait(x,p) -> A.Wait(x, cost_send_aug f p)


  | A.Work(pot,p) -> A.Work(pot, cost_send_aug f p)   (* allow in source *)
  | A.Pay(x,pot,p) -> A.Pay(x,pot, cost_send_aug f p)
  | A.Get(x,pot,p) -> A.Get(x,pot, cost_send_aug f p)

  | A.Acquire(x,y,p) -> A.Acquire(x,y,cost_send_aug f p)
  | A.Accept(x,y,p) -> A.Accept(x,y,cost_send_aug f p)
  | A.Release(x,y,p) -> A.Release(x,y,cost_send_aug f p)
  | A.Detach(x,y,p) -> A.Detach(x,y,cost_send_aug f p)

  | A.RecvF(x,y,p) -> A.RecvF(x,y,cost_recv_aug f p)
  | A.SendF(x,e,p) -> f (A.SendF(x, cost_tick_aug e, cost_recv_aug f p))

  | A.Let(x,e,p) -> A.Let(x, cost_tick_aug e, cost_recv_aug f p)
  | A.IfS(e,p1,p2) -> A.IfS(cost_tick_aug e, cost_recv_aug f p1, cost_recv_aug f p2)

and cost_send_aug f {A.st_data = d; A.st_structure = p} = {A.st_data = d; A.st_structure = cost_send f p}

and cost_send_branches f bs = match bs with
    [] -> []
  | (l,p)::branches ->
      (l, cost_send_aug f p)::(cost_send_branches f branches);;

let cost_model ff fst flag exp = match flag with
    Flags.None -> exp
  | Flags.Free -> exp
  | Flags.Recv -> cost_recv f exp
  | Flags.RecvSend -> cost_send f (cost_recv f exp)
  | Flags.Send -> cost_send f exp;;

let apply_cost_work exp = cost_model (fun k -> A.Work(A.Arith (R.Int 1),k)) (!Flags.work) exp;;

let apply_cost_fwork fexp = cost_model (fun k -> A.Tick(A.Arith (R.Int 1),k)) (!Flags.work) fexp;;

(* structure Cost *)
