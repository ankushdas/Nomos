module R = Arith
module A = Ast
module F = RastFlags

let rec cost_recv f exp = match exp with
    A.Fwd(x,y) -> A.Fwd(x,y)
  | A.Spawn(x,g,xs,q) -> A.Spawn(x,g,xs, cost_recv f q)
  | A.ExpName(x,f,xs) -> A.ExpName(x,f,xs)

  | A.Lab(x,k,p) -> A.Lab(x,k, cost_recv f p)
  | A.Case(x,branches) -> f (A.Case(x, cost_recv_branches f branches))

  | A.Send(x,w,p) -> A.Send(x,w,cost_recv f p)
  | A.Recv(x,y,p) -> f (A.Recv(x,y, cost_recv f p))

  | A.Close(x) -> A.Close(x)
  | A.Wait(x,p) -> f (A.Wait(x, cost_recv f p))

  | A.Work(pot,p) -> A.Work(pot,cost_recv f p)
  | A.Pay(x,pot,p) -> A.Pay(x,pot,cost_recv f p)
  | A.Get(x,pot,p) -> A.Get(x,pot,cost_recv f p)

  | A.Acquire(x,y,p) -> A.Acquire(x,y,cost_recv f p)
  | A.Accept(x,y,p) -> A.Accept(x,y,cost_recv f p)
  | A.Release(x,y,p) -> A.Release(x,y,cost_recv f p)
  | A.Detach(x,y,p) -> A.Detach(x,y,cost_recv f p)

  | A.Marked(marked_P) ->
      A.Marked(Mark.mark'(cost_recv f (Mark.data marked_P), Mark.ext marked_P))

and cost_recv_branches f bs = match bs with
    [] -> []
  | {lab_exp = (l,p); exp_extent = ext}::branches ->
      {lab_exp = (l, cost_recv f p); exp_extent = ext}::cost_recv_branches f branches;;

let rec cost_send f exp = match exp with
    A.Fwd(x,y) -> A.Fwd(x,y)
  | A.Spawn(x,g,xs,p) -> A.Spawn(x,g,xs, cost_send f p)
  | A.ExpName(x,f,xs) -> A.ExpName(x,f,xs)

  | A.Lab(x,k,p) -> f (A.Lab(x,k, cost_send f p))
  | A.Case(x,branches) -> A.Case(x, cost_send_branches f branches)

  | A.Send(x,w,p) -> f (A.Send(x,w, cost_send f p))
  | A.Recv(x,y,p) -> A.Recv(x,y, cost_send f p)

  | A.Close(x) -> f (A.Close(x)) (* no continuation here to delay *)
  | A.Wait(x,p) -> A.Wait(x, cost_send f p)


  | A.Work(pot,p) -> A.Work(pot, cost_send f p)   (* allow in source *)
  | A.Pay(x,pot,p) -> A.Pay(x,pot, cost_send f p)
  | A.Get(x,pot,p) -> A.Get(x,pot, cost_send f p)

  | A.Acquire(x,y,p) -> A.Acquire(x,y,cost_send f p)
  | A.Accept(x,y,p) -> A.Accept(x,y,cost_send f p)
  | A.Release(x,y,p) -> A.Release(x,y,cost_send f p)
  | A.Detach(x,y,p) -> A.Detach(x,y,cost_send f p)


  | A.Marked(marked_P) ->
      A.Marked(Mark.mark'(cost_send f (Mark.data marked_P), Mark.ext marked_P))

and cost_send_branches f bs = match bs with
    [] -> []
  | {lab_exp = (l,p); exp_extent = ext}::branches ->
      {lab_exp = (l, cost_send f p); exp_extent = ext}::cost_send_branches f branches;;

let cost_model f flag exp = match flag with
    F.None -> exp
  | F.Free -> exp
  | F.Recv -> cost_recv f exp
  | F.RecvSend -> cost_send f (cost_recv f exp)
  | F.Send -> cost_send f exp;;

let apply_cost_work exp = cost_model (fun k -> A.Work(A.Arith (R.Int 1),k)) (!F.work) exp;;

(* structure Cost *)
