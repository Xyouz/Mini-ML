open Ast

let rec subs exp x rep = match exp with
  | l,Var(y) when x=y -> rep
  | _,Var(_)| _,Int(_) |_,Bool(_) -> exp
  | l,App(u,t) -> l,App((subs u x rep), (subs t x rep))
  | l,Lam(y,_,e) -> (
      if (x = y) then
        failwith "Bound variable when subs in lambda"
      else
        l,Lam(y,None,(subs e x rep))
    )
  | l,Pair(e1,e2) -> let e1p = subs e1 x rep and
    e2p = subs e2 x rep in
    l,Pair(e1p,e2p)
  | l,Ite(c,e1,e2) -> let cp = subs c x rep and
    e1p = subs e1 x rep and
    e2p = subs e2 x rep in
    l,Ite(cp, e1p, e2p)
  | l,Fix(e) -> l,Fix((subs e x rep))
  | l,Binop(op,e1,e2) -> let e1p = subs e1 x rep and
    e2p = subs e2 x rep in
    l,Binop(op, e1p,e2p)
  | l,Proj(Left(e)) -> l,Proj(Left(subs e x rep))
  | l,Proj(Right(e)) -> l,Proj(Right(subs e x rep))
  | l,LetIn(y,e1,e2) -> (
      if (x = y) then
        failwith "Bound variable when subs in LetIn"
      else
        let e1p = subs e1 x rep and
        e2p = subs e2 x rep in
        l,LetIn(y,e1p,e2p)
    )
 
let isValue exp = match exp with
  | _,Lam(_,_,_) | _,Pair(_,_) | _,Int(_) | _,Bool(_) -> true
  | _ -> false

let rec eval_expr expr context = match expr with
  | _, Var(v) -> List.assoc v context
  | l, App((ll,Lam(x,_,t)),u) -> (
      if (isValue u) then
         eval_expr (subs t x u) context
      else
        let up = eval_expr u context in 
        eval_expr (l,App((ll,Lam(x,None,t)), up)) context 
    )
  | l,App(t,u) -> let tp = eval_expr t context in (l,App(tp,u))
  | _ -> failwith "TODO eval expr"

let eval_ast cmds = failwith "todo eval"
