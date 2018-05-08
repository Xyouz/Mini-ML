open Ast

let rec subs exp x rep = match exp with
  | _,Unit -> exp
  | l,Var(y) when x=y -> rep
  | _,Var(_)| _,Int(_) |_,Bool(_) -> exp
  | l,App(u,t) -> l,App((subs u x rep), (subs t x rep))
  | l,Lam(y,t,e) -> (
      if (x = y) then
        l,Lam(y,t,e)
      else
        l,Lam(y,t,(subs e x rep))
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
        l,LetIn(y,e1,e2)
      else
        let e1p = subs e1 x rep and
        e2p = subs e2 x rep in
        l,LetIn(y,e1p,e2p)
    )

let isValue exp = match exp with
  | Lam(_,_,_) | Pair(_,_) | Int(_) | Bool(_) | Unit -> true
  | _ -> false

let rec eval_expr expr context = match expr with
  | _,Unit -> Unit
  | _, Var(v) -> List.assoc v context
  | l, App((ll,Lam(x,_,t)),(lll,u)) -> (
      if (isValue u) then
         eval_expr (subs t x (lll,u)) context
      else
        let up = eval_expr (lll,u) context in
        eval_expr (l,App((ll,Lam(x,None,t)), (lll,up))) context
    )
  | l,App((ll,t),u) -> let tp = eval_expr (ll,t) context in
    eval_expr (l,App((ll,tp),u)) context
  | _,Int(n) -> Int(n)
  | _,Bool(b) -> Bool(b)
  | l,LetIn(x,(ll,t),u) -> (
      if (isValue t) then
        eval_expr (subs u x (ll,t)) context
      else
        let tp = eval_expr (ll,t) context in
        eval_expr (l,LetIn(x,(ll,tp),u)) context
    )
  | _, Ite((_,Bool(b)),t,u) -> (
      if b then
        eval_expr t context
      else
        eval_expr u context
    )
  | l, Ite((ll,c),t,u) ->
    let cp = eval_expr (ll,c) context in
    eval_expr (l,Ite((ll,cp),t,u)) context
  | _,Binop(op,e1,e2) -> eval_binop op e1 e2 context
  | l,Fix((ll,Lam(x,top,t))) -> (
      eval_expr (subs t x (l,(Fix (ll,Lam(x,top,t))))) context
    )
  | l,Fix((ll,t)) -> let tp = eval_expr (ll,t) context in
    eval_expr (l,Fix((ll,tp))) context
  | _,Lam(x,t,e) -> Lam(x,t,e)
  | _,Pair(e1,e2) -> Pair(e1,e2)
  | _,Proj(Left(_,Pair(e,_))) | _,Proj(Right(_,Pair(_,e))) -> (
      eval_expr e context
    )
  | _,Proj(_) -> failwith "Type error at eval"
and eval_binop op e1 e2 context =
  let e1p = eval_expr e1 context
  and e2p = eval_expr e2 context in
  match e1p,e2p with
  | Int(n1),Int(n2) -> (
      match op with
      | Plus  -> Int(n1 + n2)
      | Minus -> Int(n1 - n2)
      | Times -> Int(n1 * n2)
      | Div   -> Int(n1 / n2)
      | Gt    -> Bool(n1 < n2)
      | Eq    -> Bool(n1 = n2)
      | _     -> failwith "Type error at eval"
    )
  | Bool(b1),Bool(b2) -> (
      match op with
      | And -> Bool(b1 && b2)
      | Or  -> Bool(b1 || b2)
      | Eq  -> Bool(b1 = b2)
      | _ -> failwith "Type error at eval"
    )
  | _ -> (
      match op with
      | Eq -> Bool(e1p = e2p)
      | _ -> failwith "Type error at eval"
    )

let eval_cmd context cmd = match cmd with
  | _,Let(x,_,e) -> let res = eval_expr e context
    in (x,res)::context
  | l,LetRec(x,t,e) -> let res = eval_expr (l,Fix(l,Lam(x,t,e))) context
    in (x,res)::context

let eval_ast cmds = List.rev (List.fold_left eval_cmd [] cmds)
