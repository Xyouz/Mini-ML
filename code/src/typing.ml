open Ast

type typing_error = int

exception Typing_Error

let print_error fmt err = failwith "todo print error"


let rec typing_expr expr context = match expr with
  | _,Var(s) -> failwith "todo typing_expr Var"
  | _,App(e1, e2) -> (
      let t1 = typing_expr e1 context
      and t2 = typing_expr e2 context in
      match t1 with
      | TyArrow(a,b) when (t2 = a) -> b
      | _ -> raise Typing_Error
    )
  | _,Lam(_,_,_) -> failwith "todo typing_expr Lam"
  | _,Pair(e1,e2) -> TyTimes((typing_expr e1 context),(typing_expr e2 context))
  | _,LetIn(_,_,_) -> failwith "todo typing_expr LetIn"
  | _,Fix(e) -> (
      match typing_expr e context with
      | TyArrow(a,b) when a=b -> a
      | _ -> raise Typing_Error
    )
  | _,Int(_) -> TyInt
  | _,Bool(_) -> TyBool
  | _,Proj(Left(e)) | _,Proj(Right(e)) -> typing_expr e context
  | _,Ite(c,e1,e2) -> (
      match typing_expr c context with
      | TyBool -> (
          let t1 = typing_expr e1 context
          and t2 = typing_expr e2 context in
          if t1 = t2 then t1 else (raise Typing_Error)
        )
      | _ -> raise Typing_Error 
    )
  | _,Binop(op, e1, e2) -> typing_binop op e1 e2 context
and typing_binop op e1 e2 context =
  let t1 = typing_expr e1 context
  and t2 = typing_expr e2 context in
  match op with
  | Plus | Minus | Times | Div -> if (t1=t2 && t1=TyInt) then t1 else (raise Typing_Error)
  | Gt -> if (t1=t2 && t1=TyInt) then TyBool else (raise Typing_Error)
  | Eq -> if t1=t2 then TyBool else (raise Typing_Error)
  | And | Or -> if (t1=t2 && t1=TyBool) then TyBool else (raise Typing_Error)

let rec typing_ast ast = failwith "todo typing ast"
