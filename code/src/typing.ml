open Ast
open Utils

type typing_error = int

exception Typing_Error of string

let print_error fmt err = failwith "todo print error"


let rec typing_expr expr context = match expr with
  | _,Var(s) -> (
      (* try {*)
        List.assoc s context
     (* }
      with {
        Not_found -> raise Typing_Error
        }*)
    )
  | _,App(e1, e2) -> (
      let t1 = typing_expr e1 context
      and t2 = typing_expr e2 context in
      match t1 with
      | TyArrow(a,b) when (t2 = a) -> b
      | _ -> raise (Typing_Error "app")
    )
  | _,Lam(v, Some(ty), e) -> (
      let b = typing_expr e ((v,ty)::context) in
      TyArrow(ty,b)
    )
  | l,Lam(v, None, e) -> typing_expr (l,Lam(v,Some(TyInt),e)) context
  | _,Pair(e1,e2) -> TyTimes((typing_expr e1 context),(typing_expr e2 context))
  | _,LetIn(v,e1,e2) -> (
      let t1 = typing_expr e1 context in
      typing_expr e2 ((v,t1)::context)
    )
  | _,Fix(e) -> (
      match typing_expr e context with
      | TyArrow(a,b) when a=b -> a
      | _ -> raise (Typing_Error "fix")
    )
  | _,Int(_) -> TyInt
  | _,Bool(_) -> TyBool
  | _,Proj(Left(e)) | _,Proj(Right(e)) -> typing_expr e context
  | _,Ite(c,e1,e2) -> (
      match typing_expr c context with
      | TyBool -> (
          let t1 = typing_expr e1 context
          and t2 = typing_expr e2 context in
          if t1 = t2 then t1 else (raise (Typing_Error "thenelse"))
        )
      | _ -> raise (Typing_Error "if") 
    )
  | l,Binop(op, e1, e2) -> typing_binop op e1 e2 context l
and typing_binop op e1 e2 context l=
  let t1 = typing_expr e1 context
  and t2 = typing_expr e2 context in
  match op with
  | Plus | Minus | Times | Div -> if (t1=TyInt && t2=TyInt) then TyInt else (raise (Typing_Error "op Int"))
  | Gt -> if (t1=TyInt && t2=TyInt) then TyBool else (raise (Typing_Error "op <"))
  | Eq -> if t1=t2 then TyBool else (
      match t2 with
      |TyVar(s) -> raise (Typing_Error s)
      |TyInt -> raise (Typing_Error "int")
      |TyTimes(a,b)-> raise(Typing_Error "times")
      |TyBool -> raise (Typing_Error "bool")
      |TyArrow(a,b) -> raise(Typing_Error "arrow")
    )
  | And | Or -> if (t1=TyBool && t2=TyBool) then TyBool else (raise (Typing_Error "op Bool"))

let rec typing_cmd c context = match c with
  | l,Let(v,_,e) -> (
      let t = typing_expr e context in
      ((l,Let(v,Some(t),e)),(v,t)::context)
    )
  | l,LetRec(v,Some(t),e) -> (
      let tt = typing_expr e ((v,t)::context) in
      if (tt = t) then
        ((l,LetRec(v,Some(t),e)),(v,t)::context)
      else
        raise (Typing_Error "let rec")
    )
  | l,LetRec(v,None,e) -> typing_cmd (l,LetRec(v,Some(TyInt),e)) context

let rec typing_ast ast =
  let astCont = List.fold_left
      (fun (a,conte) c -> let (ct,cont)= typing_cmd c conte in ((ct::a),cont))  ([],[]) ast
  in Ok(fst astCont)
