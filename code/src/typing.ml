open Ast
open Utils
open Genlab

type typing_error = int

exception Typing_Error of string

let print_error fmt err = failwith "todo print error"

let rec constraints_expr expr context constraints = match expr with
  | _,Unit -> (TyUnit,constraints,expr)
  | _,Var(s) -> (
      try (
        let t = List.assoc s context in
        (t,constraints,expr)
      )
      with
        Not_found -> raise (Typing_Error ("Unbound variable "^ s))
    )
  | l,App(e1, e2) -> (
      let t1,c1,e1' = constraints_expr e1 context constraints in
      let t2,c2,e2' = constraints_expr e2 context c1 in
      let lb = TyVar(Genlab.label ()) in
      (lb, ((TyArrow(t2,lb),t1)::c2), (l,App(e1',e2')))
    )
  | l,Lam(v, Some(ty), e) -> (
      let t,c,e' = constraints_expr e ((v,ty)::context) constraints in
      (TyArrow(ty,t), c, (l,Lam(v,Some(ty),e')))
    )
  | l,Lam(v, None, e) -> (
      Genlab.next_letter ();
      constraints_expr (l,Lam(v,Some(TyVar(Genlab.label ())),e)) context constraints
    )
  | l,Pair(e1,e2) -> let t1,c1,e1' = constraints_expr e1 context constraints
    in let t2,c2,e2' = constraints_expr e2 context c1
    in (TyTimes(t1,t2),c2,(l,Pair(e1',e2')))
  | l,LetIn(v,e1,e2) -> (
      let t1,c1,e1' = constraints_expr e1 context constraints in
      let t2,c2,e2' = constraints_expr e2 ((v,t1)::context) constraints in
      (t2,c2, (l,LetIn(v,e1',e2')))
    )
  | l,Fix(e) -> (
      let t,c,e' =  constraints_expr e context constraints in
      let var = (TyVar (Genlab.next_letter(); Genlab.label())) in
      (var, (t,TyArrow(var,var))::c, (l,Fix(e')))
    )
  | _,Int(_) -> TyInt, constraints, expr
  | _,Bool(_) -> TyBool, constraints, expr
  | l,Proj(Right(e)) -> (
      let t,c,e' = constraints_expr e context constraints in
      (t, c, (l,Proj(Right(e'))))
    )
  | l,Proj(Left(e)) -> (
      let t,c,e' = constraints_expr e context constraints in
      (t, c, (l,Proj(Left(e'))))
    )
  | l,Ite(c,e1,e2) -> (
      let tc,cc,c' = constraints_expr c context constraints in
      let t1,c1,e1' = constraints_expr e1 context cc in
      let t2,c2,e2' = constraints_expr e2 context c1 in
      (t1, (t1,t2)::(tc,TyBool)::c2, (l,Ite(c',e1',e2')))
    )
  | l,Binop(op, e1, e2) -> typing_binop op e1 e2 context l constraints
and typing_binop op e1 e2 context l constraints =
  let t1, c1, e1' = constraints_expr e1 context constraints in
  let t2, c2, e2' = constraints_expr e2 context c1 in
  match op with
  | Plus | Minus | Times | Div -> (
      (TyInt, (t1,TyInt)::(t2,TyInt)::c2, (l,Binop(op,e1',e2')))
    )
  | Gt -> (
      (TyBool, (t1,TyInt)::(t2,TyInt)::c2, (l,Binop(op,e1',e2')))
    )
  | Eq -> (
      (TyBool, (t1,t2)::c2, (l,Binop(op,e1',e2')))
    )
  | And | Or -> (
      (TyBool, (t1,TyBool)::(t2,TyBool)::c2, (l,Binop(op,e1',e2')))
    )


let rec occur_check x t = match t with
  | TyUnit | TyBool | TyInt -> false
  | TyVar(v) -> (x = v)
  | TyArrow(t1,t2) | TyTimes(t1,t2) -> (
      (occur_check x t1)||(occur_check x t2)
    )

let rec subs x s typ = match typ with
  | TyVar(v) when v=x -> s
  | TyInt | TyBool | TyUnit | TyVar(_)-> typ
  | TyArrow(u,t) -> TyArrow((subs x s u),(subs x s t))
  | TyTimes(u,t) -> TyTimes((subs x s u),(subs x s t))

let replace v t l = List.map (fun (s,u) -> ((subs v t s),(subs v t u))) l

let replace_right v t l = List.map (fun (s,u) -> (s,(subs v t u))) l



let rec unification l acc = 
  (
  );
match l with
  | [] -> acc
  | (TyVar(v1),TyVar(v2))::q when v1=v2 -> unification q acc
  | (TyInt,TyInt)::q | (TyBool,TyBool)::q | (TyUnit,TyUnit)::q -> unification q acc
  | (TyArrow(t,t'),TyArrow(t1,t1'))::q | (TyTimes(t,t'),TyTimes(t1,t1'))::q ->
    unification ((t,t1)::(t',t1')::q) acc
  | (TyVar(v), t)::q | (t, TyVar(v))::q -> (
      if (occur_check v t) then (
        raise (Typing_Error "Occur_check failed")
      )
      else (
        unification (replace v t q) ((TyVar(v),t)::(replace_right v t acc))
      )
    )
  | (t,v)::q ->(
                 raise (Typing_Error "Unification failed")
                )


let subs_list t cl = let aux tt c = match c with
    | (TyVar(v),tt') -> (subs v tt' tt)
    | _ -> failwith "Unification faite avec les pieds"
  in  List.fold_left aux t cl


let rec typing_cmd c context = match c with
  | l,Let(v,_,e) -> (
      let t,c,e' = constraints_expr e context [] in
      let c' = unification c [] in
      let t' = subs_list t c' in
      ((l,Let(v,Some(t'),e)),(v,t')::context)
    )
 (* | l,LetRec(v,Some(t),e) -> (
      let t',c,e' = constraints_expr e ((v,t)::context) [] in
      let c' = unification ((t,t')::c) [] in
      let tt = subs_list t c' in
      ((l,LetRec(v,Some(tt),e)),(v,tt)::context)
    )*)
  | l,LetRec(v,Some(t),e) -> ((*
      Printer.print_expr Format.std_formatter (snd e);
    Format.fprintf Format.std_formatter "\n";
      Printer.print_expr Format.std_formatter (Lam(v, Some(t), e));
    Format.fprintf Format.std_formatter "\n\n";*)
      let t',c,e' = constraints_expr (l,Lam(v,Some(t),e)) ((v,t)::context) [] in
   (* (List.iter
  (fun (t,t')-> Printer.print_ty Format.std_formatter t;
    Format.fprintf Format.std_formatter "\n";
    Printer.print_ty Format.std_formatter t';
    Format.fprintf  Format.std_formatter "\n\n") (((TyArrow(t,t),t')::c)));*)
let vt = TyVar(Genlab.label ()) in
let c' = unification ((TyArrow(t,t),t')::c) [] in
let tt = subs_list t c' in
      ((l,LetRec(v,Some(tt),e')),(v,tt)::context)
    )
  | l,LetRec(v,None,e) -> (
      Genlab.next_letter ();
      typing_cmd (l,LetRec(v,Some(TyVar(Genlab.label ())),e)) context
    )

let rec typing_ast ast =
  let astCont = List.fold_left
      (fun (a,conte) c -> let (ct,cont)= typing_cmd c conte in ((ct::a),cont))  ([],[]) ast
  in Ok(fst astCont)

