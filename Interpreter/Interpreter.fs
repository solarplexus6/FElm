module Interpreter

open System
open AbstractSyntax
open Gensym

type context = EmptyContext
             | App1 of context * expr
             | Op1 of context * binop * expr
             | Op2 of expr * binop * context
             | If1 of context * expr * expr
             | Let1 of varname * context * expr
             | Let2 of varname * expr * context
             | Lift1 of context * expr list
             | Liftk of expr * expr list * context * expr list
             | Foldp1 of context * expr * expr
             | Foldp2 of expr * context * expr
             | Foldp3 of expr * expr * context

let binopToFunction =
  let convertOperator op =
    fun x y -> (op x y : bool) |> Convert.ToInt32
  in function
  | Add -> (+)
  | Sub -> (-)
  | Mul -> (*)
  | Div -> (/)
  | Lt  -> convertOperator (<)
  | Le  -> convertOperator (<=)
  | Eq  -> convertOperator (=)
  | Neq -> convertOperator (<>)
  | Ge  -> convertOperator (>=)
  | Gt  -> convertOperator (>)

let rec subst x e =
  let rec subst' expr =
    match expr with
      | Var y when y = x -> e
      | Var y when y <> x -> expr
      | Fun (y, _) when y = x -> expr
      | Fun (y, body) when y <> x ->
        let freshVar = Gensym.next()
        let body' = subst y (Var freshVar) body
        let body'' = subst' body'
        in Fun (freshVar, body'')
      | App (e1, e2) -> App (subst' e1, subst' e2)
      | Op (e1, op, e2) -> Op (subst' e1, op, subst' e2)
      | If (e1, e2, e3) -> If (subst' e1, subst' e2, subst' e3)
      | Let (y, e1, e2) when y = x -> Let (y, subst' e1, e2)
      | Let (y, e1, e2) when y <> x ->
        let freshVar = Gensym.next()
        let e2' = subst y (Var freshVar) e2
        let e2'' = subst' e2'
        in Let (freshVar, subst' e1, e2'')
      | Lift (e1, elist) -> Lift (subst' e1, List.map subst' elist)
      | Foldp (e1, e2, e3) -> Foldp (subst' e1, subst' e2, subst' e3)
      | _ -> expr
  in subst'
