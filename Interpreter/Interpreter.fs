module Interpreter

open System
open AbstractSyntax

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
