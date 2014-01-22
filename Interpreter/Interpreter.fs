module Interpreter

open System
open AbstractSyntax

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
