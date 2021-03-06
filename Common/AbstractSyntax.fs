namespace AbstractSyntax

type varname = string
type num = int

type binop = Add
           | Sub
           | Mul
           | Div
           | Lt
           | Le
           | Eq
           | Neq
           | Ge
           | Gt

type expr = Unit
          | Num of num
          | Var of varname
          | Fun of varname * expr
          | App of expr * expr
          | Op of expr * binop * expr
          | If of expr * expr * expr
          | Let of varname * expr * expr
          | Signal of int
          | Lift of expr * expr list
          | Foldp of expr * expr * expr
