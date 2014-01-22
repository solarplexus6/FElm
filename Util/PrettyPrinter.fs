module PrettyPrinter

open AbstractSyntax

let spaces = 4

let binopToString = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Lt -> "<"
  | Le -> "<="
  | Eq -> "=="
  | Neq -> "\="
  | Ge -> ">="
  | Gt -> ">"

let pretty_print =
  let rec pretty ind e =
    let rec aux = function
      | Unit -> "()"
      | Num n -> string n
      | Var x -> x
      | Fun (x, e) -> "\\" + x + " -> " + aux e
      | App (e1, e2) -> aux e1 + " " + aux e2
      | Op (e1, op, e2) -> aux e1 + " " + binopToString op + " " + aux e2
      | If (e1, e2, e3) -> "if " + aux e1 + " then " + aux e2 + " else " + aux e3
      | Let (x, e1, e2) -> "let " + x + " = " + aux e1 + " in\n" + pretty (ind + 1) e2
      | Input _ -> ""
      | Lift (e1, elist) ->
        let elistStrings = List.map (fun e -> " " + aux e) elist
        "lift" + string (List.length elist) + " " + aux e1 + List.fold (+) "" elistStrings
      | Foldp (e1, e2, e3) -> "foldp " + aux e1 + " " + aux e2 + " " + aux e3
    (Core.String.replicate (ind * spaces) " ") + aux e
  pretty 0
