module Functional

open System
open AbstractSyntax
open Gensym

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
        let (y', body') = alphaConvert (y, body)
        let body'' = subst' body'
        in Fun (y', body'')
      | App (e1, e2) -> App (subst' e1, subst' e2)
      | Op (e1, op, e2) -> Op (subst' e1, op, subst' e2)
      | If (e1, e2, e3) -> If (subst' e1, subst' e2, subst' e3)
      | Let (y, e1, e2) when y = x -> Let (y, subst' e1, e2)
      | Let (y, e1, e2) when y <> x ->
        let (y', e2') = alphaConvert (y, e2)
        let e2'' = subst' e2'
        in Let (y', subst' e1, e2'')
      | Lift (e1, elist) -> Lift (subst' e1, List.map subst' elist)
      | Foldp (e1, e2, e3) -> Foldp (subst' e1, subst' e2, subst' e3)
      | _ -> expr
  in subst'
and alphaConvert (x, e) =
  let x' = Gensym.next()
  let e' = subst x (Var x') e
  in (x', e')  

let isValue = function
  | Unit
  | Num _
  | Fun (_, _) -> true
  | _ -> false

let rec isSignalTerm = function
  | Var _ -> true
  | Let (_, e1, e2) -> isSignalTerm e1 && isFinalTerm e2
  | Input _ -> true
  | Lift (e1, elist) -> isValue e1 && List.forall isSignalTerm elist
  | Foldp (e1, e2, e3) -> isValue e1 && isValue e2 && isSignalTerm e3
  | _ -> false
and isFinalTerm e = isValue e || isSignalTerm e  

// Jeśli zastosowanie reguły EXPAND jest możliwe, to zwraca:
// Some (x, l1, l2, f)
// gdzie x, l1, l2 to podwyrażenia leta
// zaś f to funkcja która wkłada wyrażenie do kontekstu F
// w przeciwnym razie zwraca None
let rec tryExpand = function
  | App (Let (x, l1, l2), e2) when isSignalTerm l1 && isFinalTerm l2 ->
    Some (x, l1, l2, fun e -> App (e, e2))
  | Op (Num n1, op, Let (x, l1, l2)) when isSignalTerm l1 && isFinalTerm l2 ->
    Some (x, l1, l2, fun e -> Op (Num n1, op, e))
  | Op (Let (x, l1, l2), op, e2) when isSignalTerm l1 && isFinalTerm l2 ->
    Some (x, l1, l2, fun e -> Op (e, op, e2))
  | If (e1, e2, e3) ->
    match tryExpand e1 with
      | Some (x, l1, l2, f') -> Some (x, l1, l2, fun e -> If (f' e, e2, e3))
      | None -> None
  | Lift (Let (x, l1, l2), elist) when isSignalTerm l1 && isFinalTerm l2 ->
    Some (x, l1, l2, fun e -> Lift (e, elist))
  | Foldp (e1, Let (x, l1, l2), e3) when isValue e1 && isSignalTerm l1 && isFinalTerm l2 ->
    Some (x, l1, l2, fun e -> Foldp (e1, e, e3))
  | Foldp (Let (x, l1, l2), e2, e3) when isSignalTerm l1 && isFinalTerm l2 ->
    Some (x, l1, l2, fun e -> Foldp (e, e2, e3))
  | _ -> None

let rec reduce e =
  match tryExpand e with
    | Some (x, l1, l2, f) ->
      let (x', l2') = alphaConvert (x, l2)
      in Let (x', l1, f l2')
    | None -> reduce' e
and reduce' = function
  | App (Fun (x, e1), e2) -> Let (x, e2, e1)                  // APPLICATION
  | App (e1, e2) ->
    let r = reduce e1
    in App (r, e2)
  | Op (Num n1, op, Num n2) -> Num (binopToFunction op n1 n2) // OP
  | Op (Num n1, op, e2) ->
    let r = reduce e2
    in Op (Num n1, op, r)
  | Op (e1, op, e2) ->
    let r = reduce e1
    in Op (r, op, e2)
  | If (Num 0, e2, e3) -> e3                                  // COND-FALSE
  | If (Num _, e2, e3) -> e2                                  // COND-TRUE
  | If (e1, e2, e3) ->                
    let r = reduce e1
    in If (r, e2, e3)
  | Let (x, e1, e2) when isValue e1 -> subst x e1 e2          // REDUCE
  | Let (x, e1, e2) when isSignalTerm e1 ->
    let r = reduce e2
    in Let (x, e1, r)
  | Let (x, e1, e2) ->
    let r = reduce e1
    in Let (x, r, e2)
  | Lift (e1, elist) when isValue e1 ->
    let rec splitLift = function
      | (e :: es) when isSignalTerm e ->
        let (elist1, ek, elist2) = splitLift es
        in ((e :: elist1), ek, elist2)
      | (e :: es) -> ([], e, es)
      | [] -> failwith "splitLift"
    let (elist1, ek, elist2) = splitLift elist
    let r = reduce ek
    in Lift (e1, elist1 @ [r] @ elist2)
  | Lift (e1, elist) ->
    let r = reduce e1
    in Lift (r, elist)
  | Foldp (e1, e2, e3) when isValue e1 && isValue e2 ->
    let r = reduce e3
    in Foldp (e1, e2, r)
  | Foldp (e1, e2, e3) when isValue e1 ->
    let r = reduce e2
    in Foldp (e1, r, e3)
  | Foldp (e1, e2, e3) ->
    let r = reduce e1
    in Foldp (r, e2, e3)
  | _ -> failwith "reduce couldn't find a valid redex"    

let rec normalize e =
  if isFinalTerm e then e
  else reduce e |> normalize
