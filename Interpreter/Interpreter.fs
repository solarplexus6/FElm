module Interpreter

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
      Let (x', l1, f l2')
    | None -> reduce' e
and reduce' = function
  | App (Fun (x, e1), e2) -> Let (x, e1, e2)                  // APPLICATION
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

// poprzednia (niekompletna) wersja
// let rec reduce e =
//   match e with
//     | App (Fun (x, e1), e2) -> Let (x, e1, e2) // APPLICATION
//     | App (Let (x, l1, l2), e2) when isSignalTerm l1 && isFinalTerm l2 -> // EXPAND
//       let (x', l2') = alphaConvert (x, l2)
//       Let (x', l1, App (l2', e2))
//     | App (e1, e2) ->
//       let r = reduce e1
//       in App (r, e2)
//     | Op (Num n1, op, Num n2) -> Num (binopToFunction op n1 n2) // OP
//     | Op (Num n1, op, Let (x, l1, l2)) when isSignalTerm l1 && isFinalTerm l2 -> // EXPAND
//       let (x', l2') = alphaConvert (x, l2)
//       Let (x', l1, Op (Num n1, op, l2'))
//     | Op (Num n1, op, e2) ->
//       let r = reduce e2
//       in Op (Num n1, op, r)
//     | Op (Let (x, l1, l2), op, e2) when isSignalTerm l1 && isFinalTerm l2 -> // EXPAND
//       let (x', l2') = alphaConvert (x, l2)
//       Let (x', l1, Op (l2', op, e2))
//     | Op (e1, op, e2) ->
//       let r = reduce e1
//       in Op (r, op, e2)
//     | If (Num 0, e2, e3) -> e3          // COND-FALSE
//     | If (Num _, e2, e3) -> e2          // COND-TRUE
//     | If (e1, e2, e3) ->                // co z EXPAND?
//       let r = reduce e1
//       in If (r, e2, e3)
//     | Let (x, e1, e2) when isValue e1 -> subst x e1 e2 // REDUCE
//     | Let (x, e1, e2) when isSignalTerm e1 ->
//       let r = reduce e2
//       in Let (x, e1, r)
//     | Let (x, e1, e2) ->
//       let r = reduce e1
//       in Let (x, r, e2)
//     | Lift (e1, elist) when isValue e1 ->
//       let rec splitLift = function
//         | (e :: es) when isSignalTerm e ->
//           let (elist1, ek, elist2) = splitLift es
//           in ((e :: elist1), ek, elist2)
//         | (e :: es) -> ([], e, es)
//         | [] -> failwith "splitLift"
//       let (elist1, ek, elist2) = splitLift elist
//       let r = reduce ek
//       in Lift (e1, elist1 @ [r] @ elist2)
//     | Lift (Let (x, l1, l2), elist) when isSignalTerm l1 && isFinalTerm l2 -> // EXPAND
//       let (x', l2') = alphaConvert (x, l2)
//       Let (x', l1, Lift (l2', elist))
//     | Lift (e1, elist) ->
//       let r = reduce e1
//       in Lift (r, elist)
//     | Foldp (e1, e2, e3) when isValue e1 && isValue e2 ->
//       let r = reduce e3
//       in Foldp (e1, e2, r)
//     | Foldp (e1, Let (x, l1, l2), e3) when isValue e1 && isSignalTerm l1 && isFinalTerm l2 -> // EXPAND
//       let (x', l2') = alphaConvert (x, l2)
//       Let (x', l1, Foldp (e1, l2', e3))
//     | Foldp (e1, e2, e3) when isValue e1 ->
//       let r = reduce e2
//       in Foldp (e1, r, e3)
//     | Foldp (Let (x, l1, l2), e2, e3) when isSignalTerm l1 && isFinalTerm l2 -> // EXPAND
//       let (x', l2') = alphaConvert (x, l2)
//       Let (x', l1, Foldp (l2', e2, e3))
//     | Foldp (e1, e2, e3) ->
//       let r = reduce e1
//       in Foldp (r, e2, e3)
//     | _ -> failwith "reduce couldn't find a valid redex"
    

// poprzednia (jeszcze bardziej niekompletna) wersja

// type context = EmptyC
//              | App1 of context * expr
//              | Op1 of context * binop * expr
//              | Op2 of expr * binop * context
//              | If1 of context * expr * expr
//              | Let1 of varname * context * expr
//              | Let2 of varname * expr * context
//              | Lift1 of context * expr list
//              | Liftk of expr * expr list * context * expr list
//              | Foldp1 of context * expr * expr
//              | Foldp2 of expr * context * expr
//              | Foldp3 of expr * expr * context

// let rec plug e = function
//   | EmptyC -> e
//   | App1 (c, e2) -> App (plug e c, e2)
//   | Op1 (c, op, e2) -> Op (plug e c, op, e2)
//   | Op2 (e1, op, c) -> Op (e1, op, plug e c)
//   | If1 (c, e2, e3) -> If (plug e c, e2, e3)
//   | Let1 (x, c, e2) -> Let (x, plug e c, e2)
//   | Let2 (x, e1, c) -> Let (x, e1, plug e c)
//   | Lift1 (c, elist) -> Lift (plug e c, elist)
//   | Liftk (e1, elist1, c, elist2) -> Lift (e1, elist1 @ [plug e c] @ elist2)
//   | Foldp1 (c, e2, e3) -> Foldp (plug e c, e2, e3)
//   | Foldp2 (e1, c, e3) -> Foldp (e1, plug e c, e3)
//   | Foldp3 (e1, e2, c) -> Foldp (e1, e2, plug e c)           

// // wszystko poza EXPAND
// let contract1 = function
//   | Op (Num n1, op, Num n2) -> Num (binopToFunction op n1 n2)
//   | If (Num 0, e2, e3) -> e3
//   | If (v, e2, e3) -> e2
//   | App (Fun (x, e1), e2) -> Let (x, e1, e2)
//   | Let (x, v, e) -> subst x v e
//   | _ -> failwith "contract1 was given an invalid redex"

// // tylko EXPAND
// let contract2 f = function
//   | Let (x, s, u) ->
//     let (x', u') = alphaConvert (x, u)
//     in Let (x', s, plug u' f)
//   | _ -> failwith "contract2 was given an invalid redex"

// let rec decompose e =
//   match e with
//     | App (e1, e2) when isValue e1 -> (e, EmptyC)
//     | App (e1, e2) ->
//       let (r, c) = decompose e1
//       in (r, App1 (c, e2))        
//     | Op (e1, op, e2) when isValue e1 && isValue e2 -> (e, EmptyC)
//     | Op (e1, op, e2) when isValue e1 ->
//       let (r, c) = decompose e2
//       in (r, Op2 (e1, op, c))
//     | Op (e1, op, e2) ->
//       let (r, c) = decompose e1
//       in (r, Op1 (c, op, e2))
//     | If (e1, e2, e3) when isValue e1 -> (e, EmptyC)      
//     | If (e1, e2, e3) ->
//       let (r, c) = decompose e1
//       in (r, If1 (c, e2, e3))
//     | Let (x, e1, e2) when isValue e1 -> (e, EmptyC)
//     | Let (x, e1, e2) when isSignalTerm e1 ->
//       let (r, c) = decompose e2
//       in (r, Let2 (x, e1, c))
//     | Let (x, e1, e2) ->
//       let (r, c) = decompose e1
//       in (r, Let1 (x, c, e2))
//     | Lift (e1, elist) when isValue e1 ->
//       let rec splitLift = function
//         | (e :: es) when isSignalTerm e ->
//           let (elist1, ek, elist2) = splitLift es
//           in ((e :: elist1), ek, elist2)
//         | (e :: es) -> ([], e, es)
//         | [] -> failwith "splitLift"
//       let (elist1, ek, elist2) = splitLift elist
//       let (r, c) = decompose ek
//       in (r, Liftk (e1, elist1, c, elist2))
//     | Lift (e1, elist) ->
//       let (r, c) = decompose e1
//       in (r, Lift1 (c, elist))
//     | Foldp (e1, e2, e3) when isValue e1 && isValue e2 ->
//       let (r, c) = decompose e3
//       in (r, Foldp3 (e1, e2, c))
//     | Foldp (e1, e2, e3) when isValue e1 ->
//       let (r, c) = decompose e2
//       in (r, Foldp2 (e1, c, e3))
//     | Foldp (e1, e2, e3) ->
//       let (r, c) = decompose e1
//       in (r, Foldp1 (c, e2, e3))
//     | _ -> failwith "decompose couldn't find a redex"

    
