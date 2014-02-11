module Signal

open AbstractSyntax
open Graph
open Helpers

type Signal = 
    | LiftVertex of
        expr (*lift function *) * 
        expr (*last value *)
    | FoldpVertex of
        expr (*fold function *) *
        expr (*last value *)

let buildGraph (g : Graph<Signal, _>) = function
    | Var v -> undefined
    | Lift (e1, elist) -> undefined   
    | Foldp (e1, e2, e3) -> undefined
    | _ -> undefined