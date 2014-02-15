module Signal

open AbstractSyntax
open Graph
open Helpers
open Functional

type VertexType = LiftV | FoldpV | InputV

type SigVertex = 
    VertexType * 
    expr (*function *) * 
    expr (*default/last value *)

type Edge = 
    | Change of expr
    | NoChange of expr

let lastValue (((_, _, (_, _, v)), _) : Vertex<SigVertex, _>) : expr = v

// todo: przypadki gdy na koncu leta mamy wartosc, nie sygnal
let rec buildGraph' (label : string) (g : Graph<SigVertex, Edge>) = function
    | Var v -> (getVertexByLabel v g, g)
    | Lift (e, slist) ->
        let (deps, g1) = List.fold (fun (vs, g') s -> let (v', newG) = buildGraph' null g' s in (v' :: vs, newG)) ([], g) slist
        let depsDefaults = List.map lastValue deps
        let defaultV = List.fold (fun f a -> App (f,a)) e depsDefaults |> normalize
        let (v, g2) = Graph.addVertex (LiftV, e, defaultV) label g1
        (v, List.fold (fun g' v' -> snd <| Graph.addEdge (vertexId v', vertexId v) (NoChange <| lastValue v') g') g2 deps)
    | Foldp (e, d, s) ->
        let (v', g1) = buildGraph' null g s
        let (v, g2) = Graph.addVertex (FoldpV, e, d) label g1
        (v, snd <| Graph.addEdge (vertexId v', vertexId v) (NoChange <| lastValue v') g2)
    | Let (l, s, r) -> 
        let (_, g') = buildGraph' l g s
        buildGraph' label g' r
    | _ -> undefined

let baseGraph : Graph<SigVertex, Edge> =
    Graph.addVertex (InputV, Unit, Num 0) "Window.height" Graph.empty |> snd |>
    Graph.addVertex (InputV, Unit, Num 0) "Window.width" |> snd
    
let buildGraph = buildGraph' "main" baseGraph