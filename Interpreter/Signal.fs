module Signal

open AbstractSyntax
open Graph
open Helpers
open Functional

type SigDef = LiftV of int list | FoldpV | InputV

type SigVertex = 
    SigDef * 
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
        // deps = dependencies
        let (deps, g1) = List.fold (fun (vs, g') s -> let (v', newG) = buildGraph' null g' s in (v' :: vs, newG)) ([], g) slist
        let depsDefaults = List.map lastValue deps
        let defaultV = List.fold (fun f a -> App (f,a)) e depsDefaults |> normalize
        let (v, g2) = Graph.addVertex (LiftV (List.map vertexId deps), e, defaultV) label g1
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

// todo: prawdziwy topological sort przez dfsa
// chwilowo dzieki letom mamy od razu posortowane odpowiednio wierzcholki
let topologicalSort (g : Graph<_, _>) : int list =
    List.map vertexId (snd g) |> List.rev

let processV (g : Graph<SigVertex, Edge>) (v : Vertex<SigVertex, Edge>) =
    let (id, label, vd) = fst v
    match vd with
        | (InputV, _, _) -> g
        | (LiftV depsIds, e, d)  -> 
            //let depsE = List.map (fun vId -> getEdges vId g |> List.filter (fun _ -> true)) depsIds
            //let newV = List.fold (fun f a -> App (f,a)) e depsDefaults |> normalize
            g
        | (FoldpV, e, d) -> undefined
    
//let dispatch (e : expr, sn : varname) g = 