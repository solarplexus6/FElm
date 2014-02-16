module Signal

open AbstractSyntax
open Graph
open Helpers
open Functional
open Helpers

type SigDef = LiftV of int list | FoldpV | InputV

type SigVertex = 
    SigDef * 
    expr (*function *) * 
    expr (*default/last value *)

type Edge = 
    | Change of expr
    | NoChange of expr

let lastValue (((_, _, (_, _, v)), _) : Vertex<SigVertex, _>) : expr = v
let updateLastVal newVal ((id, l, (sd, f, v)) : VertexData<SigVertex>) : VertexData<SigVertex> = 
    (id, l, (sd, f, newVal))

// todo: przypadki gdy na koncu leta mamy wartosc, nie sygnal
let rec buildGraph' (label : string) (g : Graph<SigVertex, Edge>) = function
    | Var v -> (getVertexByLabel v g, g)
    | Lift (e, slist) ->
        // deps = dependencies
        let (reversedDeps, g1) = List.fold (fun (vs, g') s -> let (v', newG) = buildGraph' null g' s in (v' :: vs, newG)) ([], g) slist
        let deps = List.rev reversedDeps
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

let changeE = function
    | Change _ -> true
    | NoChange _ -> false

let bodyE = function
    | Change v -> v
    | NoChange v -> v

let processV (g : Graph<SigVertex, Edge>) (v : Vertex<SigVertex, Edge>) =
    let (id, label, vd) = fst v
    match vd with
        | (InputV, _, _) -> g
        | (LiftV depsIds, e, d)  -> 
            let depsE = 
                List.map (fun vId -> getEdges vId g |> List.filter (isEdgeTarget id)) depsIds |>
                List.concat
            if List.exists (changeE << edgeData) depsE 
            then
                let depsVals = List.map (bodyE << edgeData) depsE
                let newVal = List.fold (fun f a -> App (f,a)) e depsVals |> normalize
                let newVs =
                    snd g |> List.map (fun v -> 
                        if (vertexId v) = id 
                        then 
                            (fst v |> updateLastVal newVal, snd v |> List.map (fun (eId, t, _) -> (eId, t, Change newVal)))
                        else v)
                (fst g, newVs)
            else g
        | (FoldpV, e, d) ->
            let depV = 
                (first (fun (_, es) -> List.exists (isEdgeTarget id) es) <| snd g) |> fst
                //List.map (fun vId -> getEdges vId g |> List.filter (isEdgeTarget id))  |>
            //    List.concat
            g

// todo: prawdziwy topological sort przez dfsa
// chwilowo dzieki letom mamy od razu posortowane odpowiednio wierzcholki
let topologicalSort (g : Graph<_, _>) : int list =
    List.map vertexId (snd g) |> List.rev

//let dispatch (e : expr, sn : varname) g = 