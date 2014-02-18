﻿module Signal

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
        buildGraph' label g' r          // r is guaranteed to be a signal term
    | _ -> failwith "buildGraph' received something other than a signal term"

let baseGraph : Graph<SigVertex, Edge> =
    Graph.addVertex (InputV, Unit, Num 0) "Window.height" Graph.empty |> snd |>
    Graph.addVertex (InputV, Unit, Num 0) "Window.width" |> snd

let buildGraph = buildGraph' "main" baseGraph

let changeE = function
    | Change _ -> true
    | NoChange _ -> false

let bodyOfE = function
    | Change v -> v
    | NoChange v -> v

let propagateChange vId newVal (g : Graph<SigVertex, Edge>) = 
    snd g |> List.map (fun v -> 
                        if (vertexId v) = vId
                        then 
                            (fst v |> updateLastVal newVal, snd v |> List.map (fun (eId, t, _) -> (eId, t, Change newVal)))
                        else v)

let propagateNoChange vId (g : Graph<'v, Edge>) =
    snd g |> List.map (fun v -> 
                        if (vertexId v) = vId
                        then 
                            (fst v, snd v |> List.map (fun (eId, t, _) -> (eId, t, NoChange <| lastValue v)))
                        else v)

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
                let depsVals = List.map (bodyOfE << edgeData) depsE
                let newVal = List.fold (fun f a -> App (f,a)) e depsVals |> normalize
                (fst g, propagateChange id newVal g)
            else
                // tutaj jednak trzeba zaktualizowac na NoChange, bo przeciez wczesniej moglo byc Change
                (fst g, propagateNoChange id g)
        | (FoldpV, f, d) ->
            let depE = 
                snd g |> first (fun (_, es) -> List.exists (isEdgeTarget id) es) |> 
                snd |> first (isEdgeTarget id) // nieefektywne, ale tymczasowe
                |> edgeData
            if changeE depE
            then
                let newVal = App (App (f, (bodyOfE depE)), d) |> normalize
                (fst g, propagateChange id newVal g)
            else
                (fst g, propagateNoChange id g)


// todo: prawdziwy topological sort przez dfsa
// chwilowo dzieki letom mamy od razu posortowane odpowiednio wierzcholki
let topologicalSort (g : Graph<'v, _>) : Vertex<'v,_> list =
    snd g |> List.rev

// todo: dispatch ma propagowac NoChange na sygnalach wejsciowych w ktorych
//       nie pojawila sie nowa wartosc?
// todo: przerobic dispatch dla wielu eventu wystepujacych jednoczesnie
let dispatch (e : expr, sn : varname) g = 
    let v = getVertexByLabel sn g
    let g' = (fst g, propagateChange (vertexId v) e g)
    List.fold processV g' <| topologicalSort g'
