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

type Env = Map<varname, Vertex<SigVertex, Edge>>

let rec buildGraph' (label : varname) (env : Env) (g : Graph<SigVertex, Edge>) = function
    | Var v -> (env.[v], g)
    | Lift (e, slist) ->
        // deps = dependencies
        let (reversedDeps, g1) = List.fold (fun (vs, g') s -> let (v', newG) = buildGraph' label env g' s in (v' :: vs, newG)) ([], g) slist
        let deps = List.rev reversedDeps
        let depsDefaults = List.map lastValue deps
        let defaultV = List.fold (fun f a -> App (f,a)) e depsDefaults |> normalize
        let (v, g2) = Graph.addVertex (LiftV (List.map vertexId deps), e, defaultV) label g1
        (v, List.fold (fun g' v' -> snd <| Graph.addEdge (vertexId v', vertexId v) (NoChange <| lastValue v') g') g2 deps)
    | Foldp (e, d, s) ->
        let (v', g1) = buildGraph' label env g s
        let (v, g2) = Graph.addVertex (FoldpV, e, d) null g1
        (v, snd <| Graph.addEdge (vertexId v', vertexId v) (NoChange <| lastValue v') g2)
    | Let (l, s, r) -> 
        let (v, g') = buildGraph' l env g s
        let env' = Map.add l v env
        buildGraph' label env' g' r          // r should be a signal term
    | _ -> failwith "buildGraph' received something other than a signal term"

[<Literal>]
let windowWidthVar = "Window.width"

[<Literal>]
let windowHeightVar = "Window.height"

let baseGraph : Graph<SigVertex, Edge> =
    Graph.addVertex (InputV, Unit, Num 0) windowHeightVar Graph.empty |> snd |>
    Graph.addVertex (InputV, Unit, Num 0) windowWidthVar |> snd

let baseEnv : Env =
    Map.empty |> 
    Map.add windowHeightVar (getVertexByLabel windowHeightVar baseGraph) |>
    Map.add windowWidthVar (getVertexByLabel windowWidthVar baseGraph)

let buildGraph = buildGraph' "main" baseEnv baseGraph

//
// Reduce combined with graph building for normalization of functions returning signals
//

let isSignal = function
  | Input _ -> true
  | _ -> false

let isSimple e = isValue e || isSignal e

let signalToInt = function
  | Input i -> i
  | _ -> failwith "signalToInt"

let rec sigReduce (g : Graph<SigVertex, Edge>) =
  let rec aux = function
    | App (Fun (x, e1), e2) -> printfn "APPLICATION"; (Let (x, e2, e1), g)                  // APPLICATION
    | App (e1, e2) ->
      let (r, g') = aux e1
      in (App (r, e2), g')
    | Op (Num n1, op, Num n2) -> printfn "OP"; (Num (binopToFunction op n1 n2), g) // OP
    | Op (Num n1, op, e2) ->
      let (r, _) = aux e2
      in (Op (Num n1, op, r), g)
    | Op (e1, op, e2) ->
      let (r, _) = aux e1
      in (Op (r, op, e2), g)
    | If (Num 0, e2, e3) -> printfn "COND_FALSE"; (e3, g)                                  // COND-FALSE
    | If (Num _, e2, e3) -> printfn "COND_TRUE"; (e2, g)                                  // COND-TRUE
    | If (e1, e2, e3) ->                
      let (r, _) = aux e1
      in (If (r, e2, e3), g)
    | Let (x, e1, e2) when isSimple e1 -> printfn "REDUCE"; (subst x e1 e2, g)          // REDUCE
    | Let (x, e1, e2) ->
      let (r, g') = aux e1
      in (Let (x, r, e2), g')
    | Lift (e1, elist) when isValue e1 && List.forall isSignal elist -> // LIFT
      let depNums = List.map signalToInt elist
      let defaultV = List.fold (fun f i -> App (f, lastValue <| getVertex i g)) e1 depNums |> normalize
      let (v, g1) = Graph.addVertex (LiftV depNums, e1, defaultV) "whatevs" g
      let i = vertexId v    
      let g2 = List.fold (fun g' i' -> snd <| Graph.addEdge (i', i) (NoChange (lastValue <| getVertex i' g')) g') g1 depNums
      (Input i, g2)
    | Lift (e1, elist) when isValue e1 ->
      let rec splitLift = function
        | (((Input i) as e) :: es) ->
          let (elist1, ek, elist2) = splitLift es
          in ((e :: elist1), ek, elist2)
        | (e :: es) -> ([], e, es)
        | [] -> failwith "splitLift"
      let (elist1, ek, elist2) = splitLift elist
      let (r, g') = aux ek
      in (Lift (e1, elist1 @ [r] @ elist2), g')
    | Lift (e1, elist) ->
      let (r, g') = aux e1
      in (Lift (r, elist), g')
    | Foldp (e1, e2, Input i) when isValue e1 && isValue e2 -> // FOLDP
      let (v, g1) = Graph.addVertex (FoldpV, e1, e2) "whatevs" g
      let i' = vertexId v
      let g2 = snd <| Graph.addEdge (i, i') (NoChange (lastValue <| getVertex i g1)) g1
      (Input i, g2)
    | Foldp (e1, e2, e3) when isValue e1 && isValue e2 ->
      let (r, g') = aux e3
      in (Foldp (e1, e2, r), g')
    | Foldp (e1, e2, e3) when isValue e1 ->
      let (r, g') = aux e2
      in (Foldp (e1, r, e3), g')
    | Foldp (e1, e2, e3) ->
      let (r, g') = aux e1
      in (Foldp (r, e2, e3), g')
    | _ -> failwith "sigReduce couldn't find a valid redex"
  in aux

let rec sigNormalize' (e, g) =
  if isSimple e then (e, g)
  else sigReduce g e |> sigNormalize'

let rec sigNormalize e = sigNormalize' (e, baseGraph)

//
// Propagation of events
//

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

// todo: przerobic dispatch dla wielu eventu wystepujacych jednoczesnie
let dispatch (g : Graph<SigVertex, Edge>) (e : expr, sn : varname) = 
    let inputs = snd g |> List.filter (fun v -> match vertexData v with (InputV, _, _) -> true | _ -> false) |> List.map vertexId
    // resetujemy wierzcholki Input (propagacja NoChange)
    let g1 = List.fold (fun accG inV -> (fst accG, propagateNoChange inV accG)) g inputs
    let v = getVertexByLabel sn g
    let g2 = (fst g1, propagateChange (vertexId v) e g1)
    List.fold processV g2 <| topologicalSort g2

let simulate (g : Graph<SigVertex, Edge>) (events : (expr * varname) list) = List.fold dispatch g events
