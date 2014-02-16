module Graph

open Helpers
open Option

type VertexData<'v> =
    int (* identifier *) *
    string (* label *) *
    'v (* vertex data *)

type EdgeData<'e> =
    int (* id *) *
    int (* vertex target *) *
    'e (* edge data*)

type 'e Adjacency = 'e EdgeData list

type Vertex<'v, 'e> = 
    VertexData<'v> *
    Adjacency<'e>

type Graph<'v, 'e> =
    int *
    Vertex<'v, 'e> list
 
let empty : Graph<_, _> = (0, [])

let vertexId (((id, _, _), _) : Vertex<_,_>) = id
let vertexLabel (((_, label, _), _) : Vertex<_, _>) = label
let vertexData (((_, _, vd), _) : Vertex<_, _>) = vd

let edgeData ((_, _, ed) : EdgeData<'e>) : 'e = ed

let isEdgeTarget id ((_, tId, _) : EdgeData<_>) : bool = id = tId

let getVertexByLabel l (g : Graph<_,_>) : Vertex<_,_> =
    snd g |> List.find (fun v' -> vertexLabel v' = l)

(* Get a vertex from a graph by id *)
let getVertex v (g : Graph<_, _>) : Vertex<_,_> =
    snd g |> List.find (fun v' -> vertexId v' = v)
(* Get all edges from a graph by a source vertex id *)
let getEdges v (g : Graph<_, _>) =
    g |> getVertex v |> snd

(* Add a new vertex *)
let addVertex (vd : 'v) (l : string) ((id, s) : Graph<'v, 'e>) : Vertex<'v,_> * Graph<'v, _> = 
    let newVd = (id, l, vd)
    let newV = (newVd, [])
    (newV, (id + 1, newV::s))
     
 (* Add a new edge. *)
let addEdge (s : int, t : int) (e : 'e) ((id, vs) : Graph<'v, 'e>) : int * Graph<'v, 'e> =    
    let newE : EdgeData<_> = (id, t, e)
    (id, (id + 1, vs |> List.map (fun v -> if (vertexId v) = s then (fst v, newE::(snd v)) else v)))

let mapV f = fun ((id, l, vd), e) -> (((id, l, f vd), e))
let mapVd f (g : Graph<'v, _>) : Graph<'v, _> = (fst g, snd g |> List.map (mapV f))
let mapVerts f (g : Graph<'v, _>) : Graph<'v, _> = (fst g, snd g |> List.map f)