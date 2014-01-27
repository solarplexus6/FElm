module Graph

open Helpers

type VertexData = LiftVertex
                | FoldpVertex

type EdgeData =
    int (* identifier *) *
    int (* priority *) *
    int

type Adjacency = EdgeData list

type Vertex = 
    int *
    VertexData

type Graph =
    int *
    Vertex list *
    Adjacency
 
let empty : Graph = (0, [], [])

let addVertex (v : VertexData) (g : Graph) : int * Graph = undefined

let addEdge (s : int) (t : int) (g : Graph) : int * Graph = undefined