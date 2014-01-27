module Graph

type VertexData = LiftVertex
                | FoldpVertex

type EdgeData =
    int (* identifier *) *
    int (* priority *) *
    int (* vertex target *)

(* The graph uses adjacency list notation *)
type Adjacency = EdgeData list

type Vertex = 
    int (* identifier *) *
    VertexData

(* A Graph is a Vertex list.  The nextNode allows for
    consistent addressing of nodes *)
type Graph =
    int (* nextNode identifier *) *
    Vertex list *
    Adjacency

val empty : Graph

(* Add a new vertex *)
val addVertex : VertexData -> Graph -> 
    int (*new id*) * Graph
  
val addEdge :
    int (*source vertex*) ->
    int (*target vertex*) ->
    Graph ->
    int (*new id*) * Graph