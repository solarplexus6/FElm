module SignalXunit

open TestUtil
open AbstractSyntax
open Functional
open Graph
open Signal

open Swensen.Unquote
open Xunit

[<Fact>]
let TestLiftPathBuildGraph () = 
    let lexbuf = ParserInterface.parse (filenameToLexbuf "..\\..\\Interpreter\\graphLiftPath.felm") in
    test <@ normalize lexbuf |> buildGraph = 
             (((6, "main", (LiftV [4], Fun ("y",Var "y"), Num 6)), []),
              (8,
                  [((6, "main", (LiftV [4], Fun ("y",Var "y"), Num 6)), []);
                   ((4, "timesTwo", (LiftV [2] , Fun ("x",Op (Var "x",Mul,Num 2)), Num 6)),
                    [(7, 6, NoChange (Num 6))]);
                   ((2, "plusThree", (LiftV [1], Fun ("x",Op (Var "x",Add,Num 3)), Num 3)),
                    [(5, 4, NoChange (Num 3))]);
                   ((1, "Window.width", (InputV, Unit, Num 0)), [(3, 2, NoChange (Num 0))]);
                   ((0, "Window.height", (InputV, Unit, Num 0)), [])])) @>

let liftBranchGraph =
    (((8, "main", (LiftV [4; 6], Fun ("x",Fun ("y",Op (Var "x",Add,Var "y"))), Num -2)), []),
              (11,
                  [((8, "main", (LiftV [4; 6], Fun ("x",Fun ("y",Op (Var "x",Add,Var "y"))), Num -2)),
                    []);
                   ((6, "minusFive", (LiftV [2], Fun ("z",Op (Var "z",Sub,Num 5)), Num -5)),
                    [(10, 8, NoChange (Num -5))]);
                   ((4, "plusThree", (LiftV [2], Fun ("x",Op (Var "x",Add,Num 3)), Num 3)),
                    [(9, 8, NoChange (Num 3))]);
                   ((2, "timesTwo", (LiftV [1], Fun ("x",Op (Var "x",Mul,Num 2)), Num 0)),
                    [(7, 6, NoChange (Num 0)); (5, 4, NoChange (Num 0))]);
                   ((1, "Window.width", (InputV, Unit, Num 0)), [(3, 2, NoChange (Num 0))]);
                   ((0, "Window.height", (InputV, Unit, Num 0)), [])]))

[<Fact>]
let TestLiftBranchBuildGraph () = 
    let lexbuf = ParserInterface.parse (filenameToLexbuf "..\\..\\Interpreter\\graphLiftBranch.felm")
    test <@ normalize lexbuf |> buildGraph = liftBranchGraph @>

// test z dokladnoscia do nazw niektorych zmiennych
[<Fact>]
let TestFoldpBuildGraph () = 
    let lexbuf = ParserInterface.parse (filenameToLexbuf "..\\..\\Interpreter\\withCount.felm") in
    test <@ match normalize lexbuf |> buildGraph with
             | (((4, "main",
                       (LiftV [1; 2], Fun ("x",Fun ("y",Fun ("f",App (App (Var "f",Var "x"),Var "y")))),
                        Fun (_,App (App (Var _,Num 0),Num 0)))), []),
                 (7,
                      [((4, "main",
                         (LiftV [1; 2], Fun ("x",Fun ("y",Fun ("f",App (App (Var "f",Var "x"),Var "y")))),
                          Fun (_,App (App (Var _,Num 0),Num 0)))), []);
                       ((2, null,
                         (FoldpV, Fun (_,Fun (_,Op (Var _,Add,Num 1))), Num 0)),
                        [(6, 4, NoChange (Num 0))]);
                       ((1, "Window.width", (InputV, Unit, Num 0)),
                        [(5, 4, NoChange (Num 0)); (3, 2, NoChange (Num 0))]);
                       ((0, "Window.height", (InputV, Unit, Num 0)), [])]))
                 -> true
             | _ -> false @>


[<Fact>]
let TestProcessVLiftNoChange () =
    let lexbuf = ParserInterface.parse (filenameToLexbuf "..\\..\\Interpreter\\graphLiftBranch.felm")
    let (_, g) = normalize lexbuf |> buildGraph
    test <@ processV g (getVertexByLabel "timesTwo" g) = g @>

[<Fact>]
let TestProcessVFoldpNoChange() =
    let lexbuf = ParserInterface.parse (filenameToLexbuf "..\\..\\Interpreter\\withCount.felm")
    let (_, g) = normalize lexbuf |> buildGraph
    test <@ processV g (getVertex 2 g) = g @>

[<Fact>]
let TestPropagateChange () =
    let lexbuf = ParserInterface.parse (filenameToLexbuf "..\\..\\Interpreter\\graphLiftBranch.felm")
    let (_, g) = normalize lexbuf |> buildGraph
    test <@ propagateChange (vertexId <| getVertexByLabel "Window.width" g) (Num 1920) g = 
             [((8, "main", (LiftV [4; 6], Fun ("x",Fun ("y",Op (Var "x",Add,Var "y"))), Num -2)),
                    []);
                   ((6, "minusFive", (LiftV [2], Fun ("z",Op (Var "z",Sub,Num 5)), Num -5)),
                    [(10, 8, NoChange (Num -5))]);
                   ((4, "plusThree", (LiftV [2], Fun ("x",Op (Var "x",Add,Num 3)), Num 3)),
                    [(9, 8, NoChange (Num 3))]);
                   ((2, "timesTwo", (LiftV [1], Fun ("x",Op (Var "x",Mul,Num 2)), Num 0)),
                    [(7, 6, NoChange (Num 0)); (5, 4, NoChange (Num 0))]);
                   ((1, "Window.width", (InputV, Unit, Num 1920)), [(3, 2, Change (Num 1920))]);
                   ((0, "Window.height", (InputV, Unit, Num 0)), [])] @>

[<Fact>]
let TestProcessVLiftChange() = 
    let lexbuf = ParserInterface.parse (filenameToLexbuf "..\\..\\Interpreter\\graphLiftBranch.felm")
    let (_, g) = normalize lexbuf |> buildGraph
    let g' = (fst g, propagateChange (vertexId <| getVertexByLabel "Window.width" g) (Num 1920) g)
    test <@ processV g' (getVertexByLabel "timesTwo" g') = 
                (11,
                  [((8, "main", (LiftV [4; 6], Fun ("x",Fun ("y",Op (Var "x",Add,Var "y"))), Num -2)),
                    []);
                   ((6, "minusFive", (LiftV [2], Fun ("z",Op (Var "z",Sub,Num 5)), Num -5)),
                    [(10, 8, NoChange (Num -5))]);
                   ((4, "plusThree", (LiftV [2], Fun ("x",Op (Var "x",Add,Num 3)), Num 3)),
                    [(9, 8, NoChange (Num 3))]);
                   ((2, "timesTwo", (LiftV [1], Fun ("x",Op (Var "x",Mul,Num 2)), Num 3840)),
                    [(7, 6, Change (Num 3840)); (5, 4, Change (Num 3840))]);
                   ((1, "Window.width", (InputV, Unit, Num 1920)), [(3, 2, Change (Num 1920))]);
                   ((0, "Window.height", (InputV, Unit, Num 0)), [])]) @>

[<Fact>]
let TestProcessVFoldpChange() = 
    let lexbuf = ParserInterface.parse (filenameToLexbuf "..\\..\\Interpreter\\simpleFoldp.felm")
    let (_, g) = normalize lexbuf |> buildGraph
    let g' = (fst g, propagateChange (vertexId <| getVertexByLabel "Window.height" g) (Num 1200) g)
    test <@ processV g' (getVertex 2 g') =
                (6,
                      [((4, "main",
                         (LiftV [2], Fun ("s", Var "s"), Num 1000)), []);
                       ((2, null,
                         (FoldpV, Fun ("x",Fun ("acc",Op (Var "acc",Sub,Var "x"))), Num -200)),
                        [(5, 4, Change (Num -200))]);
                       ((1, "Window.width", (InputV, Unit, Num 0)), []);
                       ((0, "Window.height", (InputV, Unit, Num 1200)), 
                        [(3, 2, Change (Num 1200))])]) @>

[<Fact>]
let TestDispatchLiftBranch () =
    let lexbuf = ParserInterface.parse (filenameToLexbuf "..\\..\\Interpreter\\graphLiftBranch.felm")
    let (_, g) = normalize lexbuf |> buildGraph
    test <@ dispatch g (Num 1920, "Window.width") = 
                (11,
                  [((8, "main", (LiftV [4; 6], Fun ("x",Fun ("y",Op (Var "x",Add,Var "y"))), Num 7678)),
                    []);
                   ((6, "minusFive", (LiftV [2], Fun ("z",Op (Var "z",Sub,Num 5)), Num 3835)),
                    [(10, 8, Change (Num 3835))]);
                   ((4, "plusThree", (LiftV [2], Fun ("x",Op (Var "x",Add,Num 3)), Num 3843)),
                    [(9, 8, Change (Num 3843))]);
                   ((2, "timesTwo", (LiftV [1], Fun ("x",Op (Var "x",Mul,Num 2)), Num 3840)),
                    [(7, 6, Change (Num 3840)); (5, 4, Change (Num 3840))]);
                   ((1, "Window.width", (InputV, Unit, Num 1920)), [(3, 2, Change (Num 1920))]);
                   ((0, "Window.height", (InputV, Unit, Num 0)), [])]) @>

[<Fact>]
let TestSimulateLiftBranch () =
    let lexbuf = ParserInterface.parse (filenameToLexbuf "..\\..\\Interpreter\\graphLiftBranch.felm")
    let (_, g) = normalize lexbuf |> buildGraph
    test <@ simulate g [(Num 1920, "Window.width"); (Num 1000, "Window.width"); (Num 300, "Window.width"); (Num 500, "Window.width")] = 
                (11,
                  [((8, "main", (LiftV [4; 6], Fun ("x",Fun ("y",Op (Var "x",Add,Var "y"))), Num 1998)),
                    []);
                   ((6, "minusFive", (LiftV [2], Fun ("z",Op (Var "z",Sub,Num 5)), Num 995)),
                    [(10, 8, Change (Num 995))]);
                   ((4, "plusThree", (LiftV [2], Fun ("x",Op (Var "x",Add,Num 3)), Num 1003)),
                    [(9, 8, Change (Num 1003))]);
                   ((2, "timesTwo", (LiftV [1], Fun ("x",Op (Var "x",Mul,Num 2)), Num 1000)),
                    [(7, 6, Change (Num 1000)); (5, 4, Change (Num 1000))]);
                   ((1, "Window.width", (InputV, Unit, Num 500)), [(3, 2, Change (Num 500))]);
                   ((0, "Window.height", (InputV, Unit, Num 0)), [])]) @>

// test z dokladnoscia do nazw niektorych zmiennych
[<Fact>]
let TestSimulateWithCount () =
    let lexbuf = ParserInterface.parse (filenameToLexbuf "..\\..\\Interpreter\\withCount.felm")
    let (_, g) = normalize lexbuf |> buildGraph
    test <@ match simulate g [(Num 1920, "Window.width"); (Num 1000, "Window.width"); (Num 300, "Window.width"); (Num 500, "Window.width"); (Num 100, "Window.height")] with
             | (7,
                      [((4, "main",
                         (LiftV [1; 2], Fun ("x",Fun ("y",Fun ("f",App (App (Var "f",Var "x"),Var "y")))),
                          Fun (_,App (App (Var _,Num 500),Num 4)))), []);
                       ((2, null,
                         (FoldpV, Fun (_,Fun (_,Op (Var _,Add,Num 1))), Num 4)),
                        [(6, 4, NoChange (Num 4))]);
                       ((1, "Window.width", (InputV, Unit, Num 500)),
                        [(5, 4, NoChange (Num 500)); (3, 2, NoChange (Num 500))]);
                       ((0, "Window.height", (InputV, Unit, Num 100)), [])])
                 -> true
             | _ -> false @>