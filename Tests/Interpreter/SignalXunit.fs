module SignalXunit

open TestUtil
open AbstractSyntax
open Signal
open Functional

open Swensen.Unquote
open Xunit

[<Fact>]
let TestLiftPath () = 
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

[<Fact>]
let TestLiftBranch () = 
    let lexbuf = ParserInterface.parse (filenameToLexbuf "..\\..\\Interpreter\\graphLiftBranch.felm") in
    test <@ normalize lexbuf |> buildGraph = 
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
                   ((0, "Window.height", (InputV, Unit, Num 0)), [])])) @>

[<Fact>]
let TestFoldp () = 
    let lexbuf = ParserInterface.parse (filenameToLexbuf "..\\..\\Interpreter\\withCount.felm") in
    test <@ normalize lexbuf |> buildGraph =
             (((4, "main",
                   (LiftV [1; 2], Fun ("x",Fun ("y",Fun ("f",App (App (Var "f",Var "x"),Var "y")))),
                    Fun ("f^7",App (App (Var "f^7",Num 0),Num 0)))), []),
             (7,
                  [((4, "main",
                     (LiftV [1; 2], Fun ("x",Fun ("y",Fun ("f",App (App (Var "f",Var "x"),Var "y")))),
                      Fun ("f^7",App (App (Var "f^7",Num 0),Num 0)))), []);
                   ((2, null,
                     (FoldpV, Fun ("x^1",Fun ("acc^3",Op (Var "acc^3",Add,Num 1))), Num 0)),
                    [(6, 4, NoChange (Num 0))]);
                   ((1, "Window.width", (InputV, Unit, Num 0)),
                    [(5, 4, NoChange (Num 0)); (3, 2, NoChange (Num 0))]);
                   ((0, "Window.height", (InputV, Unit, Num 0)), [])])) @>