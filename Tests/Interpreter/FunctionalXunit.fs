module FunctionalXunit

open Parser
open TestUtil
open AbstractSyntax

open Swensen.Unquote
open Xunit

[<Fact>]
let TestFix () =
    let lexbuf = ParserInterface.parse (filenameToLexbuf "..\\..\\Interpreter\\fix.felm") in
    test <@ Functional.normalize lexbuf = Num 120 @>

[<Fact>]
let TestList () =
    let lexbuf = ParserInterface.parse (filenameToLexbuf "..\\..\\Interpreter\\list.felm") in
    test <@ Functional.normalize lexbuf = Num 5 @>

[<Fact>]
let TestLiftPath () = 
    let lexbuf = ParserInterface.parse (filenameToLexbuf "..\\..\\Interpreter\\graphLiftPath.felm") in
    test <@ Functional.normalize lexbuf = 
        Let
          ("plusThree",Lift (Fun ("x",Op (Var "x",Add,Num 3)),[Var "Window.width"]),
           Let
             ("timesTwo",Lift (Fun ("x",Op (Var "x",Mul,Num 2)),[Var "plusThree"]),
              Lift (Fun ("y",Var "y"),[Var "timesTwo"]))) @>

[<Fact>]
let TestLiftBranch () = 
    let lexbuf = ParserInterface.parse (filenameToLexbuf "..\\..\\Interpreter\\graphLiftBranch.felm") in
    test <@ Functional.normalize lexbuf = 
        Let ("timesTwo",
             Lift (Fun ("x",Op (Var "x",Mul,Num 2)),[Var "Window.width"]),
             Let ("plusThree",Lift (Fun ("x",Op (Var "x",Add,Num 3)),[Var "timesTwo"]),
              Let ("minusFive",Lift (Fun ("z",Op (Var "z",Sub,Num 5)),[Var "timesTwo"]),
                 Lift (Fun ("x",Fun ("y",Op (Var "x",Add,Var "y"))),
                    [Var "plusThree"; Var "minusFive"])))) @>

[<Fact>]
let TestWithCount () = 
    let lexbuf = ParserInterface.parse (filenameToLexbuf "..\\..\\Interpreter\\withCount.felm") in
    test <@ match Functional.normalize lexbuf with
                | Lift (Fun ("x",Fun ("y",Fun ("f",App (App (Var "f",Var "x"),Var "y")))),
                        [Var "Window.width";
                         Foldp (Fun (_,Fun (_,Op (Var _,Add,Num 1))),Num 0,Var "Window.width")])
                    -> true
                | _ -> false @>

[<Fact>]
let TestCanonicalForm () =
    let lexbuf = ParserInterface.parse (filenameToLexbuf "..\\..\\Interpreter\\canonicalForm.felm") in
    test <@ match Functional.normalize lexbuf with
                | Fun (y1,App (App (Fun (z1,Fun (_,Var z2)),Var y2),Var y3)) 
                    when y1 = y2 && y2 = y2 && z1 = z2
                    -> true
                | _ -> false @>