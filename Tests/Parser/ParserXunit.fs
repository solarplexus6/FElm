module ParserXunit

open AbstractSyntax
open Parser
open TestUtil

open Swensen.Unquote
open Xunit

[<Fact>]
let TestLambdaApplication () =
    test <@ ParserInterface.parse (filenameToLexbuf "..\\..\\Parser\\test1.felm") = App (Fun ("x",If (Var "x",Num 0,Num 6)),Op (Num 2,Add,Op (Num 3,Mul,Num 6))) @>

[<Fact>]
let TestFoldpLift () =
    test <@ ParserInterface.parse (filenameToLexbuf "..\\..\\Parser\\test2.felm") = 
        Let
          ("x",Op (Num 3,Div,Num 2),
           Let
             ("y",Op (Var "x",Add,Num 7),
              If
                (Op (Op (Var "x",Add,Var "y"),Gt,Num 5),Foldp (Num 1,Num 2,Num 3),
                 App (Fun ("z",Unit),Lift (Num 1,[Num 2; Num 3]))))) @>

[<Fact>]
let TestLet () =
    test <@ ParserInterface.parse (filenameToLexbuf "..\\..\\Parser\\test3.felm") = Let ("f",Fun ("x",Num 0),Op (App (Var "f",Num 5),Add,Num 7)) @>

[<Fact>]
let TestEqOpComposition () =
    raises<ParserInterface.ParseException> <@ ParserInterface.parse (filenameToLexbuf "..\\..\\Parser\\test4.felm") @>

[<Fact>]
let TestLambda () =
    test <@ ParserInterface.parse (filenameToLexbuf "..\\..\\Parser\\test5.felm") = Fun ("x", Op (Num 5, Add, Num 2)) @>

[<Fact>]
let TestAssociativity () =
    test <@ ParserInterface.parse (filenameToLexbuf "..\\..\\Parser\\test6.felm") = App (App (Var "f1", Var "f2"), Num 5) @>