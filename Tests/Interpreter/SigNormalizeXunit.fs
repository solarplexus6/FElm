module SigNormalizeXunit

open TestUtil
open AbstractSyntax
open Functional
open Graph
open Signal

open Swensen.Unquote
open Xunit

// todo: dodac odpowiednio zmodyfikowane testy z SignalXunit

[<Fact>]
let TestLetWithFunApp () =
    let lexbuf = ParserInterface.parse (filenameToLexbuf "..\\..\\Interpreter\\letBodyFunApp.felm") in
    test <@ Signal.sigNormalize lexbuf = 
                (Signal 4,
                 (6,
                  [((4, null, (LiftV [1], Fun ("w^39",Var "w^39"), Num 0)), []);
                   ((2, null, (LiftV [0], Fun ("y^34",Var "y^34"), Num 0)), []);
                   ((1, "Window.width", (InputV, Unit, Num 0)), [(5, 4, NoChange (Num 0))]);
                   ((0, "Window.height", (InputV, Unit, Num 0)), [(3, 2, NoChange (Num 0))])])) @>