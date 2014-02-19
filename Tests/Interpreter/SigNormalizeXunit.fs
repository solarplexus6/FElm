module SigNormalizeXunit

open TestUtil
open AbstractSyntax
open Functional
open Graph
open Signal

open Swensen.Unquote
open Xunit

[<Fact>]
let TestLetWithFunApp () =
    let lexbuf = ParserInterface.parse (filenameToLexbuf "..\\..\\Interpreter\\letBodyFunApp.felm") in
    test <@ Signal.sigNormalize lexbuf = (Unit, (0, [])) @>