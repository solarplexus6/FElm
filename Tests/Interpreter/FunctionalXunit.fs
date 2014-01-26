module FunctionalXunit

open Parser
open TestUtil
open AbstractSyntax

open Swensen.Unquote
open Xunit

[<Fact>]
let TestFix () =
    test <@ Functional.normalize <| ParserInterface.parse (filenameToLexbuf "..\\..\\Interpreter\\fix.felm") = Num 120 @>

[<Fact>]
let TestList () =
    test <@ Functional.normalize <| ParserInterface.parse (filenameToLexbuf "..\\..\\Interpreter\\list.felm") = Num 5 @>