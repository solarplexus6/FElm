module ParserTest

open System
open System.IO
open Lexer
open Parser
open TestUtil

let args = Environment.GetCommandLineArgs()
let lexbuf = filenameToLexbuf args.[1]
printfn "%A" (Parser.program Lexer.tokenize lexbuf)
