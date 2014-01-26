module LexerTest

open System
open System.IO
open Lexer
open Parser
open TestUtil

let args = Environment.GetCommandLineArgs()
let lexbuf = filenameToLexbuf args.[1]
while not lexbuf.IsPastEndOfStream do  
    printfn "%A" (Lexer.tokenize lexbuf)
