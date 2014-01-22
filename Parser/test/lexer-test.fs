module LexerTest

open System
open System.IO
open Lexer
open Parser

let input =
  let args = Environment.GetCommandLineArgs()
  in File.OpenText(args.[1])
let lexbuf = Lexing.LexBuffer<_>.FromTextReader input
while not lexbuf.IsPastEndOfStream do  
    printfn "%A" (Lexer.tokenize lexbuf)
