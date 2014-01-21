module LexerTest

open System   
open Lexer
 
let x = "   
    if 0 then 5 else 6   
"   
 
let lexbuf = Lexing.LexBuffer<_>.FromString x   
while not lexbuf.IsPastEndOfStream do  
    printfn "%A" (Lexer.tokenize lexbuf)   
