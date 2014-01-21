module ParserTest

open System   
open Parser
 
let x = "   
    (\x -> if x then 0 else 6) (2 + 3 * 6)
"   
 
let lexbuf = Lexing.LexBuffer<_>.FromString x
printfn "%A" (Parser.program Lexer.tokenize lexbuf)
