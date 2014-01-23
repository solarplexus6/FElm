module TestUtil

open System
open System.IO

let filenameToLexbuf filename =
  let input = File.OpenText(filename)
  in Lexing.LexBuffer<_>.FromTextReader input
