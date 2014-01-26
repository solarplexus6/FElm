module FunctionalTest

open System
open System.IO
open Functional
open Lexer
open Parser
open PrettyPrinter
open TestUtil

let rec debug e =
  printfn "%s\n" (pretty_print e);
  if isFinalTerm e then ()
  else reduce e |> debug

let fromFile filename =
  let lexbuf = TestUtil.filenameToLexbuf filename
  debug (ParserInterface.parse lexbuf)

let args = Environment.GetCommandLineArgs()
fromFile args.[1]