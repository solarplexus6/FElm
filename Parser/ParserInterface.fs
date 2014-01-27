module ParserInterface

open Parser
open Lexer

type ParseException (message:string, ?innerException:exn) =
    inherit System.Exception (message, 
        match innerException with | Some(ex) -> ex | _ -> null)

let parse lexbuf = 
    try Parser.program Lexer.tokenize lexbuf with
        ex -> let pos = lexbuf.EndPos
              in raise (new ParseException (System.String.Format ("Parse exception at line: {0}, column: {1}", pos.Line, pos.Column), ex))