module ParserInterface

open Parser
open Lexer

exception ParseException of string

let parse lexbuf = 
    try Parser.program Lexer.tokenize lexbuf with
        ex -> let pos = lexbuf.EndPos
              in raise (ParseException (System.String.Format ("Parse exception at line: {0}, column: {1}", pos.Line, pos.Column)))