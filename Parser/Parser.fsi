// Signature file for parser generated by fsyacc
module Parser

open AbstractSyntax

type token = 
  | RPAREN
  | LPAREN
  | GT
  | GE
  | NEQ
  | EQ
  | LE
  | LT
  | DIV
  | MUL
  | SUB
  | ADD
  | FOLDP
  | LIFT3
  | LIFT2
  | LIFT1
  | IN
  | EQUAL
  | LET
  | ELSE
  | THEN
  | IF
  | ARROW
  | FUN
  | ID of (string)
  | NUM of (int)
  | UNIT
  | EOF
type tokenId = 
    | TOKEN_RPAREN
    | TOKEN_LPAREN
    | TOKEN_GT
    | TOKEN_GE
    | TOKEN_NEQ
    | TOKEN_EQ
    | TOKEN_LE
    | TOKEN_LT
    | TOKEN_DIV
    | TOKEN_MUL
    | TOKEN_SUB
    | TOKEN_ADD
    | TOKEN_FOLDP
    | TOKEN_LIFT3
    | TOKEN_LIFT2
    | TOKEN_LIFT1
    | TOKEN_IN
    | TOKEN_EQUAL
    | TOKEN_LET
    | TOKEN_ELSE
    | TOKEN_THEN
    | TOKEN_IF
    | TOKEN_ARROW
    | TOKEN_FUN
    | TOKEN_ID
    | TOKEN_NUM
    | TOKEN_UNIT
    | TOKEN_EOF
    | TOKEN_end_of_input
    | TOKEN_error
type nonTerminalId = 
    | NONTERM__startprogram
    | NONTERM_program
    | NONTERM_expr
    | NONTERM_expr2
    | NONTERM_expr3
    | NONTERM_expr4
    | NONTERM_expr5
    | NONTERM_expr6
/// This function maps integers indexes to symbolic token ids
val tagOfToken: token -> int

/// This function maps integers indexes to symbolic token ids
val tokenTagToTokenId: int -> tokenId

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
val prodIdxToNonTerminal: int -> nonTerminalId

/// This function gets the name of a token as a string
val token_to_string: token -> string
val program : (Microsoft.FSharp.Text.Lexing.LexBuffer<'cty> -> token) -> Microsoft.FSharp.Text.Lexing.LexBuffer<'cty> -> (expr) 
