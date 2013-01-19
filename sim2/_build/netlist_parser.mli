exception Error

type token = 
  | XOR
  | VAR
  | SLICE
  | SELECT
  | ROM
  | REG
  | RAM
  | OUTPUT
  | OR
  | NOT
  | NAND
  | NAME of (string)
  | MUX
  | INPUT
  | IN
  | EQUAL
  | EOF
  | CONST of (string)
  | CONCAT
  | COMMA
  | COLON
  | AND


val program: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Netlist_ast.program)