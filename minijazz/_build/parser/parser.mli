exception Error

type token = 
  | XOR
  | WHERE
  | THEN
  | STRING of (string)
  | STAR
  | SLASH
  | SEMICOL
  | RPAREN
  | ROM
  | REG
  | RBRACKET
  | RAM
  | PROBING
  | POWER
  | PLUS
  | OR
  | NOT
  | NAND
  | NAME of (string)
  | MINUS
  | LPAREN
  | LESS
  | LEQ
  | LBRACKET
  | INT of (int)
  | INLINED
  | IF
  | GREATER
  | EQUAL
  | EOF
  | END
  | ELSE
  | DOTDOT
  | DOT
  | CONST
  | COMMA
  | COLON
  | BOOL_INT of (string)
  | BOOL of (bool)
  | AND


val program: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.program)