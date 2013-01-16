{
  open Token

  exception Lex_error
}

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z' '_']

let space = ['\t' '\n' ' ']

rule token = parse
  | (letter (letter|digit)* as s) space* ':'
                  { Mark s }
  | (letter (letter|digit)* as s)
                  { L s }
  | ';'           { Next }
  | '#'           { comment lexbuf }
  | '\n'          { Lexing.new_line lexbuf; token lexbuf }
  | space         { token lexbuf }
  | '-'? digit+ as n   { N (int_of_string n) }
  | eof           { EOF }
  | _             { raise Lex_error }

and comment = parse
  | '\n'          { token lexbuf }
  | eof           { EOF }
  | [^'\n']       { comment lexbuf }
