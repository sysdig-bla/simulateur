{
  open Token

  exception Lex_error
}

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z' '_']

let space = ['\t' '\n' ' ']

rule token = parse
  | (letter (letter|digit)* as s) space* ':'
                  { L s }
  | (letter (letter|digit)* as s)
                  { Mark s }
  | '#'           { comment lexbuf }
  | space         { token lexbuf }
  | '-'? digit+ as n   { N (int_of_string n) }
  | eof           { EOF }
  | _             { raise Lex_error }

rule comment = parse
  | '\n'          { token lexbuf }
  | eof           { EOF }
  | [^'\n']       { comment lexbuf }
