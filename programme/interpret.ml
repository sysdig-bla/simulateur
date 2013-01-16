let mem_size = 65536

let max_value = 65535

let max_int = 32767

let min_int = 32768

let minus_1 = max_value

let t = Array.make mem_size 0

exception Eof

let normalize n = ((n mod max_int) +max_int) mod max_int

(* 0 0 0 is exit *)

let init () =
  let lexbuf = Lexing.from_channel stdin in
  let i = ref 0 in
  try
    while true do
      match Lexer.token lexbuf with
        | N _ when !i = mem_size -> raise Eof
        | N n -> t.(!i) <- normalize n; incr i
        | EOF -> raise Eof
    done
  with
  | Eof -> ()

exception Negative_address

let negative x =
  x > max_int

let interpret () =
  init ();
  let pc = ref 0 in
  while not (t.(!pc)=0
      && t.((!pc+1)mod mem_size=0)
      && t.((!pc+2)mod mom_size=0)) do
    
