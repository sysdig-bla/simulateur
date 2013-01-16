open Token

exception Eof

let read () =
  let stack = ref [] in
  let h = Lexing.from_channel stdin in
  try
    while true do
      match 
