open Token
open Lexing

exception Eof

let read h =
  let stack = ref [] in
  let h = from_channel h in
  let tbl = Hashtbl.create 120 in
  let rev_map_token l =
    let rec rev_map acc = function
      | [] -> acc
      | N n::t -> rev_map (n::acc) t
      | L l::t -> begin
          try
            rev_map (Hashtbl.find tbl l::acc) t
          with
            | Not_found ->
                let pos = lexeme_start_p h in
                Printf.eprintf "Line %d : Label not found %s\n"
                  pos.pos_lnum l;
                exit 2
        end
      | _ -> assert false
    in rev_map [] l
  in
  let index = ref 0 in
  try
    while true do
      match Lexer.token h with
        | EOF -> raise Eof
        | N n -> stack := N n::!stack; incr index
        | L l -> stack := L l::!stack; incr index
        | Mark l -> Hashtbl.add tbl l !index
        | Current -> stack := N !index::!stack; incr index
    done;
    []
  with
      | Eof -> rev_map_token !stack

let rec print h = function
  | h1::h2::h3::t -> Printf.fprintf h "%3d %3d %3d\n" h1 h2 h3; print h t
  | [h1;h2] -> Printf.fprintf h "%3d %3d\n" h1 h2
  | [h_] -> Printf.fprintf h "%3d\n" h_
  | [] -> ()
