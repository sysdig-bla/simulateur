open Token
open Lexing
open Constants

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
        | Next -> incr index; stack := N !index::!stack
    done;
    []
  with
      | Eof -> rev_map_token !stack

let rec print h = function
  | h0::h1::h2::t -> Printf.fprintf h "%3d %3d %3d\n" h0 h1 h2; print h t
  | [h0;h1] -> Printf.fprintf h "%3d %3d\n" h0 h1
  | [h_] -> Printf.fprintf h "%3d\n" h_
  | [] -> ()

let split l =
  let rec split a0 a1 a2 = function
    | h0::h1::h2::t -> split (h0::a0) (h1::a1) (h2::a2) t
    | [h0;h1] -> h0::a0,h1::a1,a2
    | [h] -> h::a0,a1,a2
    | [] -> a0,a1,a2
  in
  let a0,a1,a2 = split [] [] [] l in
  List.rev a0,List.rev a1,List.rev a2

let print_bin h a =
  let a = ref a in
  for i = 1 to size do
    Printf.fprintf h "%d" (!a land 1);
    a := !a lsr 1;
  done

let rec print_binpart h = function
  | hd::t ->
      print_bin h hd;
      Printf.fprintf h "\n";
      print_binpart h t
  | [] -> ()

let print_binary h l =
  Printf.fprintf h "1\n";
  Printf.fprintf h "default %d %d\n" (List.length l) size;
  print_binpart h l;
