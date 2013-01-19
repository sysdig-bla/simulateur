open Constants

let t = Array.make mem_size 0

let end_cells = 8 (* to be printed *)

exception Eof

let print_cells h =
  for i = end_cells downto 1 do
    Printf.fprintf h "%3d " t.(mem_size-i)
  done;
  Printf.fprintf h "\n%!"

let normalize n = ((n mod mem_size)+mem_size) mod mem_size

(* 0 0 0 is exit *)

let init l =
  let rec init i = function
    | h::tl -> t.(i) <- normalize h; init (i+1) tl
    | [] -> ()
  in init 0 l

let relative x =
  if x > max_int then x-mem_size else x

let print h s pc a b c () =
  Printf.fprintf h "%3d - pc=%3d - " s pc;
  Printf.fprintf h "%3d %3d %3d : " (relative a) (relative b) (relative c);
  print_cells h
  

let interpret h l steps =
  init l;
  let pc = ref 0 in
  let s = ref 0 in
  let continue = ref true in
  Printf.fprintf h "  0 : ";
  print_cells h;
  while !continue && !s < steps do
    incr s;
    let a = t.(!pc) in
    let b = t.(!pc+1) in
    let c = t.(!pc+2) in
    let print = print h !s !pc a b c in
    (* if c = b, then the cell is overwritten at the next cycle *)
    let diff = (t.(b)-t.(a)+mem_size) mod mem_size in
    t.(b) <- diff;
    if (a= !pc && b= !pc && c= !pc)
      then continue := false
    else if t.(b) > max_int || t.(b) = 0
      then pc := c
    else pc := !pc+3;
    if !pc > max_int-3
      then continue := false;
    if steps < 11 || !s land 524287 = 0
      then print ()
  done;
  Printf.fprintf h "## END OF SIMULATION ##\n"
