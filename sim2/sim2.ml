open Netlist_ast
open Netgraph
open Scheduler
open Memo

type input = bool array
type output = bool array

type tape = {
  t:bool array array;
  output:bool array;
  outcells:int array;
  mutable time:int;
  sch:batch list;
  ma:memaddress;
}

type circuit = {
  in_length:int;
  out_length:int;
  whereis:string -> int;
  size:string -> int;
  tape:tape
}

let new_circuit p =
  let g = mk_graph p in
  let sch,ma,out_order,n,in_length = batch g in
  let t=Array.make_matrix 2 n false in
  t.(0).(n-1) <- true;
  t.(1).(n-1) <- true;
  let ol = Array.length out_order in
  let out_pos,out_size = get_output_loc g in
  {
    tape = {
      t=t ;
      output=Array.make ol false;
      outcells=out_order;
      time=0;
      sch=sch;
      ma=ma
    };
    in_length = in_length;
    out_length = ol;
    whereis = (fun s -> Smap.find s out_pos);
    size = (fun s -> Smap.find s out_size);
  }

let len = Array.length

let int_of_addr t a =
  let c = ref 0 in
  for i = 0 to len a-1 do
    c := if t.(a.(i)) then !c * 2 + 1 else !c * 2
  done;
  !c

let update_address t l =
  Array.iter (fun (i,a) -> i := int_of_addr t a) l
      

let execute t u = function
  | BNot (start,op) ->
      for i = 0 to len op -1 do
        t.(start+i) <- not t.(op.(i));
      done
  | BAnd (start,op) ->
      for i = 0 to len op -1 do
        let a,b=op.(i) in
        t.(start+i) <- t.(a) && t.(b);
      done
  | BOr (start,op) ->
      for i = 0 to len op -1 do
        let a,b=op.(i) in
        t.(start+i) <- t.(a) || t.(b);
      done
  | BMux (start,op) ->
      for i = 0 to len op -1 do
        let a,b,c=op.(i) in
        t.(start+i)<- if t.(a) then t.(b) else t.(c)
      done
  | BReg (start,op) ->
      for i = 0 to len op -1 do
        t.(start+i) <- u.(op.(i));
      done
  | BRom (start,op) ->
      for i = 0 to len op -1 do
        let ra,mem=op.(i) in
        t.(start+i) <- mem.(!ra)
      done
  | BRam (start,op) ->
      for i = 0 to len op -1 do
        let ra,we,wa,d,mem=op.(i) in
        t.(start+i) <- mem.(!ra);
        if u.(we) then
          mem.(!wa) <- u.(d);
      done

let rec follow_schedule t u = function
  | [] -> ()
  | h::tl ->
      execute t u h;
      follow_schedule t u tl

let step ({
  in_length=il; out_length=ol;
  tape={
    t=t; time=d;
    sch=sch;
    output=o; outcells=oc;
    ma=ma}
  } as c) input =
    let t1 = d land 1 in
    let t0 = 1-t1 in
    Array.blit input 0 t.(t1) 0 il;
    update_address t.(t0) ma;
    follow_schedule t.(t1) t.(t0) sch;
    for i = 0 to ol-1 do
      o.(i) <- t.(t1).(oc.(i));
    done;
    c.tape.time <- c.tape.time+1;
    o

let print_raw h b =
  for i = 0 to Array.length b-1 do
    Format.fprintf h "%d" (if b.(i) then 1 else 0)
  done

let print_matrix h t =
  for i = 0 to Array.length t-1 do
    for j = 0 to Array.length t.(i)-1 do
      Format.fprintf h "%d" (if t.(i).(j) then 1 else 0)
    done;
    Format.fprintf h "@\n"
  done

let print_state h c =
  Format.fprintf h "Step %d | %a@\n"
    c.tape.time print_raw c.tape.output;
  print_matrix h c.tape.t;
