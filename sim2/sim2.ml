open Netlist_ast
open Netgraph
open Scheduler
open Memory

type input = bool array
type output = string -> bool array

type tape = {
  t:bool array array;
  output:bool array;
  outcells:int array;
  mutable time:int;
  sch0:batch list;
  sch1:batch list;
  ma:memaddress;
}

type circuit = {
  in_length:int;
  out_length:int;
  tape:tape
}

let new_circuit p m =
  let g = mk_graph p in
  let sch1,maddr,sch0,out_order,n = batch g in
  let t=Array.make_matrix 2 n false in
  t.(0).(n-1) <- true;
  t.(1).(n-1) <- false;
  let ol = Array.length out_order in
  {
    tape = {
      t=t ;
      output=Array.make ol;
      outcells=out_order;
      time=0;
      sch=sch
    };
    in_length = n;
    out_length = ol;
  }

let len = Array.length in

let int_of_addr t a =
  let c = ref 0 in
  for i = len a-1 downto 0 do
    c := if t.(a.(i)) then !c * !c + 1 else !c * !c
  done

let rec update_address t = function
  | (i,a)::tl -> i := int_of_addr t a; update_address t tl
  | [] -> ()
      

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
        u.(start+i) <- t.(op.(i));
      done
  | BRom (start,op) ->
      for i = 0 to len op -1 do
        let ra,mem=op.(i) in
        u.(start+i) <- mem.(!ra)
      done
  | BRam (start,op) ->
      for i = 0 to len op -1 do
        let ra,we,wa,d,mem=op.(i) in
        u.(start+i) <- mem.(!ra);
        if t.(we) then
          mem.(!wa) <- t.(d);
      done

let rec follow_schedule t u = function
  | [] -> ()
  | h::tl ->
      execute t u h;
      follow_schedule t u tl

let step {
  in_length=il; out_length=ol;
  tape={
    t=t; time=d;
    sch0=sch0; sch1=sch1;
    output=o; outcells=oc;
    ma=ma}
  } input =
    Array.blit input 0 t 0 il;
    follow_schedule t.(d land 1) [||] sch1;
    update_address t.(d land 1) ma;
    follow_schedule t.(d land 1) t.(1- (d land 1)) sch0;
    for i = 0 to ol-1 do
      o.(i) <- t.(oc.(i));
    done;
    o
