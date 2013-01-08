open Netlist_ast
open Sim2
open Netgraph
open Scheduler

let std = Format.std_formatter

let s_by_s = ref false
let cps = ref 0
let disp = ref false
let debug_verbose = ref false
let verbose = ref false
let steps = ref 10
let seconds = ref 0.
let screen = ref false

let net_file = ref ""

let usage = Printf.sprintf
"Usage: %s [-m memory.mem] [-c CPS] [-disp-schedule] [-v] [-vb] [-s steps]"
  (Filename.basename Sys.argv.(0))

let optlist =
  Arg.align
  [
    ("-m", Arg.String Memo.load_data,
      " ROM/RAM initialization data");
    ("-c", Arg.Set_int cps,
      " Clicks per second");
    ("-disp-schedule",Arg.Set disp,
      " Clear");
    ("-dv",Arg.Set debug_verbose,
      " Print output and tape");
    ("-v",Arg.Set verbose,
      " Print output");
    ("-s",Arg.Set_int steps,
      " Nb of steps");
    ("-timeout",Arg.Set_float seconds,
      " Duration of program in seconds");
    ("-disp",Arg.Set screen,
      " Display the screen (only in slow mode)");
  ]


let new_circuit p =
  if !disp
    then begin
      let g = mk_graph p in
      let nb_nodes = !free in
      if nb_nodes < 20 then
        Format.printf "Original graph@\n    @[%a@]@\n"
          print_graph g;
      let sch,ma,out_order,n,in_length = batch g in
      let t=Array.make_matrix 2 n false in
      t.(0).(n-1) <- true;
      t.(1).(n-1) <- true;
      let ol = Array.length out_order in
      let o_loc,o_sz = get_output_loc g in
      Format.printf "Schedule@\n    @[%a@]@\n"
        print_sch sch;
      Format.printf "Output nodes@\n    @[%a@]@\n"
        print_outnodes (out_order,o_loc,o_sz,p.p_outputs);
      Format.printf "Initially %d eqs, %d nodes@\n"
        (List.length p.p_eqs) nb_nodes;
      Format.printf "Nodes - %d | Input - %d@\n" n in_length;
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
        whereis = (fun s -> Smap.find s o_loc);
        size = (fun s -> Smap.find s o_sz);
      }
    end
  else new_circuit p

let gtod = Unix.gettimeofday

let wait = Unix.select [] [] [] 

let sim c =
  let a = Array.make c.in_length false in
  let start = gtod () in
  let elapsed () = gtod () -. start in
  if !debug_verbose then
    print_state std c;
  for i = 0 to !steps do
    let o=step c a in
    if !debug_verbose then
      print_state std c
    else if !verbose then
      Format.printf "STEP %d - %a@."
        i print_raw o;
    if !seconds > 0. && elapsed()> !seconds
	then exit 0
  done

let sbs_sim c = failwith "Not done"

let real_time cps c =
  let a = Array.make c.in_length false in
  let cps = float_of_int cps in
  let start = gtod () in
  let elapsed () = gtod () -. start in
  if !screen then
      Display.open_display ();
  for i = 0 to !steps do
    let o = step c a in
    if !screen then
        Display.update o;
    if !verbose then
      Format.printf "STEP %d - %a@."
      i print_raw o;
    let t = (float_of_int i)/.cps -. elapsed () in
    if t>0.05 then ignore (wait t)
  done

let main () =
  let collect s = net_file := s in
  Arg.parse optlist collect usage;

  let p = Netlist.read_file !net_file in
  let c = new_circuit p in
  if !s_by_s
    then sbs_sim c
  else if !cps>0
    then real_time !cps c
    else sim c

let () = main ()
