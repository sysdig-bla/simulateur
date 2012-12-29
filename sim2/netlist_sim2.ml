open Netlist_ast
open Sim2
open Netgraph
open Scheduler

let std = Format.std_formatter

let s_by_s = ref false
let cps = ref 0
let disp_sch = ref false
let debug_verbose = ref false
let verbose = ref false
let steps = ref 30
let seconds = ref 0.

let net_file = ref ""

let usage = Printf.sprintf
"Usage: %s [-m memory.mem] [-i IPS] [-disp-schedule] [-v] [-vb]"
  (Filename.basename Sys.argv.(0))

let optlist = [
  ("-m", Arg.String Memo.load_data,
    "ROM/RAM initialization data");
  ("-c", Arg.Set_int cps,
    "Clicks per second");
  ("-disp-schedule",Arg.Set disp_sch,
    "clear");
  ("-dv",Arg.Set debug_verbose,
  "Print output and tape");
  ("-v",Arg.Set verbose,
  "Print output");
  ("-s",Arg.Set_int steps,
  "Nb of steps (for fast mode)");
  ("-timeout",Arg.Set_float seconds,
  "Duration of program in seconds")
]


let print p =
  let g = mk_graph p in
  Format.printf "Original graph@\n    @[%a@]@\n"
    print_graph g;
  let sch,ma,out_order,n,in_length = batch g in
  let o_loc,o_sz = get_output_loc g in
  Format.printf "Schedule@\n    @[%a@]@\n"
    print_sch sch;
  Format.printf "Nodes - %d | Input - %d@\n" n in_length;
  Format.printf "Output nodes@\n    @[%a@]@\n"
    print_outnodes (out_order,o_loc,o_sz,p.p_outputs)

let sim p =
  let c = new_circuit p in
  let a = Array.make c.in_length false in
  if !debug_verbose then
    print_state std c;
  for i = 0 to !steps do
    let o=step c a in
    if !debug_verbose then
      print_state std c
    else if !verbose then
      Format.printf "STEP %d - %a@\n"
        c.tape.time print_raw o
  done

let sbs_sim p = failwith "Not done"

let gtod = Unix.gettimeofday

let wait = Unix.select [] [] [] 

let real_time cps p =
  let c = new_circuit p in
  let a = Array.make c.in_length false in
  let cps = float_of_int cps in
  let start = gtod () in
  let elapsed () = gtod () -. start in
  for i = 0 to !steps do
    let o = step c a in
    if !verbose then
      Format.printf "STEP %d - %a@\n"
      c.tape.time print_raw o;
    let t = elapsed () -. (float_of_int i)/.cps in
    if t>=0.05 then ignore (wait 0.05)
  done
    

let main () =
  let collect s = net_file := s in
  Arg.parse optlist collect usage;

  let p = Netlist.read_file !net_file in
  if !disp_sch
    then print p;
  if !s_by_s
    then sbs_sim p
  else if !cps>0
    then real_time !cps p
    else sim p

let () = main ()
