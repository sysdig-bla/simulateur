open Netgraph
open Reducegraph
open Scheduler

let g =
    mk_graph (Netlist.read_file Sys.argv.(1))

let () =
  Format.printf "Netlist 1 :@\n  @[%a@]@\n"
  print_graph g;
  Format.printf "Netlist 2 :@\n  @[%a@]@\n"
  print_sgraph (toposort g)

let () =
  reduce g;
  Format.printf "Netlist 3 :@\n  @[%a@]@\n"
  print_sgraph (toposort g);
  Format.printf "Netlist 4 :@\n  @[%a@]@\n"
  print_graph g

let () =
  let l,output,out_order,n,in_length = batch g in
  Format.printf "Schedule :@\n  @[%a@]@\n" print_sch l

  (*
let () =
  Printf.printf "%d\n" (nodenum (toposort g));
  reduce g;
  Printf.printf "%d\n" (nodenum (toposort g))
*)
