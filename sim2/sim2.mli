type input = bool array
type output = string -> bool array

type tape

type circuit = {
  in_length:int; (* In the same order as in the .net file *)
  out_length:int; (* same *)
  tape:tape
}

val new_circuit : Netlist_ast.program -> Mem2.mem -> circuit

val step : circuit -> input -> output
