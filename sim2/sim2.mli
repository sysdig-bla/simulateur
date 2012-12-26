type circuit
type input = bool array
type output = string -> bool array

val new_circuit : Netls2.netlist -> Mem2.mem -> circuit

val step : circuit -> input -> output
