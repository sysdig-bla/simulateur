
open Tape
open Netlist_ast

let inputs = [| false; false; false; false; false; true; false; true; false; false; true; true; true; false; false; true; false; true; true; true; false; true; true; true |] 

let pr = Netlist.read_file "test/fulladder.net" 
let proxy = Netlist_proxy.create_from_program pr
 
let outputs = simulate proxy pr.p_eqs 8 (Array.map (fun x -> { v = [| x |]; a =
    fun () -> () }) inputs) 

let () = List.iter (fun x -> Format.printf "%b " x) outputs;
print_newline ()



