open Tape
open Netlist_ast


(* Ce code ne fait rien d'intéressant, mais uniquement :
    * La gestion des arguments donnés en ligne de commande
    * La gestion des valeurs d'entrée et sortie en mode interactif
 *)

(* Parsing of the arguments *)
let usage = Printf.sprintf
"Usage: %s [-bd] netlist"
(Filename.basename Sys.argv.(0))

let batch_mode = ref false
let debug_mode = ref false

let optlist = [
    ("-b", Arg.Unit (fun () -> batch_mode := true), " batch mode");
    ("-d", Arg.Unit (fun () -> debug_mode := true), " debug mode");
]


let main () =
    let args = ref [] in
    let collect arg =
        args := !args @ [arg] in
    Arg.parse optlist collect usage;

    (match !args with
    | [] -> print_string "No netlist to simulate. See --help.\n"
    | h::t ->
        begin
            (* Loads the netlist *)
            let pr = Netlist.read_file h in 
            let proxy = Netlist_proxy.create_from_program pr in
            let cycles_count = ref 0 in
            
            (* The next 3 functions are needed for the batch mode.
             * It's not implemented yet. *)
            let get_input_b () = [| false; false; false |] in

            let is_input_available_b () =
                if !cycles_count < 3 then
                    (incr cycles_count; true)
                else false in

            let put_output_b tab =
                List.iter (fun i -> Printf.printf "%d " (if i then 1 else 0)) tab;
                Format.printf "\n"
            in

            (* Reads the input from the standard input *)
            let input_i_array = Array.make (Netlist_proxy.nb_inputs proxy) false in
            let get_input_i () =
                Format.printf "Step %d:\n%!" (!cycles_count + 1);
                let i = ref 0 in
                while !i < Netlist_proxy.nb_inputs proxy do
                    Printf.printf "%s ? %!" (Netlist_proxy.get_name proxy !i);
                    try
                        input_i_array.(!i) <- Scanf.bscanf (Scanf.Scanning.stdin)
                            " %d" (fun x -> x > 0);
                        incr i
                    with Scanf.Scan_failure(_) ->
                        let _ = Scanf.bscanf (Scanf.Scanning.stdin) " %s" (fun x
                        -> x) in () 
                done;
                input_i_array
            in

            (* To stop the program, interrupt it with ^C *)
            let is_input_available_i () = true
            in

            (* Prints the output to the standard output *)
            let put_output_i lst =
                let nbinputs = Netlist_proxy.nb_inputs proxy in
                let tab = Array.of_list lst in
                for i = 0 to Netlist_proxy.nb_outputs proxy - 1 do
                    Printf.printf "=> %s = %d\n%!"
                    (Netlist_proxy.get_name proxy (nbinputs+i))
                    (if tab.(i) then 1 else 0)
                done;
                incr cycles_count
            in

            (* Simulates the netlist *)
            if !batch_mode then
                simulate proxy pr.p_eqs get_input_b put_output_b
                is_input_available_b !debug_mode
            else
                simulate proxy pr.p_eqs get_input_i put_output_i 
                is_input_available_i !debug_mode 
        end
    )

let () = main ()
