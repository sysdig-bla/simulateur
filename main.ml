open Tape
open Netlist_ast
open Graph

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

(* Fonction utilitaires *)
let string_to_bool_array s =
    if (String.length s = 0) then
        raise (Scanf.Scan_failure "");
    Array.init (String.length s)
        (fun i -> if (s.[i] <> '0' && s.[i] <> '1') then
               raise (Scanf.Scan_failure ""); s.[i] = '1')

let bool_array_to_string t =
    let nb = Array.length t in
    let str = String.make nb ' ' in
    for i = 0 to nb-1 do
        str.[i] <- (if t.(i) then '1' else '0')
    done;
    str


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
            let get_input_b () = [| [| false; false; false |] |] in

            let is_input_available_b () =
                if !cycles_count < 3 then
                    (incr cycles_count; true)
                else false in

            let put_output_b lst =
                List.iter (fun i -> Printf.printf "%s "
                    (bool_array_to_string i)) lst;
                Format.printf "\n"
            in

            (* Reads the input from the standard input *)
            let input_i_array = Array.make (Netlist_proxy.nb_inputs proxy) [| |] in
            let get_input_i () =
                Format.printf "Step %d:\n%!" (!cycles_count + 1);
                let i = ref 0 in
                while !i < Netlist_proxy.nb_inputs proxy do
                    Printf.printf "%s ? %!" (Netlist_proxy.get_name proxy !i);
                    try
                        input_i_array.(!i) <- Scanf.bscanf (Scanf.Scanning.stdin)
                            " %s" string_to_bool_array;
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
                    Printf.printf "=> %s = %s\n%!"
                    (Netlist_proxy.get_name proxy (nbinputs+i))
                    (bool_array_to_string tab.(i))
                done;
                incr cycles_count
            in

            (* Simulates the netlist *)
            try
                if !batch_mode then
                    simulate proxy pr.p_eqs get_input_b put_output_b
                    is_input_available_b !debug_mode
                else
                    simulate proxy pr.p_eqs get_input_i put_output_i 
                    is_input_available_i !debug_mode
            with Cycle ->
                Printf.printf "Impossible de simuler la netlist : elle contient un cycle.\n%!";
        end
    )

let () = main ()
