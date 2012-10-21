open Tape
open Netlist_ast


(* Parsing of the arguments *)
let usage = Printf.sprintf
"Usage: %s [-b] netlist"
(Filename.basename Sys.argv.(0))

let batch_mode = ref false

let optlist = [
    ("-b", Arg.Unit (fun () -> batch_mode := true), " : batch mode");
]


let main () =
    let args = ref [] in
    let collect arg =
        args := !args @ [arg] in
    Arg.parse optlist collect usage;

    let inputs = [| false; false; false; false; false; true; false; true; false; false; true; true; true; false; false; true; false; true; true; true; false; true; true; true |] 
    in

    (match !args with
    | [] -> print_string "No netlist to simulate. See --help.\n"
    | h::t ->
        begin
            (* Loads the netlist *)
            let pr = Netlist.read_file h in 
            let proxy = Netlist_proxy.create_from_program pr in
            let cycles_count = ref 0 in
            
            
            let get_input_b () = [| false; false; false |] in

            let is_input_available_b () =
                if !cycles_count < 3 then
                    (incr cycles_count; true)
                else false in

            let put_output_b tab =
                List.iter (fun i -> Printf.printf "%d " (if i then 1 else 0)) tab;
                Format.printf "\n"
            in

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

            let is_input_available_i () = true
            in

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
                is_input_available_b
            else
                simulate proxy pr.p_eqs get_input_i put_output_i
                is_input_available_i
        end
    )

    let () = main ()
