
(*
 * Types pour les instructions converties (on change les
 * noms de variables en leurs indentifiants entiers)
 *)

open Netlist_ast

(*
 * Traduction de la netlist originale en netlist ordonnée avec identifiants
 * entiers
 *)


let schedule_program proxy =
    let nb_noeuds = Netlist_proxy.nb_identifiers proxy in
    let gr = Graph.make nb_noeuds in

    List.iter (fun (i,j) ->
        if not (Netlist_proxy.is_register proxy j) then
            Graph.add_edge gr j i)
    (Netlist_proxy.get_instructions proxy);

    let rec move_registers_to_the_front accu = function
        | [] -> accu
        | h::t when Netlist_proxy.is_register proxy h ->
                move_registers_to_the_front (h::accu) t
        | h::t -> move_registers_to_the_front (accu @ [h]) t
    in

    let order = (Graph.tsort gr) in

    move_registers_to_the_front [] order


