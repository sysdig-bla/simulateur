
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
            Graph.add_edge gr i j)
    (Netlist_proxy.get_instructions proxy);

    let order = List.rev (Graph.tsort gr) in

    order


