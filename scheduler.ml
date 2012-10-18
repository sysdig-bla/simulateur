
(*
 * Types pour les instructions converties (on change les
 * noms de variables en leurs indentifiants entiers)
 *)

open Netlist_ast

type iarg =
    | IAvar of int
    | IAconst of int

type iexp =
    | IEarg of iarg
    | IEreg of int 
    | IEnot of iarg
    | IEbinop of binop * iarg * iarg
    | IEmux of iarg * iarg * iarg
    | IErom of int * int * iarg
      (*addr size, word size, read addr*)
    | IEram of int * int * iarg * iarg * iarg * iarg
      (*addr size, word size, read addr, write enable, write addr, data*)
    | IEconcat of iarg * iarg
    | IEslice of int * int * iarg
    | IEselect of int * iarg

type iequation = int * iexp

(*
 * Traduction de la netlist originale en netlist ordonnée avec identifiants
 * entiers
 *)

let schedule_program pr =
    let proxy = Netlist_proxy.create_from_program pr in
    let nb_noeuds = Netlist_proxy.nb_identifiers proxy in
    let gr = Graph.make nb_noeuds in

    List.iter (fun (i,j) -> Graph.add_edge gr i j)
    (Netlist_proxy.get_instructions proxy);

    let order = List.rev (Graph.tsort gr) in

    (* TODO *)

    (* debug temporaire : *)
    List.iter (fun i ->
        Format.printf "[%d] : %s\n" i (Netlist_proxy.get_name proxy i))
        order



