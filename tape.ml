(*
 *
 *  Module Tape
 *
 *  Toutes les "lieux de stockage" (entrées, sorties, variables, registres)
 *  sont placés sur un ruban (array). Les indices du tableau représentent
 *  les identifieurs (int) de ces "lieux de stockage".
 *  Chaque case contient deux champs modifiables, le premier pour la valeur
 *  et le second pour un fonction d'évaluation (unit -> unit).
 *
 *  Netlist_proxy.get_id à réfléchir. Avec on initialise le ruban à partir
 *  de la liste des équations du programme, au fond on doit chercher
 *  1, 2, 3 ou 4 identifieurs à partir de leurs noms sinon on peut
 *  initialiser selon l'ordre des identifieurs mais on doit rechercher
 *  l'équation qui correspond.
 *  Dans tous les cas, il ne s'agit que d'une étape de la préparation
 *  à la simulation.
 *
 *)

open Netlist_ast
open Netlist_proxy


type case =
  {
    mutable v: value;
    mutable a: unit -> unit
  }
(* Labels "v" as "value" and "a" as "assess" *)

type tape = case array


let make_tape nb_vars =
  Array.make nb_vars {v = VBit false; a = fun () -> ()}

let interpret prg tp id1 exp = match exp with
  | Earg (Avar s) ->
    let id2 = get_id prg s in
    (fun () -> tp.(id1).v <- tp.(id2).v)
  | Earg (Aconst v) ->
    (fun () -> tp.(id1).v <- v)
  | _ ->
    (fun () -> ())
(*| ... -> ... *)

let init_case prg tp eq =
  let (id, exp) = eq in
  let a = interpret prg tp id exp in
  tp.(id).a <- a

let rec init_tape prg tp p_eqs = match p_eqs with
  | [] -> ()
  | eq :: tl -> let () = init_case prg tp eq in
    init_tape prg tp tl

let rec execute_tape tp ord = match ord with
  | [] -> ()
  | i :: tl -> let () = snd tp.(i) in
    execute_tape tp tl

let outputs_cycle tp ofs_outputs nb_outputs =
  let o = Array.sub tp ofs_outputs nb_outputs in
  Array.to_list o
