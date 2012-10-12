
(* Module représentant une netlist *)

(* N'hésitez pas à rajouter les fonctions qui vous paraissent utiles *)
(* ou à en supprimer si vous pensez qu'elles doivent êtres ailleurs. *)

module Netlist = sig

    type t

    (* Charge une netlist décrite dans le fichier filename *)
    val load_from_file : string -> t

    (** Fonctions facilitant une traduction vers un graphe **)

    (* Récupère toutes les variables qui figurent dans la netlist *)
    val get_variables : unit -> int list

    (* Récupère les instructions *)
    (* (a,b) veut dire a dépend de b *)
    val get_instructions : unit -> (int * int) list

    (** Fonctions utiles pour la simulation **)
    
    (* TODO *)

end

