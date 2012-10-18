(* 
 * Module qui sépare et renomme les différents indentifieurs
 * en fonction de leur rôle pour faciliter leur intégration dans le graphe.
 * Plus précisément :
 * - Le module gère la correspondance entre les noms de variables et les
 *   identifieurs entiers.
 *   Ces identifieurs sont séparés pour faciliter la gestion des registres.
 * - Chaque variable peut dépendre d'un certain nombre d'autres variables
 *   (de 1 à 4 pour l'opération RAM) : la fonction get_instructions
 *   simplifie ça en ne renvoyant que des couples de dépendances
 *)

open Netlist_ast ;; 

type t

(* Crée le proxy à partir de la netlist *)
val create_from_program : program -> t

(** Fonctions facilitant une traduction vers un graphe **)

(* Nombre d'entrées *)
val nb_inputs : t -> int

(* Nombre de sorties *)
val nb_outputs : t -> int 

(* Nombre de variables intermédiaires (qui ne sont pas des registres)
 * qui figurent dans la netlist *)
val nb_variables : t -> int

(* Nombre de registres *)
val nb_registers : t -> int

(* Les identifieurs sont rangés dans l'ordre ci-dessus :
 * input_1 ... input_k output_1 .. output_p variable_1 ... variableq
 * register_1 ... register_n *)

(* Identifieur d'une entrée, sortie, variable ou registre *)
val get_id : t -> string -> int

(* Vrai nom d'une entrée, sortie, variable ou registre *)
val get_name : t -> int -> string 

(* Récupère les instructions *)
(* (a,b) veut dire a dépend de b *)
val get_instructions : t -> (int * int) list
