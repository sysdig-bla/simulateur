
(* Module fournissant les données d'initialisation des mémoires RAM et
ROM *)

exception Not_found

(* Table associative (identifieur,données) *)
type t

val empty : t

(* Charge les données d'initialisation des ROM et RAM à partir d'un
fichier. Lève Invalid_arg(qqch) s'il y a un problème pendant la lecture. *)
val from_file : string -> t

(* Récupère les données d'initialisation pour un identifiant donné
 * raise Not_found si l'identifiant n'est associé à aucune valeur *)
val get_data : t -> string -> bool array

