(* Graphe à étiquettes de type int, taille fixe *)
type graph

exception Cycle

(* make n crée un graphe à n noeuds sans arêtes,
 * numérotés de 0 à n-1*)
val make : int -> graph

(* add_edge g i j crée l'arête i -> j *)
val add_edge : graph -> int -> int -> unit

(* Teste la présence de cycle dans g *)
val cyclic : graph -> bool

(* tri topologique sur un graphe orienté acyclique
 * Exception Cycle si on fait n'importe quoi *)
val tsort : graph -> int list
