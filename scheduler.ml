
(*
 * Types pour les instructions converties (on change les
 * noms de variables en leurs indentifiants entiers)
 *)

open Netlist_ast

type iarg =
    | IAvar of int
    | IAconst of value

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


(*
 * Algo intéressant :
 * étant donné une (int * 'a) list (data) et une int list (order)
 * renvoyer la liste data réordonnée pour que les étiquettes soient dans le même 
 * ordre que dans 'order'.
 *)
let reorder data order =
    (* TODO *)
    data

let schedule_program pr =
    let proxy = Netlist_proxy.create_from_program pr in
    let nb_noeuds = Netlist_proxy.nb_identifiers proxy in
    let gr = Graph.make nb_noeuds in

    List.iter (fun (i,j) -> Graph.add_edge gr i j)
    (Netlist_proxy.get_instructions proxy);

    let order = List.rev (Graph.tsort gr) in

    let get_id = Netlist_proxy.get_id proxy in

    let translate_arg = function
        | Aconst(x) -> IAconst(x)
        | Avar(s) -> IAvar(get_id s)
    in

    let translate_expr = function
        | Earg(a) -> IEarg(translate_arg a)
        | Ereg(str) -> IEreg(get_id str)
        | Enot(arg) -> IEnot(translate_arg arg)
        | Ebinop(op,a,b) -> IEbinop(op, translate_arg a, translate_arg b)
        | Emux(a,b,c) -> IEmux(translate_arg a,translate_arg b,translate_arg c)
        | Erom(a,b,c) -> IErom(a, b, translate_arg c)
        | Eram(a,b,c,d,e,f) -> IEram(a,b,translate_arg c, translate_arg d,
                                    translate_arg e, translate_arg f)
        | Econcat(a,b) -> IEconcat(translate_arg a, translate_arg b)
        | Eslice(a,b,c) -> IEslice(a, b, translate_arg c)
        | Eselect(a,b) -> IEselect(a, translate_arg b)
    in

    let translate_eqn (name,expr) =
        (get_id name,translate_expr expr)
    in

    let translated = List.map translate_eqn pr.p_eqs in

    (* Réorganise la liste des instructions selon l'ordre défini par 'order' *)
    let reordered = reorder translated order in

    (* debug temporaire : *)
    List.iter (fun i ->
        Format.printf "[%d] : %s\n" i (Netlist_proxy.get_name proxy i))
    order;

    reordered



