open Netlist_ast

module TbAssoc = Map.Make(struct
    type t = string
    let compare = compare
end)

type t = 
    {
        n_inputs : int ;
        n_outputs : int ;
        n_variables : int ;
        instructions : (int * int) list;
        registers: bool array ;
        n_registers: int ;
        names : string array ;
        revnames : int TbAssoc.t ; 
    }

exception Unknown_id

let create_from_program pr =
    let n_inputs = List.length pr.p_inputs and
        n_outputs = List.length pr.p_outputs in
    let n_totvars = List.length pr.p_eqs - n_outputs in

    let names = Array.make (n_totvars + n_outputs + n_inputs) "" in
    let revnames = ref TbAssoc.empty in
    let is_register = Array.make (Array.length names) false in
    let registers_count = ref 0 in
    let empty_field = ref (n_outputs + n_inputs) in

    let set_id name id =
            names.(id) <- name;
            revnames := TbAssoc.add name id !revnames
    in

    let get_id var_name =
        try
            TbAssoc.find var_name !revnames 
        with Not_found ->
            begin
                set_id var_name !empty_field;
                incr empty_field;
                !empty_field - 1
            end
    in

    let rec add_io start = function
        | [] -> start
        | h::t -> revnames := TbAssoc.add h start !revnames;
                  names.(start) <- h;
                  add_io (start+1) t
    in

    let dependencies_of_exp expression = 
        let find_dependencies = function
        | Ereg(p) -> [Avar(p)]
        | Earg(p)
        | Enot(p)
        | Erom(_,_,p)
        | Eslice(_,_,p)
        | Eselect(_,p) -> [p]
        | Ebinop(_,p,q)
        | Econcat(p,q) -> [p; q]
        | Emux(p,q,r) -> [p; q; r]
        | Eram(_,_,p,q,r,s) -> [p; q; r; s]
        in
        let rec convert_to_id = function
            | [] -> []
            | Aconst(_)::t -> convert_to_id t
            | Avar(name)::t -> (get_id name)::(convert_to_id t)
        in
        convert_to_id (find_dependencies expression)
    in

    let is_exp_reg = function
        | Ereg(_) -> true
        | _ -> false
    in

    let rec handle_eq deps = function
        | [] -> deps
        | (name,expr)::t ->
                let id = get_id name in
                let newdeps = (List.map (fun x -> (id,x)) (dependencies_of_exp
                expr)) @ deps in
                is_register.(id) <- (is_exp_reg expr);
                incr registers_count; 
                handle_eq newdeps t
    in

    empty_field := add_io (add_io 0 pr.p_inputs) pr.p_outputs;
    let instructions = handle_eq [] pr.p_eqs in

    { n_inputs = n_inputs; n_outputs = n_outputs;
      n_variables = n_totvars; registers = is_register;
      n_registers = !registers_count; 
      instructions = instructions; names = names;
      revnames = !revnames }

let nb_inputs p =
  p.n_inputs

let nb_outputs p =
  p.n_outputs 

let nb_variables p =
  p.n_variables

let nb_registers p =
  p.n_registers

let get_id p name =
    try
        TbAssoc.find name p.revnames
    with _ -> raise Unknown_id

let get_name p id =
    try
        p.names.(id) 
    with _ -> raise Unknown_id

let get_instructions p =
   p.instructions

let is_register p id =
    try
        p.registers.(id)
    with _ -> raise Unknown_id

let nb_identifiers p =
    nb_variables p + nb_inputs p + nb_outputs p

