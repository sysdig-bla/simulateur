
open Netlist_ast

module TbAssoc = Map.Make(struct
    type t = int
    let compare = compare
end)

type t = 
    {
        n_inputs : int ;
        n_outputs : int ;
        n_variables : int ;
        n_registers : int ;
        instructions : (int * int) list;
        names : string TbAssoc.t ;
    }

exception Unknown_id

let create_from_program pr =
    let n_inputs = List.length pr.p_inputs and
        n_outputs = List.length pr.p_outputs in
    let n_totvars = List.length pr.p_eqs - n_outputs in

    let names = ref TbAssoc.empty in
    let empty_field_reg = ref (n_totvars + n_outputs + n_inputs - 1) in
    let empty_field_nonreg = ref (n_outputs + n_inputs) in

    let get_id is_reg var_name =
        try
            TbAssoc.find var_name !names 
        with Not_found ->
            if is_reg then
            begin
                names := TbAssoc.add !empty_field_reg var_name !names;
                decr empty_field_reg;
                !empty_field_reg + 1
            end
            else
            begin
                names := TbAssoc.add !empty_field_nonreg var_name !names;
                incr empty_field_nonreg;
                !empty_field_nonreg - 1
            end
    in

    let rec add_io start = function
        | [] -> start
        | h::t -> names := TbAssoc.add start h !names;
                  add_io (start+1) t
    in

    let rec handle_eq deps = function
        | [] -> ()
        | h::t -> () (* TODO *)             
    in


    (* TODO *)
    { n_inputs = 0; n_outputs = 0;
      n_variables = 0; n_registers = 0;
      instructions = []; names = TbAssoc.empty }

let nb_inputs p =
    p.n_inputs

let nb_outputs p =
   p.n_outputs 

let nb_variables p =
    p.n_variables

let nb_registers p =
    p.n_registers

let get_name p id =
    try
        TbAssoc.find id p.names 
    with _ -> raise Unknown_id

let get_instructions p =
   p.instructions


