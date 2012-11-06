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

open Netlist_proxy
open Conversion

(** Printers.                                                               **)

let print_tape t =
  let aux c = match (curv c) with
    | [||] -> false
    | x ->  x.(0)
  in
  let () = Array.iter (fun c -> Printf.printf "%b\t" (aux c)) t in
  Printf.printf "\n"

let rec print_list = function
  | [] -> Printf.printf "\n"
  | i :: l -> Printf.printf "%d; " i; print_list l

let rec print_value v =
  Array.iter (fun b -> Printf.printf "%b; " b) v;
  Printf.printf "\n"


(** Fonctions d'interprétation.                                             **)

let evalue t = function
  | Avar i -> (curv t.(i)) 
  | Aconst v -> v

let enot v =
  Array.map (fun b -> not b) v

let rec ebinop_aux v1 v2 f =
  let n1 = Array.length v1 in
  let n2 = Array.length v2 in
  let v3 = Array.make n2 false in
  if n1 > n2
  then ebinop_aux v2 v1 f
  else begin
      for i = 0 to (n1 - 1) do
        v3.(i) <- f v1.(i) v2.(i)
      done;
      for i = n1 to (n2 - 1) do
        v3.(i) <- v2.(i)
      done;
      (* let () = print_value v1 in
      let () = print_value v2 in
      let () = print_value v3 in *)
      v3
   end

let ebinop v1 v2 = function
  | Or -> ebinop_aux v1 v2 (fun b1 b2 -> b1 || b2)
  | Xor -> ebinop_aux v1 v2 (fun b1 b2 -> (b1 || b2) && not (b1 && b2))
  | And -> ebinop_aux v1 v2 (fun b1 b2 -> b1 && b2)
  | Nand -> ebinop_aux v1 v2 (fun b1 b2 -> not (b1 && b2))

(* Sets the value of a variable, in the right field according to the
 * current_value_in_v0 flag. *)
let setv t i v =
    if !current_value_in_v0 then
        t.(i).v0 <- v
    else
        t.(i).v1 <- v

let rec fast_2 = function
  | 0 -> 1
  | n when n mod 2 = 0 -> let x = fast_2 (n/2) in x*x
  | n -> let x = fast_2 (n/2) in x*x*2

(* Converts a 1 dimensional length n*m array to a n*m matrix *)
let barray2_of_barray t n m =
  Array.init n (fun i -> Array.init m (fun j -> t.(m*i+j)))

let interpret p rdata t i1 = function
  | Earg a -> fun () ->
    let v = evalue t a in
    setv t i1 v
  | Ereg i2 -> fun () ->
    setv t i1 (if !current_value_in_v0 then t.(i2).v1 else t.(i2).v0)
  | Enot a -> fun () ->
    let v = evalue t a in
    setv t i1 (enot v)
  | Ebinop (o, a1, a2) -> fun () ->
    let v1 = evalue t a1 in
    let v2 = evalue t a2 in
    setv t i1 (ebinop v1 v2 o);
  | Emux (a1, a2, a3) -> fun () ->
    let v1 = evalue t a1 in
    if v1.(0)
    then setv t i1 (evalue t a2)
    else setv t i1 (evalue t a3)
  | Erom (l, w, a) -> 
    let size = fast_2 l in
    let r = try
        barray2_of_barray
          (Memdata.get_data rdata (Netlist_proxy.get_name p i1)) size w
      with Memdata.Not_found -> Array.init size (fun _ -> Array.make w false) in
    fun () -> setv t i1 r.(int_of_argument t a)
  | Eram (l, w, ra, we, wa, d) -> 
    let size = fast_2 l in
    let r = try
        barray2_of_barray
          (Memdata.get_data rdata (Netlist_proxy.get_name p i1)) size w
      with Memdata.Not_found -> Array.init size (fun _ -> Array.make w false) in
    fun () -> begin
      setv t i1 r.(int_of_argument t ra);
      if bool_of_argument t we
        then r.(int_of_argument t wa) <- evalue t d
    end
  | Econcat (a1, a2) -> fun () ->
    let v1 = evalue t a1 in
    let v2 = evalue t a2 in
    setv t i1 (Array.append v1 v2)
  | Eslice (s, l, a) -> fun () ->
    let v = evalue t a in
    setv t i1 (Array.sub v s l)
  | Eselect (i, a) -> fun () ->
    let v = evalue t a in
    setv t i1 [|v.(i)|]


(** Fonctions du ruban                                                      **)

let make_tape n =
    Array.init n (fun _ -> {v0 = [|false|]; v1 = [|false|]; a = fun () -> ()})

let init_case p rdata t eq =
  let (i, exp) = eq in
  let a = interpret p rdata t i exp in
  t.(i).a <- a

let rec init_tape p rdata t eqs = match eqs with
  | [] -> () 
  | eq :: tl -> let () = init_case p rdata t eq in
    init_tape p rdata t tl

let rec execute_tape t schedule = match schedule with
  | [] -> () 
  | i :: tl -> let () = t.(i).a () in
    (*let () = print_tape t in*)
    execute_tape t tl

let setup_inputs t raw_inputs nb_inputs =
    Array.blit (Array.map (fun x -> {v0 = x; v1 = x; a = (fun () -> ())}) raw_inputs) 0 t 0 nb_inputs

let inputs_cycle t nb_inputs inputs cycle =
  Array.blit inputs (cycle*nb_inputs) t 0 nb_inputs

let outputs_cycle t ofs_outputs nb_outputs =
  let o = Array.sub t ofs_outputs nb_outputs in
  let result =  Array.to_list (Array.map (fun a -> (curv a)) o) in
  result

let simulate p p_eqs rdata get_input put_output is_input_available debug_mode =
  let nb_cases = nb_identifiers p in
  let schedule = Scheduler.schedule_program p in
  let nb_inputs = nb_inputs p in
  let nb_outputs = nb_outputs p in

  let eqs = convert_equations p p_eqs in
  let t = make_tape nb_cases in
  let () = init_tape p rdata t eqs in
  
  (*let () = print_list schedule in*)
  while is_input_available () do
      let () = setup_inputs t (get_input ()) nb_inputs in
      let () = execute_tape t schedule in
      put_output (outputs_cycle t nb_inputs nb_outputs);
  
      if debug_mode then
      begin    
          for i = 0 to nb_cases - 1 do
              Printf.printf "%s : %b\n%!" (Netlist_proxy.get_name p i) (if
                  !current_value_in_v0 then t.(i).v0.(0) else t.(i).v1.(0))
          done
      end;

      current_value_in_v0 := not !current_value_in_v0; 
  done

(* 2e version (interprete sans stocker les fonctions dans un tableau) *)

let interpret2 t i1 = function
  | Earg a ->
    let v = evalue t a in
    setv t i1 v
  | Ereg i2 ->
    setv t i1 (if !current_value_in_v0 then t.(i2).v1 else t.(i2).v0)
  | Enot a ->
    let v = evalue t a in
    setv t i1 (enot v)
  | Ebinop (o, a1, a2) ->
    let v1 = evalue t a1 in
    let v2 = evalue t a2 in
    setv t i1 (ebinop v1 v2 o);
  | Emux (a1, a2, a3) ->
    let v1 = evalue t a1 in
    if v1.(0)
    then setv t i1 (evalue t a2)
    else setv t i1 (evalue t a3)
  | Erom _ 
  | Eram _ -> assert false
  | Econcat (a1, a2) ->
    let v1 = evalue t a1 in
    let v2 = evalue t a2 in
    setv t i1 (Array.append v1 v2)
  | Eslice (s, l, a) ->
    let v = evalue t a in
    setv t i1 (Array.sub v s l)
  | Eselect (i, a) ->
    let v = evalue t a in
    setv t i1 [|v.(i)|]


let simulate2 p p_eqs get_input put_output is_input_available debug_mode =
  let nb_cases = nb_identifiers p in
  let schedule = Scheduler.schedule_program p in
  let nb_inputs = nb_inputs p in
  let nb_outputs = nb_outputs p in

  let eqs = convert_equations p p_eqs in

  let sorted_eqs = List.fold_right (fun i -> fun l ->
    try (i,List.assoc i eqs)::l with _ -> l) schedule [] in

  let t = make_tape nb_cases in
  
  (*let () = print_list schedule in*)
  while is_input_available () do
      let () = setup_inputs t (get_input ()) nb_inputs in
      let () = List.iter (fun (i,exp)->interpret2 t i exp) sorted_eqs in
      put_output (outputs_cycle t nb_inputs nb_outputs);
  
      if debug_mode then
      begin    
          for i = 0 to nb_cases - 1 do
              Printf.printf "%s : %b\n%!" (Netlist_proxy.get_name p i) (if
                  !current_value_in_v0 then t.(i).v0.(0) else t.(i).v1.(0))
          done
      end;

      current_value_in_v0 := not !current_value_in_v0; 
  done

