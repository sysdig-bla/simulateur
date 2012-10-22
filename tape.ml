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


(** Types.                                                                  **)

type identifier = int

type value = bool array

type operation =
  | Or
  | Xor
  | And
  | Nand
  
type argument =
  | Avar of identifier
  | Aconst of value

type expression =
  | Earg of argument
  | Ereg of identifier
  | Enot of argument
  | Ebinop of operation * argument * argument
  | Emux of argument * argument * argument
  | Erom of int * int * argument
  | Eram of int * int * argument * argument * argument * argument
  | Econcat of argument * argument
  | Eslice of int * int * argument
  | Eselect of int * argument

type equation = identifier * expression

type case =
  { mutable v0: value;
    mutable v1: value;
    mutable a: unit -> unit }

type tape = case array

(* Case : value, assess                                                      *)
(* Erom : addr size, word size, read addr                                    *)
(* Eram : addr size, word size, read addr, write enable, write addr, data    *)


(** Conversion de types.                                                    **)

let int_of_value v =
  let n = Array.length v in
  let i = ref 0 in
  for j = (n - 1) downto 0 do
    if v.(j)
    then i := j + 2 * !i
    else i := 2 * !i
  done;
  !i

  (* This flags tells wether the current value is stored
   * in v0 or v1.
   * At each cycle, we flip the flag, so that the values of the previous
   * cycle are still stored in the array. *)
let current_value_in_v0 = ref false 

  (* Curv : current value, according to the flag *)
let curv c =
    if !current_value_in_v0 then c.v0 else c.v1

let int_of_argument t = function
  | Avar i -> int_of_value (curv t.(i)) 
  | Aconst v -> int_of_value v

let bool_of_argument t = function
  | Avar i -> (curv t.(i)).(0)
  | Aconst v -> v.(0)


(** Conversion des équations.                                               **)

let convert_value v = match v with
  | Netlist_ast.VBit b -> [|b|] 
  | Netlist_ast.VBitArray a -> a

let convert_argument p a = match a with
  | Netlist_ast.Avar s -> Avar (get_id p s)
  | Netlist_ast.Aconst v -> Aconst (convert_value v)

let convert_operation o = match o with
  | Netlist_ast.Or -> Or
  | Netlist_ast.Xor -> Xor
  | Netlist_ast.And -> And
  | Netlist_ast.Nand -> Nand

let convert_expression p exp = match exp with
  | Netlist_ast.Earg a -> Earg (convert_argument p a)
  | Netlist_ast.Ereg s -> Ereg (get_id p s)
  | Netlist_ast.Enot a -> Enot (convert_argument p a)
  | Netlist_ast.Ebinop (o, a1, a2) ->
    Ebinop (convert_operation o, convert_argument p a1, convert_argument p a2)
  | Netlist_ast.Emux (a1, a2, a3) ->
    Emux (convert_argument p a1, convert_argument p a2, convert_argument p a3)
  | Netlist_ast.Erom (i1, i2, a) -> Erom (i1, i2, convert_argument p a)
  | Netlist_ast.Eram (i1, i2, a1, a2, a3, a4) ->
    Eram (i1, i2, convert_argument p a1, convert_argument p a2,
    convert_argument p a3, convert_argument p a4)
  | Netlist_ast.Econcat (a1, a2) ->
    Econcat (convert_argument p a1, convert_argument p a2)
  | Netlist_ast.Eslice (i1, i2, a) ->
    Eslice (i1, i2, convert_argument p a)
  | Netlist_ast.Eselect (i, a) -> Eselect (i, convert_argument p a) 
  
let convert_equation p eq =
  let (i1, exp1) = eq in
  let i2 = get_id p i1 in
  let exp2 = convert_expression p exp1 in
  (i2, exp2)

let rec convert_equations p eqs = match eqs with
  | [] -> []
  | eq :: tl -> (convert_equation p eq) :: (convert_equations p tl)


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

let rec read_memory r i l w = match w / l with
  | 0 -> Array.sub r.(i) 0 w
  | _ -> Array.append r.(i) (read_memory r (i + 1) l (w - l))

let rec write_memory r v1 i l w = match w / l with
  | 0 -> Array.blit v1 0 r.(i) 0 w
  | _ -> Array.blit v1 0 r.(i) 0 l;
    let v2 = Array.sub v1 l ((Array.length v1) - l - 1) in
    write_memory r v2 (i + 1) l (w - l)

(* Sets the value of a variable, in the right field according to the
 * current_value_in_v0 flag. *)
let setv t i v =
    if !current_value_in_v0 then
        t.(i).v0 <- v
    else
        t.(i).v1 <- v

let interpret t rom ram i1 = function
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
  | Erom (l, w, a) -> fun () -> 
    let i2 = int_of_argument t a in
    setv t i1 (read_memory rom i2 l w)
  | Eram (l, w, a1, a2, a3, a4) -> fun () ->
    let i2 = int_of_argument t a1 in
    let i3 = int_of_argument t a3 in
    let v = evalue t a4 in
    let () = setv t i1 (read_memory ram i2 l w) in
    if bool_of_argument t a2
    then write_memory ram v i3 l w
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

let init_case rom ram t eq =
  let (i, exp) = eq in
  let a = interpret t rom ram i exp in
  t.(i).a <- a

let rec init_tape t rom ram eqs = match eqs with
  | [] -> () 
  | eq :: tl -> let () = init_case t rom ram eq in
    init_tape t rom ram tl

let rec execute_tape t schedule = match schedule with
  | [] -> () 
  | i :: tl -> let () = t.(i).a () in
    (*let () = print_tape t in*)
    execute_tape t tl

let setup_inputs t raw_inputs nb_inputs =
    Array.blit (Array.map (fun x -> {v0 = x; v1 = x; a = (fun y -> y)}) raw_inputs) 0 t 0 nb_inputs

let inputs_cycle t nb_inputs inputs cycle =
  Array.blit inputs (cycle*nb_inputs) t 0 nb_inputs

let outputs_cycle t ofs_outputs nb_outputs =
  let o = Array.sub t ofs_outputs nb_outputs in
  let result =  Array.to_list (Array.map (fun a -> (curv a)) o) in
  result

let simulate p p_eqs get_input put_output is_input_available debug_mode =
  let nb_cases = nb_identifiers p in
  let schedule = Scheduler.schedule_program p in
  let nb_inputs = nb_inputs p in
  let nb_outputs = nb_outputs p in

  let eqs = convert_equations p p_eqs in
  let t = make_tape nb_cases in
  let rom = Array.init 1024 (fun _ -> Array.make 32 false) in
  let ram = Array.init 8192 (fun _ -> Array.make 32 false) in
  let () = init_tape rom ram t eqs in
  
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
