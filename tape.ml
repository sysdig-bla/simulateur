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
open Netlist_ast


(* Case : value, assess                                                      *)
(* IErom : addr size, word size, read addr                                    *)
(* IEram : addr size, word size, read addr, write enable, write addr, data    *)


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

let int_of_argument t = function
  | IAvar i -> int_of_value t.(i) 
  | IAconst v -> int_of_value v

let bool_of_argument t = function
  | IAvar i -> t.(i).(0)
  | IAconst v -> v.(0)


(** Conversion des équations.                                               **)

let convert_value v = match v with
  | VBit b -> [|b|] 
  | VBitArray a -> a

(** Fonctions d'interprétation.                                             **)

let evalue t = function
  | Avar i -> t.(i).v 
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

let interpret t rom ram i1 = function
  | IEarg a ->
    let v = evalue t a in
    t.(i1) <- v
  | IEreg i2 ->
    t.(i1) <- t.(i2)
  | IEnot a ->
    let v = evalue t a in
    t.(i1) <- enot v
  | IEbinop (o, a1, a2) ->
    let v1 = evalue t a1 in
    let v2 = evalue t a2 in
    t.(i1) <- ebinop v1 v2 o
  | IEmux (a1, a2, a3) ->
    let v1 = evalue t a1 in
    if v1.(0)
    then t.(i1) <- evalue t a2
    else t.(i1) <- evalue t a3
  | IErom (l, w, a) -> 
    let i2 = int_of_argument t a in
    t.(i1) <- read_memory rom i2 l w
  | IEram (l, w, a1, a2, a3, a4) ->
    let i2 = int_of_argument t a1 in
    let i3 = int_of_argument t a3 in
    let v = evalue t a4 in
    let () = t.(i1) <- read_memory ram i2 l w in
    if bool_of_argument t a2
    then write_memory ram v i3 l w
  | IEconcat (a1, a2) ->
    let v1 = evalue t a1 in
    let v2 = evalue t a2 in
    t.(i1) <- Array.append v1 v2
  | IEslice (s, l, a) ->
    let v = evalue t a in
    t.(i1) <- Array.sub v s l
  | IEselect (i, a) ->
    let v = evalue t a in
    t.(i1) <- [|v.(i)|]


(** Fonctions du ruban                                                      **)

let make_tape n =
  Array.make n [||]

let interpret_netlist rom ram t = function
    | [] -> ()
    | (i, exp)::tl ->
        interpret t rom ram i exp;
        interpret_netlist rom ram t tl

let inputs_cycle t nb_inputs inputs =
  Array.blit t 0 inputs 0 nb_inputs

let outputs_cycle t ofs_outputs nb_outputs =
  let o = Array.sub t ofs_outputs nb_outputs in
  Array.to_list (Array.map (fun a -> a.(0)) o)

let simulate prog nb_cycles inputs =
  let (p,scheduled) = schedule_program prog in
  let nb_inputs = nb_inputs p in
  let nb_outputs = nb_outputs p in
  let nb_cases = nb_identifiers p in

  let t = make_tape nb_cases in
  let rom = Array.init 1024 (fun _ -> Array.make 32 false) in
  let ram = Array.init 8192 (fun _ -> Array.make 32 false) in
  let outputs = ref [] in
  for i = 0 to (nb_cycles - 1) do
    let () = inputs_cycle t nb_inputs inputs in
    interpret_netlist rom ram t scheduled in
    outputs := !outputs @ (outputs_cycle t nb_inputs nb_outputs)
  done;
  !outputs

