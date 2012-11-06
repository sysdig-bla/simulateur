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
    then i := 1 + 2 * !i
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
