type ident = string

module Env = struct
  include Map.Make(struct
    type t = ident
    let compare = compare
  end)

  let of_list l =
    List.fold_left (fun env (x, ty) -> add x ty env) empty l
end

type ty = TBit | TBitArray of int
type value = VBit of bool | VBitArray of bool array

type binop = Or | Xor | And | Nand

type arg =
    | Avar of ident
    | Aconst of value

type exp =
    | Earg of arg
    | Ereg of ident
    | Enot of arg
    | Ebinop of binop * arg * arg
    | Emux of arg * arg * arg
    | Erom of int * int * arg
      (*addr size, word size, read addr*)
    | Eram of int * int * arg * arg * arg * arg
      (*addr size, word size, read addr, write enable, write addr, data*)
    | Econcat of arg * arg
    | Eslice of int * int * arg
    | Eselect of int * arg

type equation = ident * exp

type program =
    { p_eqs : equation list;
      p_inputs : ident list;
      p_outputs : ident list;
      p_vars : ty Env.t; }
