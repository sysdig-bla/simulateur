open Netlist_ast

type id2 = int

type bin2 = binop

type node = {
  id:id2;
  mutable mark:int;
  mutable eq:exp;
}

and exp =
  | New
  | Input
  | Const of bool
  | Node of node
  | Reg of node
  | Not of node
  | Bin of bin2*node*node
  | Mux of node*node*node
  | Rom of node array
  | Ram of node array*node*node array*node

type graph = node list*(ident,node array) Hashtbl.t
  (* output nodes, all accessible nodes *)

exception Cyclic of string

exception Netls_err of string

let ntlserr s = raise (Netls_err s)

let fresh =
  let free = ref (-3) in
  fun () -> incr free; !free

let new_node _ = {id=fresh();mark=0;eq=New}

let mk_graph p =
  let wrong_size id = ntlserr (id^" has incompatible size") in
  let eqset =
    List.fold_left (fun s (id,exp) -> Smap.add id exp s) Smap.empty p.p_eqs in
  let active:(ident,node array) Hashtbl.t = Hashtbl.create 127 in
  let queue = ref [] in
  let sz = Array.length in
  let size id =
    try
      Env.find id p.p_vars
    with
      | Not_found -> ntlserr (id^" undeclared")
  in
  let find x =
    try
      Hashtbl.find active x
    with
      | Not_found -> 
        let n = Array.init (size x) new_node in
        Hashtbl.add active x n;
        n
  in
  let c0 = new_node () in
  let c1 = new_node () in
  c0.eq <- Const false;
  c1.eq <- Const true;
  List.iter
    (fun x ->
      Array.iter 
	(fun n ->
	  n.eq <- Input;
	  n.mark <- 2) (find x)) p.p_inputs;
  let make_const b =
    Array.init (sz b) (fun i -> if b.(i) then c1 else c0)
  in
  let find_arg = function
    | Avar a -> queue := a::(!queue); find a
    | Aconst b -> make_const b
  in
  let rec add_node id =
    let n = find id in
    if n.(0).mark=1
    then raise (Cyclic id)
    else if n.(0).mark=2
    then n
    else (* marked 0 *) (
      n.(0).mark <- 1;
      begin match Smap.find id eqset with
	| Earg a ->
          let a = get_arg a in
          if sz a <> sz n then wrong_size id;
          Array.iteri (fun i n -> n.eq <- Node a.(i)) n

	| Ereg a ->
          let a = find a in
          if sz a <> sz n then wrong_size id;
          Array.iteri (fun i n -> n.eq <- Reg a.(i)) n

	| Enot a ->
          let a = get_arg a in
          if sz a <> sz n then wrong_size id;
          Array.iteri (fun i n -> n.eq <- Not a.(i)) n

	| Ebinop (o,a,b) ->
          let a = get_arg a in
          let b = get_arg b in
          if sz a <> sz n || sz b <> sz n then wrong_size id;
          Array.iteri (fun i n -> n.eq <- Bin(o,a.(i),b.(i))) n

	| Emux (a,b,c) ->
          let k = get_arg a in
          let b = get_arg b in
          let c = get_arg c in
          if sz b <> sz n || sz c <> sz n then wrong_size id;
          if sz k <> 1 then ntlserr (id^" mux has bad select line");
          Array.iteri (fun i n -> n.eq <- Mux(k.(0),b.(i),c.(i))) n

	| Erom (addrs,ws,a) ->
          let ra = find_arg a in
          if sz ra <> addrs then ntlserr (id^" rom has bad address size");
          if sz n <> ws then wrong_size id;
          Array.iter (fun n -> n.eq <- Rom ra) n

	| Eram (addrs,ws,ra_,we_,wa_,dat_) ->
          let ra = find_arg ra_ in
          let we = find_arg we_ in
          let wa = find_arg wa_ in
          let dat = find_arg dat_ in
          if sz ra <> addrs then ntlserr (id^" ram has bad address size");
          if sz we <> 1 then ntlserr (id^" ram has bad write enable size");
          if sz wa <> addrs then ntlserr (id^" ram has bad address size");
          if sz dat <> ws then ntlserr (id^" ram has bad data size");
          if sz n <> ws then wrong_size id;
          Array.iteri (fun i n -> n.eq <- Ram (ra,we.(0),wa,dat.(i))) n

	| Econcat (a,b) ->
          let a = get_arg a in
          let b = get_arg b in
          if sz a + sz b <> sz n then wrong_size id;
          for i = 0 to sz a-1 do
            n.(i).eq <- Node a.(i)
          done;
          for i = 0 to sz b-1 do
            n.(sz a+i).eq <- Node b.(i)
          done

	| Eslice (i,j,a) ->
          let a = get_arg a in
          if i<0 || i>=sz a || j<i || j>=sz a then invalid_arg "Illegal slice";
          if sz n <> j-i+1 then wrong_size id;
          Array.iteri (fun k n -> n.eq <- Node a.(i+k)) n

	| Eselect (i,a) ->
          let a = get_arg a in
          if i<0 || i>= sz a then invalid_arg "Illegal select";
          if sz n <> 1 then wrong_size id;
          n.(0).eq <- Node a.(i)

      end; n.(0).mark <- 2; n)
  and get_arg = function
    | Avar id -> add_node id
    | Aconst b -> make_const b
  in
  let g = List.map (fun id -> add_node id) p.p_outputs in
  while !queue <> [] do
    let h =List.hd !queue in
    queue := List.tl !queue;
    ignore (add_node h);
  done;
  g,active

let string_of_bin2 = function
  | Or -> "Or"
  | And -> "And"
  | Xor -> "Xor"
  | Nand -> "Nand" 

let print_exp h = function
    | New -> Format.fprintf h "new"
    | Input -> Format.fprintf h "input"
    | Const b -> Format.fprintf h "%d" (if b then 1 else 0)
    | Node n -> Format.fprintf h "%d" n.id
    | Reg n -> Format.fprintf h "Reg %d" n.id
    | Not n -> Format.fprintf h "Not %d" n.id
    | Bin (o,m,n) -> Format.fprintf h "%s %d %d" (string_of_bin2 o) m.id n.id
    | Mux (a,b,c) -> Format.fprintf h "Mux %d %d %d" a.id b.id c.id
    | Rom n -> Format.fprintf h "Rom %d...%d" n.(0).id n.(Array.length n-1).id
    | Ram (ra,we,wa,d) ->
      let ras = Array.length ra in
      let was = Array.length wa in
      Format.fprintf h "Ram ra:%d...%d we:%d wa:%d...%d d:%d"
	ra.(0).id ra.(ras-1).id we.id wa.(0).id wa.(was-1).id d.id

let print_eq h id n =
  Format.fprintf h "%s :@\n  @[" id;
  for i = 0 to Array.length n - 1 do
    Format.fprintf h "%d = %a" n.(i).id print_exp n.(i).eq;
    if i<Array.length n - 1 then Format.fprintf h "@\n";
  done;
  Format.fprintf h "@]@\n"


let print_graph ch (g,h) =
  Hashtbl.iter
    (fun s n ->
      Array.iter (fun n -> n.mark <- 0) n;
      print_eq ch s n)
    h

(*
let () =
  print_graph Format.std_formatter (mk_graph (Netlist.read_file Sys.argv.(1)))
*)
