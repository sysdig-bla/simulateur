open Netlist_ast

(* The idea is to build an explicit graph to be able to simplify it
 * by uniting common expressions and lines
 * (getting rid of Concat/Split/Select first) *)

type id2 = int

type bin2 = binop

type addr = {
  ad_i:int;
  loc:node array;
}

and node = {
  id:id2;
  mutable eq:exp;
  mutable d:int;
  mutable mark:int;
}

and exp =
  | New
  | Input of string
  | Const of bool
  | Node of node
  | Not of node
  | Bin of bin2*node*node
  | Mux of node*node*node
  | Reg of node
  | Rom of addr*bool array
  | Ram of addr*node*addr*node*bool array
  (*wa,we,wa,d*)

  (* Somewhat complicated, we could use less information *)
type graph = {
  range:node array list;(* output nodes, input nodes and registers *)
  named:node array Smap.t; (* all specified nodes *)
  address:addr array; (* For big word size memories, since we split everything
  to one line units we have to keep a common data structure *)
  in_order:ident array;
  out_order:ident array;
}

exception Cyclic of string

exception Incomplete

exception Netls_err of string

let ntlserr s = raise (Netls_err s)

let print = Printf.printf "%d\n"

(* Constants *)
let c0 = {id= -2;eq=Const false;d= -1;mark=0}
let c1 = {id= -1;eq=Const true;d= -1;mark=0}

let all_nodes = ref []
let free = ref (-1)
let free2 = ref (-1)

(* We will need a lot of nodes and structural
 * comparison between nodes is out of question
 * since we have cyclic structures *)
let fresh =
  fun () -> incr free; !free

let fresh2 =
  fun () -> incr free2; !free2

(* Reset should only be done AFTER reduction *)
let reset () =
  all_nodes := [];
  free := -1;
  free2 := -1

let new_addr ra = {ad_i=fresh2(); loc=ra}

let mk_node eq d =
  let n = {id=fresh();eq=eq;d=d;mark=0} in
  all_nodes := n::!all_nodes;
  n

let new_node _ = mk_node New 0

let (==) n m = n.id=m.id

(* Create a simple graph out of a netlist 
 * (simple as in close to the original data) *)
let mk_graph p =
  let wrong_size id = ntlserr (id^" has incompatible size") in

  (* Reorganize equations for quick access from left member *)
  let eqset =
    List.fold_left (fun s (id,exp) -> Smap.add id exp s) Smap.empty p.p_eqs in

  (* Used nodes (* Most dead nodes i.e. not necessary to
   * compute outputs, are forgotten in our structure *) *)
  let active:(ident,node array) Hashtbl.t = Hashtbl.create 127 in

  (* The way to avoid infinite loop in a cycle through a register or ROM/RAM
   * stop scanning at Reg/RAM/ROM and come again when they are "secured" *)
  let queue = ref [] in

  (* Nodes we can reach all the graph from, i.e. all ALIVE registers/R[OA]M
   * (there is some redundancy) *)
  let reach = ref [] in

  (* Keep all "address" structures in one place *)
  let addr = ref [] in
  let sz = Array.length in
  let size id =
    try
      Env.find id p.p_vars
    with
      | Not_found -> ntlserr (id^" undeclared")
  in

  (* Create nodes as they are needed *)
  let find x =
    try
      Hashtbl.find active x
    with
      | Not_found -> 
        let n = Array.init (size x) new_node in
        Hashtbl.add active x n;
        n
  in

  (* Some inputs may be dead nodes but for coherence they are needed 
   * (the netlist states they are inputs,
   * if you don't ask for them that's just weird) *)
  let inputs = List.map
    (fun x -> let n = find x in
      Array.iter 
	      (fun n ->
	        n.eq <- Input x;
          n.mark <- 1) n;
        n
    ) p.p_inputs in

      (* Just draw the graph using the equations as edges... *)
  let make_const b =
    Array.init (sz b) (fun i -> if b.(i) then c1 else c0)
  in
  let find_arg = function
    | Avar a -> queue := a::(!queue); find a
    | Aconst b -> make_const b
  in
  let rec add_node id =
    let n = find id in
    if n.(0).mark = -1
    then raise (Cyclic id)
    else if n.(0).mark=1
    then n
    else (* marked 0 *) (
      n.(0).mark <- -1;
      begin match Smap.find id eqset with
        | Earg a ->
          let a = get_arg a in
          if sz a <> sz n then wrong_size id;
          Array.iteri
            (fun i n -> n.eq <- Node a.(i); n.d <- a.(i).d+1) n

        | Ereg a ->
          queue := a:: !queue;
          let a = find a in
          if sz a <> sz n then wrong_size id;
          Array.iteri
          (fun i n -> n.eq <- Reg a.(i); n.d <- 0) n

        | Enot a ->
          let a = get_arg a in
          if sz a <> sz n then wrong_size id;
          Array.iteri
            (fun i n -> n.eq <- Not a.(i); n.d<-a.(i).d+1) n

        | Ebinop (o,a,b) ->
          let a = get_arg a in
          let b = get_arg b in
          if sz a <> sz n || sz b <> sz n then wrong_size id;
          Array.iteri
            (fun i n ->
              n.eq <- Bin(o,a.(i),b.(i)); n.d<-max a.(i).d b.(i).d+1) n

        | Emux (a,b,c) ->
          let k = get_arg a in
          let b = get_arg b in
          let c = get_arg c in
          if sz b <> sz n || sz c <> sz n then wrong_size id;
          if sz k <> 1 then ntlserr (id^" mux has bad select line");
          Array.iteri
            (fun i n ->
              n.eq <- Mux(k.(0),b.(i),c.(i));
              n.d<-max (max k.(0).d b.(i).d) c.(i).d +1) n

        (* We need to fill the memory that early because we won't have
         * easy access to variable names very soon *)
        | Erom (addrs,ws,a) ->
          let ra = find_arg a in
          if sz ra <> addrs then ntlserr (id^" rom has bad address size");
          if sz n <> ws then wrong_size id;
          let ra = new_addr ra in
          addr := ra::!addr;
          let m = Memo.get id ws (1 lsl addrs) in
          Array.iteri
            (fun i n -> n.eq <- Rom (ra,m.(i)); n.d<-0) n

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
          let ra = new_addr ra in
          let wa = new_addr wa in
          addr := wa::ra::!addr;
          let m = Memo.get id ws (1 lsl addrs) in
          Array.iteri
            (fun i n ->
              n.eq <- Ram (ra,we.(0),wa,dat.(i),m.(i));
              n.d<-0) n

        (* Representing only single lines enables us to first discard
         * this information at the cost of time and space
         * for the topoligical sort.
         * Doing this later is more complicated but we save resources
         * while sorting.
         * Given the size of the circuit to be simulated,
         * overhead is negligible *)
        | Econcat (a,b) ->
          let a = get_arg a in
          let b = get_arg b in
          let d = max a.(0).d b.(0).d +1 in
          if sz a + sz b <> sz n then wrong_size id;
          for i = 0 to sz a-1 do
            n.(i).eq <- Node a.(i);
            n.(i).d <- d;
          done;
          for i = 0 to sz b-1 do
            n.(sz a+i).eq <- Node b.(i);
            n.(sz a+i).d <-d;
          done

        | Eslice (i,j,a) ->
          let a = get_arg a in
          if i<0 || i>=sz a || j<i || j>=sz a
            then invalid_arg "Illegal slice";
          if sz n <> j-i+1 then wrong_size id;
          Array.iteri (fun k n -> n.eq <- Node a.(i+k);n.d<-a.(0).d+1) n

        | Eselect (i,a) ->
          let a = get_arg a in
          if i<0 || i>= sz a then invalid_arg "Illegal select";
          if sz n <> 1 then wrong_size id;
          n.(0).eq <- Node a.(i);
          n.(0).d <- a.(i).d+1

      end; n.(0).mark <- 1; n)
  and get_arg = function
    | Avar id -> add_node id
    | Aconst b -> make_const b
  in

  (* As I said, begin from outputs to only build needed nodes *)
  let g = List.map add_node p.p_outputs in

  (* Completing the search when it was stopped at memory units
   * Reg/RAM/ROM *)
  while !queue <> [] do
    let h = List.hd !queue in
    queue := List.tl !queue;
    let k = find h in
    if k.(0).mark = 0
      then reach := k::!reach;
    ignore (add_node h);
  done;
  {
    range=g @ !reach @ inputs;
    named=Hashtbl.fold Smap.add active Smap.empty;
    address=Array.of_list (List.rev !addr);
    in_order=Array.of_list p.p_inputs;
    out_order=Array.of_list p.p_outputs;
  }

let set_marks g x =
  List.iter
    (fun n -> n.mark <- x)
    !all_nodes

(* Using lists of lists, grouping nodes at the same depth *)
let toposort g =
  set_marks g 0;
  let rec add (d,x) = function
    | h::t when d>0 -> h::add (d-1,x) t
    | h::t when d=0 -> (x::h)::t
    | [] -> add (d,x) [[]]
    | l -> l
  in
  let add x = add (x.d,x) in
  let rec sort acc x =
    if x.mark=1 then acc
    else begin
      x.mark <- 1;
      match x.eq with
        | Input _ -> add x acc

        | Const _ -> acc

        | Node m | Reg m | Not m ->
          sort (add x acc) m

        | Bin (_,m1,m2) ->
          sort (sort (add x acc) m1) m2

        | Mux (a,b,c) ->
            sort (sort (sort (add x acc) a) b) c

        | Rom (a,_) ->
            Array.fold_left sort (add x acc) a.loc

        | Ram (a,n,b,m,_) ->
            let l = Array.fold_left sort (add x acc) a.loc in
            let l = Array.fold_left sort l b.loc in
            sort (sort l n) m

        | New -> raise Incomplete
    end
  in
  List.fold_left sort [[]] (List.flatten (List.map Array.to_list g.range))

let string_of_bin2 = function
  | Or -> "Or"
  | And -> "And"
  | Xor -> "Xor"
  | Nand -> "Nand"

let stringofbarray b =
  let s = String.make (Array.length b) '0' in
  for i = 0 to Array.length b -1 do
    if b.(i) then s.[i] <- '1';
  done;
  s


let print_exp h = function
    | New -> Format.fprintf h "new"
    | Input s -> Format.fprintf h "input %s" s
    | Const b -> Format.fprintf h "%d" (if b then 1 else 0)
    | Node n -> Format.fprintf h "%d" n.id
    | Reg n -> Format.fprintf h "Reg %d" n.id
    | Not n -> Format.fprintf h "Not %d" n.id
    | Bin (o,m,n) -> Format.fprintf h "%s %d %d" (string_of_bin2 o) m.id n.id
    | Mux (a,b,c) -> Format.fprintf h "Mux %d %d %d" a.id b.id c.id
    | Rom (n,mem) -> Format.fprintf h "Rom %d...%d:%s" n.loc.(0).id
    n.loc.(Array.length n.loc-1).id (stringofbarray mem)
    | Ram (ra,we,wa,d,mem) ->
      let ras = Array.length ra.loc in
      let was = Array.length wa.loc in
      Format.fprintf h "Ram ra:%d...%d we:%d wa:%d...%d d:%d:%s"
	ra.loc.(0).id ra.loc.(ras-1).id we.id wa.loc.(0).id wa.loc.(was-1).id d.id
  (stringofbarray mem)

let print_eq h n =
  Format.fprintf h "%d = %a" n.id print_exp n.eq

let print_var h id n =
  Format.fprintf h "%s@\n        @[" id;
  for i = 0 to Array.length n - 1 do
    print_eq h n.(i);
    if i<Array.length n - 1 then Format.fprintf h "@\n";
  done;
  Format.fprintf h "@]@\n"

let print_graph ch g =
  Smap.iter (print_var ch) g.named

let print_sgraph ch l =
  List.iter
    (fun l ->
      List.iter (fun x -> Format.fprintf ch "%d %a @\n" x.d print_eq x;) l;
      Format.fprintf ch "@\n";) l

let nodenum l =
  List.fold_left (fun s l -> s+List.length l) 0 l
