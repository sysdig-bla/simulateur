open Netlist_ast

type id2 = int

type bin2 = binop

type node = {
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
  | Rom of node array
  | Ram of node array*node*node array*node
  (*wa,we,wa,d*)

type graph = {
  output:node array list;
  reach:node array list;
  named:node array Smap.t;
}
  (* output nodes, reachable nodes, all specified nodes *)

exception Cyclic of string

exception Incomplete

exception Netls_err of string

let ntlserr s = raise (Netls_err s)

let c0 = {id= -1;eq=Const false;d= -1;mark=0}
let c1 = {id= -2;eq=Const true;d= -1;mark=0}

let all_nodes = ref []
let free = ref (-1)

let fresh =
  fun () -> incr free; !free

let reset () =
  all_nodes := [];
  free := -1

let mk_node eq d =
  let n = {id=fresh();eq=eq;d=d;mark=0} in
  all_nodes := n::!all_nodes;
  n

let new_node _ = mk_node New 0

let mk_graph p =
  let wrong_size id = ntlserr (id^" has incompatible size") in
  let eqset =
    List.fold_left (fun s (id,exp) -> Smap.add id exp s) Smap.empty p.p_eqs in
  let active:(ident,node array) Hashtbl.t = Hashtbl.create 127 in
  let queue = ref [] in
  let reach = ref [] in
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
  List.iter
    (fun x ->
      Array.iter 
	(fun n ->
	    n.eq <- Input x;
	    n.mark <- 1) (find x)) p.p_inputs;
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

        | Erom (addrs,ws,a) ->
          let ra = find_arg a in
          if sz ra <> addrs then ntlserr (id^" rom has bad address size");
          if sz n <> ws then wrong_size id;
          Array.iter
            (fun n -> n.eq <- Rom ra; n.d<-0) n

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
          Array.iteri (fun i n -> n.eq <- Ram (ra,we.(0),wa,dat.(i)); n.d<-0) n

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
          if i<0 || i>=sz a || j<i || j>=sz a then invalid_arg "Illegal slice";
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
  let g = List.map add_node p.p_outputs in
  while !queue <> [] do
    let h = List.hd !queue in
    queue := List.tl !queue;
    reach := add_node h::!reach
  done;
  {output=g;reach=g@ !reach;named=Hashtbl.fold Smap.add active Smap.empty}

let set_marks x =
  List.iter
    (fun n -> n.mark <- x)
    !all_nodes

(* Using lists of lists, grouping nodes at the same depth *)
let toposort g =
  set_marks 0;
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
        | New -> raise Incomplete

        | Input _ -> add x acc

        | Const _ -> acc

        | Node m | Reg m | Not m ->
          sort (add x acc) m

        | Bin (_,m1,m2) ->
          sort (sort (add x acc) m1) m2

        | Mux (a,b,c) ->
            sort (sort (sort (add x acc) a) b) c

        | Rom a ->
            Array.fold_left sort (add x acc) a

        | Ram (a,n,b,m) ->
            let l = Array.fold_left sort (add x acc) a in
            let l = Array.fold_left sort l b in
            sort (sort l n) m
    end
  in
  List.fold_left sort [] (List.flatten (List.map Array.to_list g.output))

let string_of_bin2 = function
  | Or -> "Or"
  | And -> "And"
  | Xor -> "Xor"
  | Nand -> "Nand"

let print_exp h = function
    | New -> Format.fprintf h "new"
    | Input s -> Format.fprintf h "input %s" s
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

let print_eq h n =
  Format.fprintf h "%d = %a" n.id print_exp n.eq

let print_var h id n =
  Format.fprintf h "%s :@\n  @[" id;
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
