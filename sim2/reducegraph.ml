open Netlist_ast
open Netgraph

exception Interrupt

(* Use hashtables to unify nodes *)
(* Substitution tables, logically equivalent nodes are unified *)
let subs_tbl:(id2,node) Hashtbl.t = Hashtbl.create 171

let not_tbl:(id2,node) Hashtbl.t = Hashtbl.create 137

let binop_tbl:(bin2*id2*id2,node) Hashtbl.t = Hashtbl.create 149

let mux_tbl:(id2*id2*id2,node) Hashtbl.t = Hashtbl.create 127

let queue=ref []
let push x = queue:=x::!queue

let reset_tbl g =
  Hashtbl.clear not_tbl;
  Hashtbl.clear binop_tbl;
  Hashtbl.add subs_tbl c0.id c0;
  Hashtbl.add subs_tbl c1.id c1;
  Hashtbl.add not_tbl c0.id c1;
  Hashtbl.add not_tbl c1.id c0

let subs n m =
  n.eq <- m.eq;
  n.d <- m.d;
  Hashtbl.replace subs_tbl n.id m;
  m

let find n =
  try
    Hashtbl.find subs_tbl n.id
  with
  | Not_found -> subs n n

let rec mk_not m =
  try
    Hashtbl.find not_tbl m.id(*= not n*)
  with
  | Not_found ->
      let n = mk_node (Not m) (m.d+1) in
      n.mark <- 1;
      Hashtbl.add not_tbl n.id m;
      Hashtbl.add not_tbl m.id n;
      n

and mk_binop o a b =
  let i,j = a.id,b.id in
  let i,j = if i<j then i,j else j,i in
  try
    Hashtbl.find binop_tbl (o,i,j)
  with
  | Not_found ->
    let m =
      let d = max a.d b.d +1 in
      match o with
      | Xor ->
          mk_mux a (mk_not b) b

      | Nand ->
          mk_not (mk_binop And a b)

      | And ->
          begin match a.eq,b.eq with
          | Const true,_ -> b
          | _,Const true -> a
          | Const false,_ | _,Const false -> c0

          | _,_ when a==b -> a

          | Not a,_ when a==b -> c0
          | _,Not b when a==b -> c0
          | Not e, Not f -> mk_not (mk_binop Or e f)

          | Bin (And,a,c),_ when b==a -> mk_binop And a c
          | Bin (And,c,a),_ when b==a -> mk_binop And a c
          | _,Bin (And,b,c) when a==b -> mk_binop And c b
          | _,Bin (And,c,b) when a==b -> mk_binop And c b
          | Bin (Or,a,_),_ when a==b -> a
          | Bin (Or,_,a),_ when a==b -> a
          | _,Bin (Or,b,_) when a==b -> b
          | _,Bin (Or,_,b) when a==b -> b

          | _,_ ->
              mk_node (Bin (And,a,b)) d;
          end

      | Or ->
          match a.eq,b.eq with
          | Const false,_ -> b
          | _,Const false -> a
          | Const true,_ | _,Const true -> c1
          | _,_ when a==b -> a
          | Not a,_ when a==b -> b
          | _,Not b when a==b -> a
          | Not e, Not f ->
              let p = mk_binop And e f in
              mk_not p
          | _,_ ->
              mk_node (Bin (Or,a,b)) d
  in
  Hashtbl.add binop_tbl (o,i,j) m;
  m

and mk_mux a b c =
  let m = try
    Hashtbl.find mux_tbl (a.id,b.id,c.id)
  with
  | Not_found -> match a.eq,b.eq,c.eq with
      | Const true,_,_ -> b
      | Const false,_,_ -> c
      | _,Const true,Const true -> c1
      | _,Const false,Const false -> c0
      | _,Const true,Const false -> a
      | _,Const false,Const true -> mk_not a
      | _,Const true,_ -> mk_binop Or a c
      | _,_,Const false -> mk_binop And a b
      | _,_,_ when b==c -> b
      | _,_,_ when a==b -> mk_binop Or a c
      | Not a,_,_ -> mk_mux a c b
      | _,Not b,Not c -> mk_not (mk_mux a b c)
      | _,_,_ -> 
          mk_node (Mux (a,b,c)) (max (max a.d b.d) c.d +1);
  in
  Hashtbl.add mux_tbl (a.id,b.id,c.id) m;
  m


and reduce1 n =
  if n.mark=1
  then find n
  else begin
    let m = match n.eq with
      | Node m ->
          let m=reduce1 m in
          subs n m

      | Not m ->
          let m=reduce1 m in
          subs n (mk_not m)

      | Bin (o,a,b) ->
          let a = reduce1 a in
          let b = reduce1 b in
          subs n (mk_binop o a b)

      | Mux (a,b,c) ->
          let a = reduce1 a in
          let b = reduce1 b in
          let c = reduce1 c in
          subs n (mk_mux a b c)

      | Reg m -> begin
          let m = find m in
          try
            match m.eq with
            | Const false -> subs n c0 
            | Reg n when n==m -> subs n c0
            | Reg m when n==m -> subs n c0
            | _ when n==m -> subs n c0
            | _ -> raise Interrupt
          with | Interrupt -> push n; find n
          end
      | Rom (ra,mem) -> find n
      | Ram (ra,we,wa,d,mem) ->
          push n;
          find n;
      | Input _ -> subs n n
      | Const true -> subs n c1
      | Const false -> subs n c0
      | New -> raise Incomplete
    in n.mark <- 1; m
  end

let reduce1 g =
  set_marks g 0;
  ignore (List.map reduce1 !all_nodes);
  (* For some reason it does not reduce nodes otherwise *)
  List.iter (fun a -> Array.iteri (fun i n -> a.(i) <- reduce1 a.(i)) a)
    g.range;
  List.iter
    (fun n ->
      match n.eq with
        | Ram (ra,we,wa,d,mem) ->
            n.eq <- Ram (ra,find we,wa,find d,mem)
        | Reg m ->
            n.eq <- Reg (find m)
        | _ -> assert false) !queue;
  queue := [];
  Array.iter
    (fun a ->
      Array.iteri (fun i n -> a.loc.(i) <- find a.loc.(i)) a.loc)
    g.address

let reduce g =
  reset_tbl g;
  reduce1 g;
  reduce1 g
