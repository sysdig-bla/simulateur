open Netlist_ast
open Netgraph
open Reducegraph

(* At each depth level,
 * batch REG, RAM, ROM, INPUT (lvl 0), AND, OR, NOT, MUX together *)

type batch =
  | BNot of int*int array
  | BAnd of int*(int*int) array
  | BOr of int*(int*int) array
  | BMux of int*(int*int*int) array
  | BReg of int*int array
  | BRom of int*(int ref*bool array) array
  | BRam of int*(int ref*int*int ref*int*bool array) array

type memaddress = (int ref * int array) array

(* get rid of empty tasks *)
let rec clean_sch = function
  | [] -> []
  | h::tl ->
      match h with
      | BNot (_,t) | BReg (_,t) when Array.length t=0 ->
          clean_sch tl
      | BAnd (_,t) | BOr (_,t) when Array.length t=0 ->
          clean_sch tl
      | BMux (_,t) when Array.length t=0 ->
          clean_sch tl
      | BRom (_,t) when Array.length t=0 ->
          clean_sch tl
      | BRam (_,t) when Array.length t=0 ->
          clean_sch tl
      | _ -> h::clean_sch tl

let print_mem ma =
  for i = 0 to Array.length ma-1 do
    let (_,b) = ma.(i) in
    for j = 0 to Array.length b-1 do
      Printf.printf "%d " b.(j);
    done;
    Printf.printf "\n";
  done

let batch g =
  let aol = Array.of_list in

  (* Eliminate XOR and NAND operations *)
  reduce g;
  let t = toposort g in
  let n = nodenum t in

  let tape_index = ref 0 in

  (* Renumber the nodes so the it fits compactly in an array *)
  let indexmap:(id2,int) Hashtbl.t = Hashtbl.create 555 in

  (* Progressively fill the tape,
   * increment the index for each cell allocated *)
  let find = Hashtbl.find indexmap in
  let add x = Hashtbl.add indexmap x !tape_index; incr tape_index in

  (* Begin converting addresses to a suitable format *)
  let convert_addr a = (ref 0),Array.map (fun n -> n.id) a.loc in
  assert
    (Array.length g.address = 0 ||
    g.address.(Array.length g.address-1).ad_i = Array.length g.address-1);
  let memaddr = Array.map convert_addr g.address in

  (* Although they have the same type of elements,
   * l0 exclusively contains Reg,Rom,Ram,(and implicit Input) nodes
   * and l1 contains the logical operations *)
  let l0 = List.hd t in
  let l1 = List.tl t in

  (* We use two functions to process the operations,
   * it saves horizontal space because there are a lot of arguments and
   * it helps us check that the two types of operations are well split up*)
  let rec batch0 reg rom ram = function
    | [] -> reg,rom,ram
    | h::t -> match h.eq with
        | Reg m -> batch0 ((h.id,m.id)::reg) rom ram t
        | Rom (a,m) -> batch0 reg ((h.id,(fst memaddr.(a.ad_i),m))::rom) ram t
        | Ram (a,n,b,m,mem) ->
            batch0 reg rom
              ((h.id,
                (
                fst memaddr.(a.ad_i),
                n.id,
                fst memaddr.(b.ad_i),
                m.id,
                mem
                )
               )::ram) t
        | Input _ -> batch0 reg rom ram t
        | _ -> assert false
  in
  let rec batch1 a o n m = function
    | [] -> n,a,o,m
    | h::t ->
        match h.eq with
        | Not u -> batch1 a o ((h.id,[u.id])::n) m t
        | Mux (u,v,w) -> batch1 a o n ((h.id,[u.id;v.id;w.id])::m) t
        | Bin (b,u,v) ->
            begin match b with
            | And -> batch1 ((h.id,[u.id;v.id])::a) o n m t
            | Or -> batch1 a ((h.id,[u.id;v.id])::o) n m t
            | _ -> assert false
            end
        | _ -> assert false
  in
  let reg,rom,ram = batch0 [] [] [] l0 in
  let l1 = List.map (batch1 [] [] [] []) l1 in

  (* We reserve contiguous space for input variables *)
  Array.iteri
    (fun i s -> Array.iter (fun n ->
       add n.id) (Smap.find s g.named)) g.in_order;
  (* index has been incremented just that much ("input" count) *)
  let in_count = !tape_index in

  Hashtbl.add indexmap c0.id n;
  Hashtbl.add indexmap c1.id (n+1); (* Two indices for constants s *)

  tape_index := in_count;
  let assoc =  List.map (fun (x,y)-> add x; y) in
  let assoc2 = List.map (fun (x,y)-> add x; y) in (* Not polymorphic ?! *)
  let assoc3 = List.map (fun (x,y)-> add x; y) in

  (* Similar to the above *)
  (* Reserve contiguous space for registers, ROMs, RAMs,
   * remember their locations *)
  let reg_start = !tape_index in
  let reg=aol (assoc reg) in (*Array Of List*)
  let rom_start = !tape_index in
  let rom=aol (assoc2 rom) in
  let ram_start = !tape_index in
  let ram=aol (assoc3 ram) in

  (* Logical operations *)
  let assoc =
    List.map (fun (x,y)-> add x; List.map find y) in
  let assoc1 =
    List.map (function [x] -> x | _-> assert false) in
  let assoc2 =
    List.map (function [x;y] -> x,y | _-> assert false) in
  let assoc3 =
    List.map (function [x;y;z] -> x,y,z | _-> assert false) in
  let l1 = List.map
    (fun (n,a,o,m) ->
      let nstart = !tape_index in
      let n = aol(assoc1 (assoc n)) in
      let astart = !tape_index in
      let a = aol(assoc2 (assoc a)) in
      let ostart = !tape_index in
      let o = aol(assoc2 (assoc o)) in
      let mstart = !tape_index in
      let m = aol(assoc3 (assoc m)) in
      [
        BNot (nstart,n);
        BAnd (astart,a);
        BOr (ostart,o);
        BMux (mstart,m)]) l1 in

  (* This had to be done alternating the allocation of cells for logical ops 
   * and that of memory ops *)
  let reg = Array.map find reg in
  let ram = Array.map (fun (a,m,b,n,mem) ->
    a,find m,b,find n,mem) ram in
  (*let () =
    Array.iter (fun (_,a) ->
      Array.iteri (fun i n -> a.(i) <- find n) a) memaddr in*)

  let l0 = [
    BReg (reg_start,reg);
    BRom (rom_start,rom);
    BRam (ram_start,ram)] in
  let l1 = List.flatten l1 in

  if !tape_index <> n
    then begin
      Printf.eprintf "%d %d\n" n !tape_index;
      assert false;
    end;

  (* Picking output cells *)
  let outnodes =
    Array.to_list
      (Array.map
        (fun s -> Array.map (fun n->find n.id) (Smap.find s g.named))
        g.out_order) in

  let memaddr = Array.map (fun (r,a) -> (r,Array.map find a)) memaddr in

  (* I chose to put the memory ops first because it somewhat felt
   * more natural. But I think it should be symmetric
   * The switching of memory happens at clock tick whereas logical operations
   * happen inbetween, so things could be done the opposite way.
   * It seems however that with the other way we have to split the two parts
   * in one time step, whereas here we will be able to do it all at once *)
  clean_sch l0 @ clean_sch l1,memaddr,Array.concat outnodes,n+2,in_count

let get_output_loc g = 
  let r = ref 0 in
  let out_pos,out_size = Array.fold_left
    (fun (set1,set2) s ->
      let set1 = Smap.add s !r set1 in
      let sz = Array.length (Smap.find s g.named) in
      let set2 = Smap.add s sz set2 in
      r := !r+sz;
      set1,set2)
    (Smap.empty,Smap.empty) g.out_order in
  out_pos,out_size

let get_input_loc g = 
  let r = ref 0 in
  let in_pos,in_size = Array.fold_left
    (fun (set1,set2) s ->
      let set1 = Smap.add s !r set1 in
      let sz = Array.length (Smap.find s g.named) in
      let set2 = Smap.add s sz set2 in
      r := !r+sz;
      set1,set2)
    (Smap.empty,Smap.empty) g.in_order in
  in_pos,in_size

let print_sch_item h = function
  | BNot (i,a) -> Format.fprintf h "Not %3d -" i;
      Array.iter (Format.fprintf h " %d") a
  | BAnd (i,a) -> Format.fprintf h "And %3d -" i;
      Array.iter (fun (x,y) -> Format.fprintf h "(%d,%d) " x y) a
  | BOr (i,a) -> Format.fprintf h "Or  %3d -" i;
      Array.iter (fun (x,y) -> Format.fprintf h "(%d,%d) " x y) a
  | BMux (i,a) -> Format.fprintf h "Mux %3d -" i;
      Array.iter (fun (x,y,z) -> Format.fprintf h "(%d,%d,%d) " x y z) a
  | BReg (i,a) -> Format.fprintf h "Reg %3d -" i;
      Array.iter (Format.fprintf h "%d ") a
  | BRom (i,_) -> Format.fprintf h "ROM %3d" i;
  | BRam (i,_) -> Format.fprintf h "RAM %3d" i

let print_sch h l =
  List.iter (fun x -> print_sch_item h x; Format.fprintf h "@\n") l

let print_outnodes h (out_order,o_loc,o_sz,p_outputs) =
  List.iter
    (fun s ->
      let loc=Smap.find s o_loc in
      let sz =Smap.find s o_sz in
      Format.fprintf h "%s - " s;
      for i = 0 to sz-1 do
        Format.fprintf h "%d " out_order.(loc+i);
      done;
      Format.fprintf h "@\n") p_outputs

