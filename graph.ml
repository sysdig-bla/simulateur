type graph = int list array

exception Cycle

let make n = Array.make n []

(* Operations sur les listes triees d'entiers *)
let rec slist_insert i = function
  | [] -> [i]
  | h::t as l when h < i -> i::l
  | h::t -> h::(slist_insert i t)

let add_edge g i j =
  g.(i) <- slist_insert j g.(i)

let cyclic g =
  let n = Array.length g in
  let mark = Array.make n 0 in
  let rec depth_search i =
    match mark.(i) with
      | 0 -> mark.(i) <- 1;
        lsearch g.(i);
        mark.(i) <- 2
      | 1 -> raise Cycle
      | _ -> ()
  and lsearch = function
    | [] -> ()
    | h::t -> depth_search h;
      lsearch t
  in
  try
    for i = 0 to n-1 do
      depth_search i
    done;
    false
  with
    | Cycle -> true

let tsort g =
  let n = Array.length g in
  let adj = Array.make n 0 in
  let l = Array.make n 0 in
  let hd = ref 0 in
  let tl = ref 0 in
  for i = 0 to n-1 do
    List.fold_left
      (fun _ -> fun j -> adj.(j) <- adj.(j)+1) () g.(i);
  done;
  Array.iteri
    (fun i -> fun a -> if a = 0
       then (l.(!hd) <- i; incr hd))
    adj;
  while !tl < !hd do
    List.iter
      (fun j -> adj.(j) <- adj.(j) - 1;
         if adj.(j) = 0
           then (l.(!hd) <- j; incr hd))
      g.(l.(!tl));
    incr tl;
  done;
  if !hd < n then raise Cycle;
  Array.to_list l
