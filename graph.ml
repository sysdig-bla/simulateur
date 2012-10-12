type graph = bool array array

exception Cycle

let make n = Array.make_matrix n n false

let add_edge g i j = g.(i).(j) <- true

let cyclic g =
  let n = Array.length g in
  let mark = Array.make n 0 in
  let rec depth_search i =
    match mark.(i) with
      | 0 -> mark.(i) <- 1;
        for j = 0 to n-1 do
          if g.(i).(j)
            then depth_search j
        done;
        mark.(i) <- 2
      | 1 -> raise Cycle
      | _ -> ()
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
  let h = ref 0 in
  let k = ref 0 in
  for j = 0 to n-1 do
    for i = 0 to n-1 do
      if g.(i).(j) then adj.(j) <- adj.(j) + 1;
    done;
    if adj.(j) = 0
      then begin
        l.(!k) <- j;
        incr k
      end
  done;
  while !h < !k do
    for j = 0 to n-1 do
      if g.(l.(!h)).(j)
        then begin
          adj.(j) <- adj.(j)-1;
          if adj.(j) = 0
            then begin
              l.(!k) <- j;
              incr k
            end
        end
    done;
    incr h
  done;
  if !k < n then raise Cycle;
  Array.to_list l
