exception Invalid_data of string

let data = ref Smap.empty

let bool_of_char = function
  | '0' -> 0
  | '1' -> 1
  | _ -> 2

let print_matrix =
  Array.iter (fun a -> Array.iter (fun x -> Printf.printf "%d " (if x
then 1 else 0)) a)

let load_data filename =
  try
    let h = open_in filename in
    let n = Scanf.fscanf h " %d" (fun x -> x) in
    for i = 1 to n do
      let s = Scanf.fscanf h " %s" (fun s -> s) in
      let k,ws = Scanf.fscanf h " %d %d" (fun x y-> x,y) in
      let j = ref 0 in
      let b = Array.make_matrix ws (1 lsl k) false in
      while !j < (1 lsl k)*ws do
        match bool_of_char (Scanf.fscanf h " %c" (fun c -> c)) with
          | 0 -> b.(!j mod ws).(!j/ws) <- false; incr j;
          | 1 -> b.(!j mod ws).(!j/ws) <- true; incr j;
          | _ -> ()
      done;
      data := Smap.add s b !data;
    done
  with
  | _ -> raise (Invalid_data "failed to read file")

let dataflag = ref true

(* Default to 0 and print warning *)
let get s ws sz =
  try
    let k = try
      Smap.find s !data
    with Not_found -> Smap.find "default" !data in
    if Array.length k <> ws || Array.length k.(0) <> sz
    then begin
      let mem = Array.make_matrix ws sz false in
      for i = 0 to min (Array.length k) ws -1 do
        for j = 0 to min (Array.length k.(0)) sz -1 do
          mem.(i).(j) <- k.(i).(j)
        done
      done;
      mem
    end
    else k
  with
  | Not_found
  | Invalid_argument _ ->
      if !dataflag
        then Printf.eprintf
          "Padding data with 0\n%!";
      dataflag := false;
      Array.make_matrix ws sz false
