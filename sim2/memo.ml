exception Invalid_data of string

let data = ref Smap.empty

let bool_of_char = function
  | '0' -> 0
  | '1' -> 1
  | _ -> 2

let load_data filename =
  try
    let h = open_in filename in
    let n = Scanf.fscanf h "%d " (fun x -> x) in
    for i = 1 to n do
      let s = Scanf.fscanf h "%s " (fun s -> s) in
      let k,ws = Scanf.fscanf h "%d %d" (fun x y-> x,y) in
      let j = ref 0 in
      let b = Array.make_matrix ws k false in
      while !j < k*ws do
        match bool_of_char (Scanf.fscanf h "%c" (fun c -> c)) with
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
let get s n i =
  try
    let k = (Smap.find s !data).(i) in
    if Array.length k <> n
    then raise Not_found
    else k
  with
  | Not_found
  | Invalid_argument _ ->
      if !dataflag then Printf.eprintf "Incomplete data...Default to 0\n";
      dataflag := false;
      Array.make n false
