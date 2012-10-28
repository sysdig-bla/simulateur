(* Num of ycles *)
let n = read_int ()

(* size of adder *)
let w = read_int ()

let () =
  Printf.printf "%d\n" n;
  for i = 1 to n do
    for k = 1 to 2 do
      for j = 1 to w do
        Printf.printf "%d" (Random.int 2)
      done;
      print_string (if k=1 then " " else "")
    done;
    print_newline ()
  done


