let interp = ref false
let raw = ref false
let s  = ref 10

let file = ref ""

let usage = Printf.sprintf "Usage: %s [-r] [-i]"
  (Filename.basename Sys.argv.(0))

let optlist =
  Arg.align
  [
    "-r",Arg.Set raw,
      "print raw subleq";
    "-i",Arg.Set interp,
      "interprete code";
    "-s",Arg.Set_int s,
      "Nb of steps to simulate in interpreter";
  ]

let main () =
  let collect s = file := s in
  Arg.parse optlist collect usage;

  let h = open_in !file in
  let l = Subleq.read h in
  close_in h;
  if !raw
    then begin
      let f = if Filename.check_suffix !file ".sq"
        then (Filename.chop_suffix !file ".sq")^".s"
        else !file^".s" in
      let h = open_out f in
      Subleq.print h l
    end;
  if !interp
    then begin
      Interpret.interpret l !s
    end
  else ()

let () = main ()
