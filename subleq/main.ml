(* subleq compiler / interpreter *)

let interp = ref false
let raw = ref false
let s  = ref 10
let verbose = ref false
let a = ref "instr1"
let b = ref "instr2"
let c = ref "instr3"

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
      "nb of steps to simulate in interpreter";
    "-v",Arg.Set verbose,
      "print memory state";
    "-o",Arg.Tuple [Arg.Set_string a;Arg.Set_string b;Arg.Set_string c],
      "specify the ram names"
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
      Subleq.print h l;
      close_out h
    end;
  if !interp
    then begin
      let h = if !verbose then stdout else open_out "/dev/null" in
      Interpret.interpret h l !s;
      if !verbose then close_out h
    end
  else begin
      let f = if Filename.check_suffix !file ".sq"
        then (Filename.chop_suffix !file ".sq")^".mem"
        else !file^".s" in
      let h = open_out f in
      Subleq.print_binary h l !a !b !c;
      close_out h
  end

let () = main ()
