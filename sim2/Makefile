netlist_sim2:
	ocamlbuild -ocamlc 'ocamlc unix.cma graphics.cma' -use-menhir netlist_sim2.byte

netlist_sim2_2:
	ocamlbuild -ocamlc 'ocamlc -g unix.cma graphics.cma' -use-menhir netlist_sim2.byte

netgraph:
	ocamlbuild -ocamlc 'ocamlc -g' -use-menhir test.byte

reduce:
	ocamlbuild reducegraph.cmx

schedule:
	ocamlbuild scheduler.cmx

test:
	./netlist_sim2.byte ../montre/display_test.net -disp -v -c 1 -s 9

clean:
	ocamlbuild -clean
	rm -f *\~
