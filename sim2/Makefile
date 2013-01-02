netlist_sim2:
	ocamlbuild -ocamlc 'ocamlc -g unix.cma graphics.cma' -use-menhir netlist_sim2.byte

netgraph:
	ocamlbuild -ocamlc 'ocamlc -g' -use-menhir test.byte

reduce:
	ocamlbuild reducegraph.cmx

schedule:
	ocamlbuild scheduler.cmx

test:
	./netlist_sim2.byte counter.net -disp-schedule -v -c 5 -s 15

clean:
	ocamlbuild -clean
	rm -f *\~
