netlist_sim2:
	ocamlbuild -ocamlc 'ocamlc -g unix.cma graphics.cma' -use-menhir netlist_sim2.byte

netgraph:
	ocamlbuild -ocamlc 'ocamlc -g' -use-menhir test.byte

reduce:
	ocamlbuild reducegraph.cmx

schedule:
	ocamlbuild scheduler.cmx

test:
	./netlist_sim2.byte clock_div.net -c 2 -v -disp

clean:
	ocamlbuild -clean
	rm -f *\~
