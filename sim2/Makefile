schedule:

netgraph:
	ocamlbuild -ocamlc 'ocamlc -g' -use-menhir test.byte

reduce:
	ocamlbuild reducegraph.cmx

schedule:
	ocamlbuild scheduler.cmx

test:
	ocamlrun -b test.byte ../test/nadder.net

clean:
	ocamlbuild -clean
	rm -f *\~
