netgraph:
	ocamlbuild -ocamlc 'ocamlc -g' -use-menhir test.byte

test:
	ocamlrun -b test.byte ../test/nadder.net

clean:
	ocamlbuild -clean
	rm -f *\~
