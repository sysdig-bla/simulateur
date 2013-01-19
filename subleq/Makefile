SRC=lexer.mll subleq.ml interpret.ml main.ml

montre.mem: subleq montre.sq
	./subleq montre.sq

subleq: $(SRC)
	ocamlbuild main.native
	mv main.native subleq

clean:
	ocamlbuild -clean
