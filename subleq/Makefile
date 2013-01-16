SRC=lexer.mll subleq.ml interpret.ml main.ml

subleq: $(SRC)
	ocamlbuild main.native
	mv main.native subleq

clean:
	ocamlbuild -clean
