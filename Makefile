
all:
	ocamlbuild -use-menhir main.byte 

clean:
	rm -f *\~ *.cm[oi]
