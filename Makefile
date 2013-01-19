montre.net:
	cd sim2 ; make
	cd subleq ; make ; ./subleq montre.sq
	cd montre ; make ; ./mjc3.byte montre.mj
	./sim2/netlist_sim2.byte -s 50 -v -m subleq/montre.mem montre/montre.net

watch:
	cd sim2 ; make
	cd subleq ; make ; ./subleq montre.sq
	cd montre ; make ; ./mjc3.byte montre.mj
	./sim2/netlist_sim2.byte -s 10 -v -m subleq/counter.mem montre/montre.net
