montre.net:
	cd sim2 ; make
	cd subleq ; make ; ./subleq montre.sq
	cd montre ; make ; ./mjc2.byte montre.mj
	./sim2/netlist_sim2.byte -s 30 -v -m subleq/montre.mem montre/montre.net

watch:
	cd sim2 ; make
	cd subleq ; make ; ./subleq montre.sq
	cd montre ; make ; ./mjc3.byte montre.mj
	./sim2/netlist_sim2.byte -v -m subleq/montre.mem montre/montre.net
