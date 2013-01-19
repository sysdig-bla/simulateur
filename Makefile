montre.net:
	cd sim2 ; make
	cd subleq ; make ; ./subleq montre.sq
	cd montre ; make ; ./mjc3.byte montre.mj
	./sim2/netlist_sim2.byte -s 50 -m subleq/montre.mem montre/montre.net

real_time:
	./sim2/netlist_sim2.byte -s 1000000000 -n 1024 -m subleq/montre.mem montre/montre.net -c 4096 -v

display:
	./sim2/netlist_sim2.byte -s 1000000000 -c 10 -m subleq/montre.mem montre/montre.net -disp
watch:
	cd sim2 ; make
	cd subleq ; make ; ./subleq counter.sq
	cd montre ; make ; ./mjc2.byte montre.mj
	./sim2/netlist_sim2.byte -s 100000 -n 1000 -m subleq/counter.mem montre/montre.net
