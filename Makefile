real_time: subleq/montre.mem montre/montre.net sim2/netlist_sim2.byte
	./sim2/netlist_sim2.byte -s 1000000 -n 1024 -m subleq/montre.mem montre/montre.net -c 1024 -disp

montre/mjc.byte:
	cd minijazz ; ocamlbuild mjc.byte
	cd montre ; ln -s ../minijazz/_build/main/mjc.byte

subleq/montre.mem:
	cd subleq ; make

sim2/netlist_sim2.byte:
	cd sim2 ; make

montre/montre.net: montre/mjc.byte
	cd montre ; make

quick:
	./sim2/netlist_sim2.byte -s 50 -m subleq/montre.mem montre/montre.net -disp -n 1024

display:
	./sim2/netlist_sim2.byte -s 1000000000 -c 10 -m subleq/montre.mem montre/montre.net -disp

watch: subleq/montre.mem montre/montre.net sim2/netlist_sim2.byte
	./sim2/netlist_sim2.byte -s 100000 -n 1000 -m subleq/counter.mem montre/montre.net

clean:
	cd sim2 ; make clean
	cd subleq ; make clean
	cd montre ; make clean
	cd minijazz ; ocamlbuild -clean
	rm -rf *\~ */*\~ */*/*\~

archive: clean
	rm -rf Delpeuch-Lefebvre-Xia
	mkdir Delpeuch-Lefebvre-Xia
	cp -r Makefile montre subleq sim2 rapport minijazz Delpeuch-Lefebvre-Xia
	tar -zcf Delpeuch-Lefebvre-Xia.tgz Delpeuch-Lefebvre-Xia
