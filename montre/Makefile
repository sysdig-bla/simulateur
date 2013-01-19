source = const.mj zero.mj sub.mj scheduler.mj reg.mj bcd.mj leqz.mj pc.mj ram.mj main.mj

montre.mj: $(source)
	cat $(source) > montre.mj
	./mjc.byte montre.mj

clean:
	rm -f *\~
