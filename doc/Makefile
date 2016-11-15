all: obcman.pdf

think: Sleep.k Main.k prim.o
	obc -C -o $@ $^

think2: Sleep2.k Main2.k
	obc -o $@ $^

prim.so: prim.c
	gcc -m32 -fPIC -shared -I /usr/local/lib/obc $< -o $@

%.k: %.m
	obc -c $<

prim.o: prim.c
	obc -c $<

Main.k: Sleep.k

%.pdf: %.tex mymacs.tex
	pdftex $<
	pdftex $<

%.tex: %.ms obgrind
	obgrind -e $< >$@

obgrind: obgrind.l
	flex -o obgrind.c obgrind.l
	gcc -o obgrind obgrind.c

clean: force
	rm -f obcman.pdf obcman.tex *.aux *.log *.toc
	rm -f *.k prim.o prim.so think think2 

realclean: clean
	rm -f obgrind.c obgrind

force:
