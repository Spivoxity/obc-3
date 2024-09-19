# lablgtk3/lablgen/Makefile

OCAMLC = ocamlc

all: propcc varcc

propcc varcc: %: myStream.cmo myGenlex.cmo %.cmo
	$(OCAMLC) $^ -o $@ 

# The utilities propcc and varcc were originally built using camlp4,
# but I've saved the resulting ML source to avoid depending on it.
# 
# propcc.ml varcc.ml: %.ml: %.ml4
#	camlp4 pa_o.cmo pa_op.cmo pr_o.cmo -impl $< >$@

propcc.cmo varcc.cmo myGenlex.cmo: myGenlex.cmi myStream.cmi
myStream.cmo: myStream.cmi

%.cmi: %.mli
	ocamlc -c $< -o $@ 

%.cmo: %.ml
	ocamlc -c $< -o $@

clean:

quiteclean: clean
	rm propcc varcc *.cmi *.cmo
