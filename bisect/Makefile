#
# compiler/Makefile.  Generated from Makefile.in by configure.
#
# compiler/Makefile.in
# 
# This file is part of the Oxford Oberon-2 compiler
# Copyright (c) 2006--2016 J. M. Spivey
# All rights reserved
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# 1. Redistributions of source code must retain the above copyright notice,
#    this list of conditions and the following disclaimer.
# 2. Redistributions in binary form must reproduce the above copyright notice,
#    this list of conditions and the following disclaimer in the documentation
#    and/or other materials provided with the distribution.
# 3. The name of the author may not be used to endorse or promote products
#    derived from this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
# IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
# OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
# IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
# PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
# OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
# WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
# OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
# ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#

# Config stuff
OCAMLC = ocamlc
OCAMLLEX = ocamllex
MYACC = ../yacc/myacc
CAMLDEP = ../camldep
MLFLAGS = -g
TCLSH = tclsh
PACKAGE_VERSION = 3.1alpha
top_srcdir = ..
srcdir = .
subdir = compiler
SHELL = /bin/sh
EXEEXT = 
DEPEND = 1
COMPAT = 
# End

all : obc1

INCLUDE = -I ../mllib -I ../yacc

OBC1 = ../mllib/mllib.cma ../yacc/yyparse.cmo basis.cma compiler.cma main.cmo 
obc1: $(OBC1)
	$(OCAMLC) $(MLFLAGS) -I +bisect bisect.cma -custom $^ -o $@

COMPILER = icode.cmo peepopt.cmo tree.cmo lexer.cmo \
	parser.cmo expr.cmo inicheck.cmo check.cmo switch.cmo \
	stack.cmo igen.cmo  
compiler.cma: $(COMPILER)
	$(OCAMLC) $(MLFLAGS) -a $^ -o $@ 

BASIS = config.cmo symtab.cmo revid.cmo eval.cmo mach.cmo error.cmo \
	gcmap.cmo dict.cmo symlex.cmo symparse.cmo symfile.cmo
basis.cma: $(BASIS)
	$(OCAMLC) $(MLFLAGS) -a -o $@ $^

%.mli:: ../compiler/%.mli
	cp $< $@

%.ml:: ../compiler/%.ml
	sed -e '/^# [0-9]/d' -e 's/failwith/(*BISECT-IGNORE*) &/' \
	    -e 's/(\* (DEBUG-- \*)/(*BISECT-IGNORE-BEGIN*)/' \
	    -e 's/(\* --DEBUG) \*)/(*BISECT-IGNORE-END*)/' $< >$@

.SECONDARY:

## Rules

%.cmi: %.mli
	$(OCAMLC) $(MLFLAGS) $(INCLUDE) -c -o $@ $<

%.cmo: %.ml
	$(OCAMLC) $(MLFLAGS) $(INCLUDE) -I +bisect -c \
		-pp 'camlp4o str.cma $(OCAMLLIB)/bisect/bisect_pp.cmo' \
		-o $@ $<

OCAMLLIB := $(shell ocamlc -where)

.DELETE_ON_ERROR:

## Cleanup

# clean: remove object files
clean:
	rm -f obc1 *.mli *.ml *.cmi *.cmo *.cma *.cmp bisect*.out *.k
	rm -rf report

# quiteclean: remove generated files too
quiteclean: clean

# distclean: also remove all non-distributed files
distclean: quiteclean

realclean: distclean

## Configuration and dependencies
include ../compiler/Depend

# Don't use dependency generation for yacc scripts -- yacc may not be built
parser.cmo    : dict.cmi error.cmi lexer.cmo parser.cmi
parser.cmi    : eval.cmi symtab.cmi tree.cmi
symparse.cmo  : config.cmo error.cmi eval.cmi mach.cmi \
		symparse.cmi symtab.cmi
symparse.cmi  : dict.cmi
