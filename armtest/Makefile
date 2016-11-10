#
# armtest/Makefile
# 
# This file is part of the Oxford Oberon-2 compiler
# Copyright (c) 2006, 2015  J. M. Spivey
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

REVID := $(shell cat ../revid)

subdir = armtest
CC = arm-linux-gnueabihf-gcc -marm -march=armv6
CFLAGS = -g -O2 -I . -I ../runtime -I ../thunder
RTFLAGS = -rdynamic
LIBS = -ldl
LIBDIR = /usr/arm-linux-gnueabihf/lib
OBXJ = jit.o jitlab.o jitregs.o jitval.o vmpi.o labels.o vmdebug.o \
	codebuf.o xmain-j.o decode.o support.o loader.o dynlink.o gc.o
PRIMS = Builtin Args GC Timer DynLink Files Conv Random
DLPRIMS = Math MathL
TCLSH = tclsh

vpath %.c ../runtime ../thunder ../lib
vpath %.h ../runtime ../thunder

all: obxj-arm $(DLPRIMS:%=%.so) sanity

obxj-arm: $(OBXJ) $(PRIMS:%=%.o)
	$(CC) $(RTFLAGS) -o $@ $^ $(LIBS)

xmain-j.o: xmain.c
	$(CC) $(CFLAGS) -DJIT -DREVID=\"$(REVID)\" -c $< -o $@

Math.so MathL.so: %.so: %.c
	$(CC) $(CFLAGS) -fPIC -shared $< -lm -o $@

primtab.c: $(PRIMS:%=../lib/%.m)
	$(TCLSH) ../lib/makeprims.tcl -t $^ >$@

config.h: config.h.in
	cd .. && ./config.status --file=$(subdir)/$@

sanity: force
	env OBC_LIB=. qemu-arm -B 0x100000 $(LIBDIR)/ld-linux.so.3 \
	    --library-path $(LIBDIR) ./obxj-arm ../Fac.x 19
	@echo The factorial of 19 is 121645100408832000

clean: force
	rm -f obxj-arm *.o *.so primtab.c *.k a.out data out.0 out config.h

force:

### Crude approximation

$(OBXJ) $(PRIMS:%=%.o): \
	obx.h obcommon.h config.h exec.h keiko.h decode.h vm.h
