all: obxj

OBXJ = jit.o jitlab.o jitregs.o jitval.o \
	xmain-j.o support.o loader.o dynlink.o gc.o primtab.o

obxj: $(OBXJ)
	$(CC) $(CFLAGS) $^ ../lib/oblib.o ../thunder/libthunder.a -lm -o $@

$(OBXJ): config.h

report: force
	gcov $(OBXJ)

%.o: %.c
	$(CC) $(CFLAGS) $(INCLUDE) -c $< -o $@

xmain-j.o: xmain.c
	$(CC) $(CFLAGS) $(INCLUDE) -DJIT -DREVID=\"gcov\" -c $< -o $@

vpath %.c ../runtime ../lib

clean: force
	rm -f obxj *.o *.gcda *.gcno *.gcov
	rm -f *.k a.x data out

force:

CC = gcc -std=gnu99
CFLAGS = -g -fno-strict-aliasing -fprofile-arcs -ftest-coverage
INCLUDE = -I . -I ../runtime -I ../thunder
