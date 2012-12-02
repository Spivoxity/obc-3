/*
 * iskel.c
 * 
 * This file is part of the Oxford Oberon-2 compiler
 * Copyright (c) 2006 J. M. Spivey
 * All rights reserved
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 * 3. The name of the author may not be used to endorse or promote products
 *    derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
 * OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
 * OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
 * ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

/* This file is the skeleton of the bytecode interpreter; the parts
   specific to each instruction are inserted from the file
   'keiko.iset' by the script 'iset.tcl'.  There are four places that
   code is inserted, each marked by two dollar signs.  In order of
   appearance, they are:

   1. A list of instructions used for disassembling the byte code
     (used if TRACE is defined).

   2. A jump table for quick dispatching (used if JTABLE is defined).

   3. Macro definitions used in the action routines.

   4. Action routines for each instruction, forming the cases in a big
      switch. */

#include <math.h>
#include "obx.h"
#include "keiko.h"

#ifdef HAVE_INDEXED_JUMPS
#define JTABLE 1
#endif

#ifdef TRACE
#define DISASS 1
#undef JTABLE
#define do_find_proc if (dflag > 1) thisproc = find_proc(cp)
#else
#define do_find_proc
#endif

#ifdef PROFILE
#undef JTABLE
#endif

#ifdef OBXDEB
#define DISASS 1
#endif

#ifdef DISASS
struct opcode optable[256] = {
$$ instruction formats
};
#endif

$$ macro definitions

/* interp -- main loop of the interpreter */
void interp(value *sp0) {
     register value *cp = sp0[CP].p;
     uchar *pc = cp[CP_CODE].x;
     register uchar *pc0 = NULL;
     register value *sp = sp0;
#ifndef NOACC
     register value acc;
#endif
     register uchar ir = 0;
#ifdef PROFILE
     register counter ticks = 0;
#endif
     register value *bp = NULL;
     value *base = sp0;
#ifdef TRACE
     proc thisproc = NULL;
#endif

#ifdef JTABLE
     /* Save time by using gcc's label array feature */
     static void *jtable[256] = {
$$ jump table
     };
#endif

#ifdef JTABLE
/* Each action ends with an indexed jump to the next */
#define ACTION(op) lbl_ ## op:
#define ALSO(op)
#define DEFAULT
#define NEXT       goto *jtable[ir = *(pc0 = pc)]
#else
/* Actions are just cases in a big switch */
#define ACTION(op) case K_ ## op:
#define ALSO(op)   case K_ ## op:
#define DEFAULT    default:
#define NEXT       break
#endif

     level++;
     do_find_proc;

#ifdef PROFILE
     prof_enter(cp, 0, PROF_CALL);
#endif

     frame();

#ifdef JTABLE
     NEXT;
#else
     while (TRUE) {
#ifdef TRACE
	  if (dflag > 1) {
	       int i;
	       printf("pc=%p sp=%p bp=%p cp=%p\n", pc, sp, bp, cp);
	       printf("pc=%s+%d(%p) sp=%p bp=%p cp=%p",
		      thisproc->p_name, pc - cp[1].x, pc, sp, bp, cp);
	       fflush(stdout);
#ifdef NOACC
	       for (i = 0; 
#else
	       printf(" %x", acc.i);
	       for (i = 1; 
#endif
		    i < 8; i++) printf(" %x", sp[i].i);
	       printf("\n");
	       printf("%6d: %s\n", pc-imem, fmt_inst(pc));
	       fflush(stdout);
	  }
#endif

#ifdef PROFILE
	  ticks++;
#endif

	  switch (ir = *(pc0 = pc)) {
#endif

$$ action routines

	  ACTION(ILLEGAL)
	  DEFAULT
	       panic("*illegal instruction %d", ir);
#ifndef JTABLE
	  }
     }
#endif
}
