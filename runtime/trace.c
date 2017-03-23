/*
 * trace.c
 * 
 * This file is part of the Oxford Oberon-2 compiler
 * Copyright (c) 2006--2016 J. M. Spivey
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

#define TRACE
#include "obx.h"
#include "keiko.h"

char *fmt_inst(uchar *pc) {
     uchar *args = pc;
     struct _opcode *ip = &optable[*pc++];
     static char buf[80];
     char *s = buf;

     if (ip->i_name == NULL) {
	  strcpy(buf, "UNKNOWN");
	  return buf;
     }

     s += sprintf(s, "%s", ip->i_name);

     for (const char *p = ip->i_patt; *p != '\0'; p++) {
	  switch (*p) {
	  case '1': case 'K':
	       s += sprintf(s, " %d", get1(pc)); pc++; break;
	  case '2': case 'L':
	       s += sprintf(s, " %d", get2(pc)); pc += 2; break;
	  case 'R':
	       s += sprintf(s, " %ld", (long) (get2(pc)+(args-imem)));
               pc += 2; break;
	  case 'S':
	       s += sprintf(s, " %ld", (long) (get1(pc)+(args-imem)));
               pc += 1; break;
	  case 'N':
	       s += sprintf(s, " %d", ip->i_arg); break;
	  default:
	       s += sprintf(s, " ?%c?", *p);
	  }
     }

     return buf;
}

void dump(void) {
     for (int k = 0; k < nprocs; k++) {
	  proc p = proctab[k];
	  value *cp = p->p_addr;
	  uchar *pc, *limit;

	  if (! interpreted(cp)) continue;
	  
	  pc = pointer(cp[CP_CODE]); limit = pc + cp[CP_SIZE].i;

	  printf("Procedure %s:\n", proctab[k]->p_name);
	  while (pc < limit) {
	       int op = *pc;
	       uchar *pc1 = pc + optable[op].i_len;

	       printf("%6ld: %-30s", (long) (pc-imem), fmt_inst(pc));
	       while (pc < pc1) printf(" %d", *pc++);
	       printf("\n");

	       if (op == K_JCASE_1) {
		    int n = pc[-1];
		    for (int i = 0; i < n; i++) {
			 printf("%6ld:   CASEL %-22ld %d %d\n",
                                (long) (pc-imem), (long) (get2(pc)+(pc-imem)),
                                pc[0], pc[1]);
			 pc += 2;
		    }
	       }
	  }
     }
}

const char *prim_name(value *p) {
     if (pointer(p[1]) != NULL) return (char *) pointer(p[1]);
     return "(unknown)";
}
