/*
 * vmdebug.c
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

#include "config.h"
#include "vm.h"
#include "vminternal.h"
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

int vm_debug, vm_aflag;

const char *vm_regname(vmreg r) {
     return r->vr_name;
}

#define __op2__(op) #op,
static const char *mnemonic[] = {
     __OP__(__op2__)
};

void vm_panic(const char *fmt, ...) {
     va_list va;
     
     va_start(va, fmt);
     fprintf(stderr, "JIT panic: ");
     vfprintf(stderr, fmt, va);
     fprintf(stderr, "\n");
     va_end(va);

     exit(2);
}

void vm_unknown(const char *where, operation op) {
     vm_panic("unknown op -- %s %s (%d)\n", where, mnemonic[op], op);
}

#ifdef DEBUG
void vm_debug1(int op, int nrands, ...) {
     va_list va;

     if (vm_debug < 1) return;

     va_start(va, nrands);
     printf("--- %s", mnemonic[op]);
     if (nrands > 0) {
          printf(" %s", va_arg(va, char *));
          for (int i = 1; i < nrands; i++)
               printf(", %s", va_arg(va, char *));
     }
     printf("\n");
     va_end(va);
}

static code_addr start;

void vm_debug2(const char *fmt, ...) {
     va_list va;

     if (vm_debug < 2) return;

     va_start(va, fmt);
     printf("---   ");
     vprintf(fmt, va);
     va_end(va);

     start = pc;
}

void vm_done(void) {
     if (vm_debug < 2) return;

     if (pc > start && !vm_aflag) {
          printf(" [");
          int n = vm_print(start);
          for (code_addr p = start+n; p < pc; p += n) {
               printf(" ");
               n = vm_print(p);
          }
          printf("]");
     }
     printf("\n");
}

char *fmt_val(int v) {
     static char buf[16];

     // Print offsets in decimal, addresses in hex
     
     if (v > -2048 && v < 2048)
          sprintf(buf, "%d", v);
     else if (vm_aflag)
          sprintf(buf, "<addr>");
     else
          sprintf(buf, "%#x", v);

     return buf;
}
#endif
