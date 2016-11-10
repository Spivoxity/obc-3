/*
 * jitlab.c
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

#include "obx.h"
#include "jit.h"
#include <assert.h>

// A small hash table of targets indexed by address.

#define HASH 128

static codepoint hashtab[HASH];

/* lookup -- search for a target record for an address */
static codepoint lookup(int addr, mybool create) {
     int h = addr % HASH;
     codepoint p;

     for (p = hashtab[h]; p != NULL; p = p->l_hlink)
	  if (p->l_lab == addr) return p;

     if (!create) return NULL;

     p = (codepoint) vm_scratch(sizeof(struct _codepoint));
     p->l_lab = addr;
     p->l_vmlab = vm_newlab();
     p->l_depth = -1;
     p->l_stack = 0;
     p->l_hlink = hashtab[h];
     hashtab[h] = p;
     return p;
}

/* mark_label -- mark location as a branch target during preprocessing */
void mark_label(int addr) {
#ifdef DEBUG
     if (dflag > 2) printf("Mark %d\n", addr);
#endif
     (void) lookup(addr, TRUE);
}

codepoint find_label(int addr) {
     return lookup(addr, FALSE);
}

/* case_label -- append target address to a jump table */
void case_label(int addr) {
     codepoint lab = lookup(addr, FALSE);

#ifdef DEBUG
     if (dflag > 1) printf("\tCASEL [%#x]\n", addr);
#endif

     if (lab == NULL) panic("undefined case label");
     vm_caselab(lab->l_vmlab);
}

/* label -- define a label at the current native code address */
void label(codepoint lab) {
     assert(lab != NULL);
     vm_label(lab->l_vmlab);
}


/* Error handlers */

/* Runtime errors are handled by branching to an error handler, a
   small piece of code that is added at the end of the procedure.
   There's a table of handlers to avoid duplication of the code. */

typedef struct _error *error;

struct _error {
     int e_code;		/* Cause of the error */
     int e_line;		/* Line ot be reported in message */
     vmlabel e_lab;		/* Branch target for handler */
     error e_next;		/* Next in list of handlers */
};

error errlist;			/* List of all handlers for this proc */

/* handler -- branch to error handler */
vmlabel handler(int code, int line) {
     error e;

     for (e = errlist; e != NULL; e = e->e_next) {
	  if (e->e_code == code && e->e_line == line)
	       return e->e_lab;
     }
	  
     e = (error) vm_scratch(sizeof(struct _error));
     e->e_code = code;
     e->e_line = line; 
     e->e_lab = vm_newlab();
     e->e_next = errlist;
     errlist = e;

     return e->e_lab;
}

/* do_errors -- iterate over error handlers */
void do_errors(void (*f)(vmlabel, int, int)) {
     error e; 

     for (e = errlist; e != NULL; e = e->e_next)
	  (*f)(e->e_lab, e->e_code, e->e_line);
}

/* init_labels -- initialize label memory */
void init_labels(void) {
     memset(hashtab, 0, HASH * sizeof(codepoint));
     errlist = NULL;
}
