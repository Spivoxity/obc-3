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

/* Keep a table of branch targets, and deal with forward branches
   by backpatching when the target address is known.  There's a 
   small hash table of targets indexed by address. */

/* A (forward) branch waiting to be patched */
struct branch {
     int b_kind;		/* BRANCH or CASELAB */
     void *b_loc;		/* Branch location */
     branch b_next;		/* Next branch with same target */
};

#define BRANCH 1
#define CASELAB 2

#define HASH 128

/* There's a (little, we hope) pool of memory for recording branch
   info.  Notes about forward branches are recycled via a free-list,
   and the whole pool is made free before translating each procedure.
   To cope with really huge procedures, there's a chain of pools of
   increasing size.  None of this memory is ever returned. */

#define NPOOLS 10

static uchar *zpool[NPOOLS], *zfree, *zlimit;
static int pool;
static branch brfree;

/* zalloc -- allocate memory for storing branch info */
static void *zalloc(int size) {
     void *p;
     int poolsize;

     while (pool < 0 || zfree + size > zlimit) {
	  pool++;
	  if (pool >= NPOOLS) panic("patch memory overflow");
	  poolsize = (1 << pool) * PAGESIZE;
	  if (zpool[pool] == NULL)
	       zpool[pool] = (uchar *) scratch_alloc(poolsize, TRUE);
	  zfree = zpool[pool]; zlimit = zfree + poolsize;
     }

     p = (void *) zfree;
     zfree += size;
     return p;
}

/* bralloc -- allocate a branch record from the free-list or pool */
static branch bralloc(void) {
     if (brfree != NULL) {
	  branch p = brfree;
	  brfree = brfree->b_next;
	  return p;
     }

     return (branch) zalloc(sizeof(struct branch));
}

static codepoint hashtab[HASH];

codepoint new_label(void) {
     codepoint p;

     p = (codepoint) zalloc(sizeof(struct codepoint));
     p->l_lab = -1;
     p->l_loc = NULL;
     p->l_depth = -1;
     p->l_stack = 0;
     p->l_branches = NULL;
     p->l_hlink = NULL;

     return p;
}     

/* lookup -- search for a target record for an address */
static codepoint lookup(int addr, bool create) {
     int h = addr % HASH;
     codepoint p;

     for (p = hashtab[h]; p != NULL; p = p->l_hlink)
	  if (p->l_lab == addr) return p;

     if (!create) return NULL;

     p = new_label();
     p->l_lab = addr;
     p->l_hlink = hashtab[h];
     hashtab[h] = p;
     return p;
}

/* mark_label -- mark location as a branch target during preprocessing */
void mark_label(int addr) {
#ifdef DEBUG
     if (dflag > 1) printf("Mark %d\n", addr);
#endif
     (void) lookup(addr, TRUE);
}

/* to_label -- branch to the equivalent of a bytecode address */
codepoint to_label(int addr) {
     return lookup(addr, FALSE);
}

/* to_addr -- branch to a native code address */
codepoint to_addr(code_addr loc) {
     codepoint lab = new_label();
     lab->l_loc = loc;
     return lab;
}

/* patch_label -- insert target address, or record for backpatching later */
void patch_label(code_addr loc, codepoint lab) {
     if (lab->l_loc != NULL)
	  vm_patch(loc, lab->l_loc);
     else {
	  branch br = bralloc();
	  br->b_kind = BRANCH;
	  br->b_loc = (void *) loc;
	  br->b_next = lab->l_branches;
	  lab->l_branches = br;
     }
}

code_addr *caseptr;

/* case_label -- append target address to a jump table */
void case_label(int addr) {
     codepoint lab = lookup(addr, FALSE);

#ifdef DEBUG
     if (dflag > 1) printf("\tCASEL [%d]\n", addr);
#endif

     if (lab == NULL) panic("undefined case label");

     if (lab->l_loc != NULL)
	  *caseptr = lab->l_loc;
     else {
	  branch br = bralloc();
	  br->b_kind = CASELAB;
	  br->b_loc = (void *) caseptr;
	  br->b_next = lab->l_branches;
	  lab->l_branches = br;
     }

     caseptr++;
}

/* label -- define a label at the current native code address */
void label(codepoint lab) {
     code_addr loc; 
     branch br, q = NULL;

     assert(lab != NULL);

     lab->l_loc = loc = vm_label();

     /* Backpatch any forward branches to this location */
     for (br = lab->l_branches; br != NULL; q = br, br = br->b_next) {
	  switch (br->b_kind) {
	  case BRANCH:
	       vm_patch((code_addr) br->b_loc, loc);
	       break;
	  case CASELAB:
	       * (code_addr *) br->b_loc = loc;
	       break;
	  default:
	       panic("*bad branch code");
	  }
     }

     /* Put the branch records back on the free-list */
     if (q != NULL) {
	  q->b_next = brfree;
	  brfree = lab->l_branches;
	  lab->l_branches = NULL;
     }
}


/* Error handlers */

/* Runtime errors are handled by branching to an error handler, a
   small piece of code that is added at the end of the procedure.
   There's a table of handlers to avoid duplication of the code. */

typedef struct error *error;

struct error {
     int e_code;		/* Cause of the error */
     int e_line;		/* Line ot be reported in message */
     codepoint e_lab;		/* Branch target for handler */
     error e_next;		/* Next in list of handlers */
};

error errlist;			/* List of all handlers for this proc */

/* to_error -- branch to error handler */
codepoint to_error(int code, int line) {
     error e;

     for (e = errlist; e != NULL; e = e->e_next) {
	  if (e->e_code == code && e->e_line == line)
	       return e->e_lab;
     }
	  
     e = (error) zalloc(sizeof(struct error));
     e->e_code = code;
     e->e_line = line; 
     e->e_lab = new_label();
     e->e_next = errlist;
     errlist = e;

     return e->e_lab;
}

/* flush_errors -- generate error handlers at end of procedure */
void do_errors(void (*f)(codepoint, int, int)) {
     error e; 

     for (e = errlist; e != NULL; e = e->e_next)
	  (*f)(e->e_lab, e->e_code, e->e_line);
}

/* init_patch -- initialize patch memory */
void init_patch(void) {
     pool = -1; brfree = NULL;
     memset(hashtab, 0, HASH * sizeof(codepoint));
     errlist = NULL;
}
