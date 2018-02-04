/*
 * jit.h
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

#include "vm.h"

/* jitlab.c */

typedef struct _codepoint *codepoint;

/* A branch target */
struct _codepoint {
     int l_lab;			/* Bytecode address */
     vmlabel l_vmlab;           /* VM label */
     int l_depth;		/* Stack depth */
     int l_stack;		/* Bitmap for sizes of stack items */
     codepoint l_hlink;		/* Next label in hash chain */
};

void mark_label(int addr);
void label(codepoint lab);
void case_label(int addr);
codepoint find_label(int addr);
void do_errors(void (*f)(vmlabel, int, int));
void init_labels(void);
vmlabel handler(int code, int line);


/* jitregs.c */

#define INT 1
#define FLO 2

/* Register rSP is the same as the last integer register, so that register
   can be used only in procedures that don't use FLEXCOPY, so that the 
   frame has a fixed size and all addressing can be done relative to rBP. */

typedef struct _reg *reg;

extern int nregs;
extern reg rBP, rSP, rCP, rI0, rI1, rI2;

#define __VALKINDS__(v) \
     v(CON) v(REG) v(ADDR) v(KONW)               \
     v(MEMC) v(MEMS) v(MEMW) v(MEMQ)             \
     v(FCMPL) v(FCMPG) v(DCMPL) v(DCMPG) v(QCMP) \
     v(STKW) v(STKQ)

#define __v1__(sym) V_##sym,
typedef enum { __VALKINDS__(__v1__) } valkind;

typedef struct _ctvalue {
     valkind v_op;		/* Operation to fetch value */
     int v_type;		/* INT or FLO */
     int v_val;			/* Constant or offset */
     reg v_reg;			/* Register */
     int v_size;		/* Size (1 or 2 words) */
} *ctvalue;;

extern struct _reg {
     vmreg r_reg;               /* Physical register */
     int r_class;               /* INT or FLO, or 0 if special */
     int r_refct;               /* No of references on the stack */
     struct _ctvalue r_value;   /* Cached value */
} *regs;

#define for_regs(r) for (r = &regs[0]; r < &regs[nregs]; r++)

#define cached(r) ((r)->r_value.v_op != 0)
#define uncache(r) (r)->r_value.v_op = 0
#define member(r, s) ((r)->r_class == s)
#define rkind(r) ((r)->r_class == 0 ? INT : (r)->r_class)

#define OMEGA 1000

reg incref(reg r, int inc);

#define reserve(r) incref(r, 1)
#define rfree(r) incref(r, -1)
#define rlock(r) incref(r, OMEGA)
#define runlock(r) incref(r, -OMEGA)
#define rfreeze(r) incref(r, OMEGA+1)
#define rthaw(r) incref(r, -OMEGA-1)

#define ralloc(s) ralloc_avoid(s, NULL)

reg ralloc_suggest(int s, reg r);
reg ralloc_avoid(int s, reg r2);

reg kill(reg r);
void killregs(void);

int n_reserved(void);

void init_regs(void);

const char *regname(reg r);


/* jitvalue.c */

#define STACK 32		/* Maximum stack depth */

int count_args(int size);
void get_sp(reg r);
void flex_space(reg nreg);
void dumpregs(void);

void init_stack(int frame);
void push(valkind vkind, int type, reg r, int val, int size);
void pop(int n);
void unlock(int n);
ctvalue peek(int n);
void ldst_item(int op, reg rs, int i);
     
reg move_to_reg(int i, int ty);
void move_to_frame(int i);
ctvalue move_from_frame(int i);
ctvalue fix_const(int i, mybool rflag);
#define move_to_rc(i) fix_const(i, TRUE)

void flush_stack(int a, int b);
#define flush(a) flush_stack(a, STACK)
void spill(reg r);

void deref(valkind vkind, int ty, int size);
void store(valkind vkind, int s);
void add_offset();

void save_stack(codepoint lab);
void restore_stack(codepoint lab);
vmlabel target(int lab);

void move_longval(ctvalue src, reg rd, int offd);
void get_halflong(ctvalue src, int off, reg dst);

void gcall(void *f, int n);
void gcallr(reg r, int n);


/* Decoding tables */

#define MAXEQ 6

struct _inst {
     const char *i_name;	/* Name of the instruction */
     int i_equiv[MAXEQ];	/* Expansion into simpler instructions */
};

#define IMASK 0xffff
#define IARG 0x10000
#define ICON 0x20000

extern struct _inst instrs[];

struct _decode {
     int d_inst;
     const char *d_patt;
     int d_arg;
     int d_len;
};

extern struct _decode decode[];
