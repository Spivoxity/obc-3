/*
 * jit.h
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

#include "vm.h"

#define INT64

/* jitlab.c */

typedef struct _codepoint *codepoint;
typedef struct _branch *branch;

/* A branch target */
struct _codepoint {
     int l_lab;			/* Bytecode address */
     code_addr l_loc;		/* Native code address */
     int l_depth;		/* Stack depth */
     int l_stack;		/* Bitmap for sizes of stack items */
     branch l_branches;		/* List of branches */
     codepoint l_hlink;		/* Next label in hash chain */
};

codepoint new_label(void);
void mark_label(int addr);
void label(codepoint lab);
void patch_label(code_addr loc, codepoint lab);
void case_label(int addr);

codepoint to_label(int addr);
codepoint to_addr(code_addr loc);
codepoint to_error(int code, int line);

void do_errors(void (*f)(codepoint, int, int));
void init_patch(void);

extern code_addr *caseptr;


/* jitregs.c */

#define INT 1
#define FLO 2

/* Register rSP is the same as the last integer register, so that register
   can be used only in procedures that don't use FLEXCOPY, so that the 
   frame has a fixed size and all addressing can be done relative to rBP. */

typedef struct _reg *reg;

extern int nregs;
extern reg rBP, rSP, rI0, rI1, rI2, rZERO;

typedef struct _ctvalue {
     int v_op;			/* Operation to fetch value */
     int v_type;		/* INT or FLO */
     int v_val;			/* Constant or offset */
     reg v_reg;			/* Register */
     int v_size;		/* Size (1 or 2 words) */
} *ctvalue;;

extern struct _reg {
     char r_name[8];            /* Our name for the register */
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

#define ralloc(s) ralloc_avoid(s, rZERO)

reg ralloc_suggest(int s, reg r);
reg ralloc_avoid(int s, reg r2);

reg kill(reg r);
void killregs(void);

int n_reserved(void);

void init_regs(void);


/* jitvalue.c */

#define STACK 32		/* Maximum stack depth */

int count_args(int size);
void get_sp(reg r);
void flex_space(reg nreg);
void dumpregs(void);

void init_stack(int frame);
void push(int op, int type, reg r, int val, int size);
void pop(int n);
void unlock(int n);
ctvalue peek(int n);

reg move_to_reg(int i, int ty);
void move_to_frame(int i);
ctvalue move_from_frame(int i);
ctvalue fix_const(int i, mybool rflag);
#define move_to_rc(i) fix_const(i, TRUE)

void flush_stack(int a, int b);
#define flush(a) flush_stack(a, STACK)
void spill(reg r);

void deref(int op, int ty, int size);
void store(int ldop, int ty, int s);
void plusa();

void save_stack(codepoint lab);
void restore_stack(codepoint lab);

#ifdef INT64
void move_longval(ctvalue src, reg rd, int offd);
void get_halflong(ctvalue src, int off, reg dst);
#endif


/* jitop.c */

extern const char *mnemonic[];

#ifdef DEBUG
#define vm_dbg(fmt, ...) \
     (vm_debug > 0 ? _vm_dbg(fmt, __VA_ARGS__) : (void) 0)

void _vm_dbg(const char *fmt, ...);

char *fmt_val(int v);

char *fmt_dest(codepoint lab);
#else
#define vm_dbg(fmt, ...) (void) 0
#endif

#define vm_dbg1(fmt, op, ...) vm_dbg(fmt, mnemonic[op], ##__VA_ARGS__)

#define g0(op) \
     vm_dbg1("%s", op), vm_gen0(op)
#define g1r(op, a) \
     vm_dbg1("%s %s", op, a->r_name), vm_gen1r(op, a->r_reg)
#define g1i(op, a) \
     vm_dbg1("%s %s", op, fmt_val(a)), vm_gen1i(op, a)
#define g1b(op, lab) \
     vm_dbg1("%s %s", op, fmt_dest(lab)), \
     use_label(vm_gen1b(op, NULL), lab)
#define g2rr(op, a, b) \
     vm_dbg1("%s %s, %s", op, a->r_name, b->r_name), \
     vm_gen2rr(op, a->r_reg, b->r_reg)
#define g2ri(op, a, b) \
     vm_dbg1("%s %s, %s", op, a->r_name, fmt_val(b)),   \
     vm_gen2ri(op, a->r_reg, b)
#define g3rrr(op, a, b, c) \
     vm_dbg1("%s %s, %s, %s", op, a->r_name, b->r_name, c->r_name), \
     vm_gen3rrr(op, a->r_reg, b->r_reg, c->r_reg)
#define g3rri(op, a, b, c) \
     vm_dbg1("%s %s, %s, %s", op, a->r_name, b->r_name, fmt_val(c)),    \
     vm_gen3rri(op, a->r_reg, b->r_reg, c)
#define g3rrb(op, a, b, lab) \
     vm_dbg1("%s %s, %s, %s", op, a->r_name, b->r_name, fmt_dest(lab)), \
          use_label(vm_gen3rrb(op, a->r_reg, b->r_reg, NULL), lab)
#define g3rib(op, a, b, lab) \
     vm_dbg1("%s %s, %s, %s", op, a->r_name, fmt_val(b), fmt_dest(lab)),    \
          use_label(vm_gen3rib(op, a->r_reg, b, NULL), lab)

void use_label(code_addr loc, codepoint lab);

void gcall1(const char *fname, int f, int n, ...);
void gcallr(reg f, int n, ...);

/* gcall -- call fixed function */
#define gcall(f, n, ...) gcall1(#f, (int) f, n, __VA_ARGS__)

