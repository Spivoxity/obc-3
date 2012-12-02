/*
 * vm.h
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

typedef unsigned char *code_addr;

#define __OP__(p) \
     p(ADD) p(ADDC) p(ADDF) p(AND) p(BEQ) p(BEQF) p(BGEQ) p(BGEQF)  \
     p(BGEQU) p(BGT) p(BGTF) p(BLEQ) p(BLEQF) p(BLT) p(BLTF)	    \
     p(BLTU) p(BNEQ) p(BNEQF) p(CONVIC) p(CONVIF) p(CONVIS) p(DIVF) \
     p(EQ) p(EQF) p(GEQ) p(GEQF) p(GETARG) p(GT) p(GTF) p(JUMP)	    \
     p(LDW) p(LDCU) p(LDD) p(LDSU) p(LDC) p(LDS) p(LEQ)		    \
     p(LEQF) p(LSH) p(LT) p(LTF) p(MOV)	p(MUL) p(MULF)		    \
     p(NEG) p(NEGF) p(NEQ) p(NEQF) p(NOT) p(OR) p(RET) p(RETVAL)    \
     p(RSH) p(RSHU) p(STW) p(STC) p(STD) p(STS) p(SUB)		    \
     p(SUBC) p(SUBF) p(XOR) p(PREP) p(ARG) p(CALL) p(ZEROF)	    \
     p(BGTU) p(BLEQU)

#define __op1__(op) op,

typedef enum { 
     __OP__(__op1__)
} operation;

/* Integer resisters V0 .. V3 are callee-save.  The GETARG operation
   may not work if V3 is changed from the value it was assigned in the
   routine prolog.  Integer registers R0 .. R2 are caller-save, as are
   the floating-point registers xF0 .. xF5.  The floating-point
   registers are each big enough to hold a double. */

#define __VMREG__(p) \
     p(xV0) p(xV1) p(xV2) p(xV3) p(xR0) p(xR1) p(xR2)	\
     p(xF0) p(xF1) p(xF2) p(xF3) p(xF4) p(xF5)		\
     p(xRET) p(xZERO)

typedef enum {
     __VMREG__(__op1__)
} vmreg;

extern int vm_debug;

#ifdef MNEMONIC
#define __op2__(op) #op,

/* Mnemonics for the RISC-ish interface instructions */
static const char *mnemonic[] = {
     __OP__(__op2__)
};

int vm_debug;

#ifdef DEBUG
/* Register names */
static const char *rname[] = {
     __VMREG__(__op2__)
};

#define vm_dbg(fmt, ...) if (vm_debug > 0) _vm_dbg(fmt, __VA_ARGS__)

static void _vm_dbg(const char *fmt, ...) {
     va_list va;

     va_start(va, fmt);
     printf("\t\t\t\t\t");
     vprintf(fmt, va);
     printf("\n");
     va_end(va);
}

#else
#define vm_dbg(fmt, ...)
#endif
#endif

void vm_gen0(operation op);
void vm_gen1r(operation op, vmreg a);
void vm_gen1i(operation op, int a);
code_addr vm_gen1b(operation op, code_addr lab);
void vm_gen2rr(operation op, vmreg a, vmreg b);
void vm_gen2ri(operation op, vmreg a, int b);
void vm_gen3rrr(operation op, vmreg a, vmreg b, vmreg c);
void vm_gen3rri(operation op, vmreg a, vmreg b, int c);
code_addr vm_gen3rrb(operation op, vmreg a, vmreg b, code_addr lab);
code_addr vm_gen3rib(operation op, vmreg a, int b, code_addr lab);

void vm_patch(code_addr loc, code_addr lab);
code_addr vm_label(void);
void vm_prolog(int n);
int vm_arg(void);
void vm_flush(code_addr start, code_addr finish);
code_addr *vm_jumptable(int n);

/* Callback to allocate pages of memory */
void *vm_alloc(int size);
