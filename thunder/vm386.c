/*
 * vm386.c
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

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>

#include "config.h"
#include "vm.h"
#include "vminternal.h"

typedef intptr_t address;


/* Register assignment */

/* These are the i386/amd64 registers, numbered as in the binary encoding */
#define rAX		0
#define rCX		1
#define rDX		2
#define rBX		3
#define rSP		4
#define rBP		5
#define rSI		6
#define rDI		7
#define r8		8
#define r9		9
#define r10		10
#define r11		11
#define r14		14
#define r15		15
#define NOREG		-1

/* And these are the floating point registers */
#define rF0		0x10
#define rF1		0x11
#define rF2		0x12
#define rF3		0x13
#define rF4		0x14
#define rF5		0x15

struct _vmreg
     reg_i0 = { "I0", rAX },
     reg_i1 = { "I1", rCX },
     reg_i2 = { "I2", rDX },
     reg_f0 = { "F0", rF0 },
     reg_f1 = { "F1", rF1 },
     reg_f2 = { "F2", rF2 },
     reg_f3 = { "F3", rF3 },
     reg_f4 = { "F4", rF4 },
     reg_f5 = { "F5", rF5 },
     reg_sp = { "BASE", rSP };

#ifndef M64X32

/* Standard x86 port, using x87 for floating point by default */

struct _vmreg
     reg_v0 = { "V0", rBX },
     reg_v1 = { "V1", rSI },
     reg_v2 = { "V2", rDI },
     reg_v3 = { "V3", rBP };

/* Register layout for use by JIT client */
const int vm_nvreg = 4, vm_nireg = 7;
const vmreg vm_ireg[] = {
     &reg_v0, &reg_v1, &reg_v2, &reg_v3,  /* Callee-save */
     &reg_i0, &reg_i1, &reg_i2            /* Caller-save */
};

#else

/* A native amd64 port (enabled with M64X32) that still uses 32-bit 
   addresses.  Uses SSE for floating point.

   * We rely on the host software to supply an implementation of vm_alloc()
     that allocates storage in the bottom 4GB of the address space.  This
     is easily achieved on Linux with the MAP_32BITS flag to mmap, and on
     Windows with an undocumented API call.

   * We still use x87 for floating point.

   * All addresses that are passed to the code generation interface must
     fit in 32 bits.  That means global data storage needs to be in the bottom
     4GB of memory.  For function addresses, we use trampolines, so that the
     CALL instruction needs only a 32-bit address.  It's up to the host 
     software to create and manage these trampolines.

   Registers 12 and 13 are hard to encode as index registers, so we 
   don't bother with them.  On Windows, rSI and rDI are
   preserved across calls, so we could have made nvreg = 6; it makes
   no difference to the Kieko JIT.  We do, however, respect the
   calling convention by saving those registers in the frame. */

struct _vmreg
     reg_i3 = { "I3", rSI },
     reg_i4 = { "I4", rDI },
     reg_i5 = { "I5", r8 },
     reg_i6 = { "I6", r9 },
     reg_i7 = { "I7", r10 },
     reg_i8 = { "I8", r11 },
     reg_v0 = { "V0", r14 },
     reg_v1 = { "V1", r15 },
     reg_v2 = { "V2", rBX },
     reg_v3 = { "V3", rBP };

const int vm_nvreg = 4, vm_nireg = 13;
const vmreg vm_ireg[] = {
     &reg_v0, &reg_v1, &reg_v2, &reg_v3,   /* Callee-save */
     &reg_i0, &reg_i1, &reg_i2, &reg_i3,   /* Caller-save */
     &reg_i4, &reg_i5, &reg_i6, &reg_i7, &reg_i8
}; 

#endif

const int vm_nfreg = 5;
const vmreg vm_freg[] = {
     &reg_f0, &reg_f1, &reg_f2, /* Floating point for all variants */
     &reg_f3, &reg_f4
};

const vmreg vm_ret = &reg_i0, vm_base = &reg_sp;

#define register(r) ((r)&0x7)

/* The first four registers have 8-bit fields like AL */
#define is8bit(r) ((r) <= rBX)

/* Registers 8-15 require a REX prefix */
#define isrex(r) (((r)&0x8) != 0)

/* Floating point registers are labelled with bit 0x10 */
#define isfloat(r) (((r)&0x10) != 0)

/* DEBUGGING OUTPUT: for consistency with the layout of VM code,
   instructions are displayed with the destination at the left: thus
   for a := b - c, we would produce "mov rAX, rBX; sub rAX, rCX".  This is
   different from the AT&T syntax normally used in Unix, and more like
   the Intel syntax.  We display loads and stores using the syntax
   "movl rAX, 12(rBP)" for a load and "movl 16(rBP), rAX" for a store,
   unlike the monstrous notation used by Intel. */

#ifdef DEBUG
static char *_regname[] = {
     "none",
     "rAX", "rCX", "rDX", "rBX", "rSP", "rBP", "rSI", "rDI",
     "r8",  "r9",  "r10", "r11", "r12", "r13", "r14", "r15",
     "rF0", "rF1", "rF2", "rF3", "rF4", "rF5"
};

static char **regname = &_regname[1];

static char *fmt_addr(int rb, int imm, int rx, int s) {
     static char buf[32], sbuf[16];

     if (rb == NOREG && rx == NOREG)
          sprintf(buf, "%s", fmt_val(imm));
     else {
          char *mmm = (imm != 0 ? fmt_val(imm) : "");

          if (rx == NOREG)
               sprintf(buf, "%s(%s)", mmm, regname[rb]);
          else {
               if (s == 0)
                    strcpy(sbuf, "");
               else
                    sprintf(sbuf, "<<%d", s);
        
               if (rb == NOREG)
                    sprintf(buf, "%s(%s%s)", mmm, regname[rx], sbuf);
               else
                    sprintf(buf, "%s(%s+%s%s)", mmm, regname[rb],
                            regname[rx], sbuf);
          }
     }
     
     return buf;
}

/* When Thunder is compiled for debugging, numeric opcodes are accompanied
   by textual mnemonics as they are passed around, and the act of generating
   an instruction is optionally augmented with printing the instruction in
   assembly-language form.  Depending on whether DEBUG is defined, the macro
   MNEM(mnem, op) expands to either the pair of arguments "mnem, op" or just
   "op" on its own.  Other macros provide declarations for corresponding
   formal parameters of code-generating routines, etc. */

#define MNEM(mnem, op) mnem, op
#define OPDECL const char *mnem, int op
#define OP mnem, op

#define MNEM2(mnem, op, op2) mnem, op, op2
#define OPDECL2 const char *mnem, int op, int op2
#define OP2 mnem, op, op2

#define OPDECL_(suff) const char *mnem_##suff, int op_##suff
#define OP_(suff) mnem_##suff, op_##suff

#define OPDECL2_(suff) const char *mnem_##suff, int op_##suff, int op2_##suff
#define OP2_(suff) mnem_##suff, op_##suff, op2_##suff

#define ifdebug(text) text,

#else

#define MNEM(mnem, op) op
#define OPDECL int op
#define OP op

#define MNEM2(mnem, op, op2) op, op2
#define OPDECL2 int op, int op2
#define OP2 op, op2

#define OPDECL_(suff) int op_##suff
#define OP_(suff) op_##suff

#define OPDECL2_(suff) int op_##suff, int op2_##suff
#define OP2_(suff) op_##suff, op2_##suff

#define ifdebug(text)

#endif


/* Instructions */

#define pfx(a, b) (((b)<<8)|(a))

#define REX		0x40
#define REX_W           0x48
#define REX_R		0x44
#define REX_X		0x42
#define REX_B		0x41

/* ALU operation codes for the 386 */
#define opADD 		MNEM("add", 0) /* Binary */
#define opOR 		MNEM("or", 1)
#define opAND		MNEM("and", 4)
#define opSUB 		MNEM("sub", 5)
#define opXOR 		MNEM("xor", 6)
#define opCMP 		MNEM("cmp", 7)

#define opNOT 		MNEM("not", 2)  /* Unary */
#define opNEG 		MNEM("neg", 3)

/* Condition codes for branches */
#define opB 		MNEM("b", 2)    /* unsigned < */
#define opAE 		MNEM("ae", 3)	/* unsigned >= */
#define opE 		MNEM("e", 4)    /* = */
#define opNE 		MNEM("ne", 5)	/* != */
#define opBE 		MNEM("be", 6)	/* unsigned <= */
#define opA 		MNEM("a", 7)    /* unsigned > */
#define opS 		MNEM("s", 8)    /* negative */
#define opNS 		MNEM("ns", 9)	/* non-negative */
#define opP             MNEM("p", 10)	/* parity bit */
#define opNP            MNEM("np", 11)  /* negated parity bit */
#define opL 		MNEM("l", 12)	/* signed < */
#define opGE 		MNEM("ge", 13)	/* signed >= */
#define opLE 		MNEM("le", 14)	/* signed <= */
#define opG 		MNEM("g", 15)	/* signed > */
	
/* Operation codes for shifts */
#define opROR 		MNEM("ror", 1)
#define opSHL 		MNEM("shl", 4)
#define opSHR 		MNEM("shr", 5)
#define opSAR 		MNEM("sar", 7)

/* Floating point operation codes (387) */
#define opFADD 		MNEM("fadd", 0)
#define opFMUL 		MNEM("fmul", 1)
#define opFSUB 		MNEM("fsub", 4)
#define opFSUBR 	MNEM("fsubr", 5)
#define opFDIV 		MNEM("fdiv", 6)
#define opFDIVR 	MNEM("fdivr", 7)

#define xFLOP_0 	0xd8		// Flop with result in ST(0)
#define xFLOP_r 	0xdc		// Flop with result in ST(r)
#define xFLOPP_r 	0xde		// Flop with result popped to ST(r)

#define xSHIFT_r	0xd3    	// Shift by register
#define xSHIFT_1 	0xd1		// Shift by 1
#define xSHIFT_i 	0xc1		// Shift by a constant

/* Floating point instructions (387) */
#define opFCHS 		MNEM("fchs", pfx(0xd9, 0xe0))
#define opFLD_r 	MNEM("fld", pfx(0xd9, 0xc0))
#define opFST_r 	MNEM("fst", pfx(0xdd, 0xd0))
#define opFSTP_r 	MNEM("fstp", pfx(0xdd, 0xd8))
#define opFUCOMI_r 	MNEM("fucomi", pfx(0xdb, 0xe8))
#define opFUCOMIP_r 	MNEM("fucomip", pfx(0xdf, 0xe8))
#define opFLDZ		MNEM("fldz", pfx(0xd9, 0xee))

#define opFLDS_m	MNEM2("flds", 0xd9, 0)
#define opFLDL_m	MNEM2("fldl", 0xdd, 0)
#define opFSTS_m 	MNEM2("fsts", 0xd9, 2)
#define opFSTL_m 	MNEM2("fstl", 0xdd, 2)
#define opFSTPS_m	MNEM2("fstps", 0xd9, 3)
#define opFSTPL_m	MNEM2("fstpl", 0xdd, 3)
#define opFILDL_m 	MNEM2("fildl", 0xdb, 0)
#define opFISTTPS_m	MNEM2("fisttps", 0xdb, 1)

/* Floating point (SSE2) */
#define SSE_S(x) pfx(0xf3, pfx(0x0f, x))
#define SSE_D(x) pfx(0xf2, pfx(0x0f, x))

#define opMOVSS_r       MNEM("movss", SSE_S(0x10))
#define opMOVSS_m       MNEM("movss", SSE_S(0x11))
#define opMOVSD_r       MNEM("movsd", SSE_D(0x10))
#define opMOVSD_m       MNEM("movsd", SSE_D(0x11))
#define opMOVD_r        MNEM("movd", pfx(0x66, pfx(0x0f, 0x6e)))
#define opMOVD_m        MNEM("movd", pfx(0x66, pfx(0x0f, 0x7e)))
#define opMOVQ_r        MNEM("movq", pfx(0x66, pfx(REX_W, pfx(0x0f, 0x6e))))
#define opMOVQ_m        MNEM("movq", pfx(0x66, pfx(REX_W, pfx(0x0f, 0x7e))))

#define opADDSS         MNEM("addss", SSE_S(0x58))
#define opADDSD         MNEM("addsd", SSE_D(0x58))
#define opSUBSS         MNEM("subss", SSE_S(0x5c))
#define opSUBSD         MNEM("subsd", SSE_D(0x5c))
#define opMULSS         MNEM("mulss", SSE_S(0x59))
#define opMULSD         MNEM("mulsd", SSE_D(0x59))
#define opDIVSS         MNEM("divss", SSE_S(0x5e))
#define opDIVSD         MNEM("divsd", SSE_D(0x5e))

#define opUCOMISS       MNEM("ucomiss", pfx(0x0f, 0x2e))
#define opUCOMISD       MNEM("ucomisd", pfx(0x66, pfx(0x0f, 0x2e)))
#define opXORPS         MNEM("xorps", pfx(0x0f, 0x57))
#define opXORPD         MNEM("xorpd", pfx(0x66, pfx(0x0f, 0x57)))
#define opPXOR          MNEM("pxor",  pfx(0x66, pfx(0x0f, 0xef)))

#define opCVTSI2SS      MNEM("cvtsi2ss", SSE_S(0x2a))
#define opCVTSI2SD      MNEM("cvtsi2sd", SSE_D(0x2a))
#define opCVTTSS2SI     MNEM("cvttss2si", SSE_S(0x2c))
#define opCVTTSD2SI     MNEM("cvttsd2si", SSE_D(0x2c))
#define opCVTSS2SD      MNEM("cvtss2sd", SSE_S(0x5a))
#define opCVTSD2SS      MNEM("cvtsd2ss", SSE_D(0x5a))

/* Integer moves */
#define opMOVL_r 	MNEM("mov", 0x8b) // Move to register
#define opMOVZWL_r 	MNEM("movzwl", pfx(0x0f, 0xb7))
#define opMOVSWL_r 	MNEM("movswl", pfx(0x0f, 0xbf))
#define opMOVZBL_r 	MNEM("movzbl", pfx(0x0f, 0xb6))
#define opMOVSBL_r 	MNEM("movsbl", pfx(0x0f, 0xbe))
#define opMOVSXD_r      MNEM("movsxd", pfx(REX_W, 0x63))
#define opMOVL_m 	MNEM("mov", 0x89) // Move to memory
#define opMOVW_m 	MNEM("movw", pfx(0x66, 0x89))
#define opMOVB_m 	MNEM("movb", 0x88)
#define opMOVL_i 	MNEM("mov", 0xb8) // Load immediate into register
#define opMOVQ_i        MNEM("movq", pfx(REX_W, 0xb8))
#define opLEA		MNEM("lea", 0x8d) // Load effective address
#define opLEA64		MNEM("lea64", pfx(REX_W, 0x8d))

#define opIMUL_i 	MNEM("imul", 0x69) // Integer multiply
#define opIMUL_r	MNEM("imul", pfx(0x0f, 0xaf))
#ifndef M64X32
// On amd64, these are usurped by the REX prefixes
#define opINC_r		MNEM("inc", 0x40)
#define opDEC_r		MNEM("dec", 0x48)
#endif
#define opINC_m		MNEM2("inc", 0xff, 0)
#define opDEC_m		MNEM2("dec", 0xff, 1)     
#define opPUSH_r	MNEM("push", 0x50)
#define opPUSH_i	MNEM("push", 0x68)
#define opPOP		MNEM("pop", 0x58)
#define opRET		MNEM("ret", 0xc3)
#define opJMP		MNEM2("jmp", 0xff, 4)
#define opJMP_i		MNEM("jmp", 0xe9)
#define opCALL		MNEM2("call", 0xff, 2)
#define opCALL_i	MNEM("call", 0xe8)
#define opTEST		MNEM("test", 0x85)
#define opTESTq		MNEM("testq", pfx(REX_W, 0x85))
#define opXCHG		MNEM("xchg", 0x87)

/* Families of opcodes: you can write, e.g., ALUOP(opSUB) to get the
   effect of MNEM("sub", (5<<3)|0x3) when opSUB is MNEM("sub", 5).  Or
   ALUOP64_i(opSUB) for MNEM2("subq", pfx(REX_W, 0x81), 5).  In these
   definitions, the timing of macro expansion plays a crucial role in
   determining when a comma is part of an argument and when it is a
   separator between arguments. */

#ifdef DEBUG
#define family(f, mnem, op) f(mnem, op)
#define family2(f, mnem, op, op2) f(mnem, op, op2)
#else
#define family(f, op) f(___, op)
#define family2(f, op, op2) f(___, op, op2)
#endif

#define ALUOP(op) 	family(__ALU, op)
#define __ALU(mnem, op) MNEM(mnem, (op<<3)|0x3)

#define ALUOP64(op) 	family(__ALUq, op)
#define __ALUq(mnem, op) MNEM(mnem "q", pfx(REX_W, (op<<3)|0x3))

#define ALUOP_i(op) 	family(__ALU_i, op)
#define __ALU_i(mnem, op) MNEM2(mnem, 0x81, op)

#define ALUOP64_i(op) 	family(__ALUq_i, op)
#define __ALUq_i(mnem, op) MNEM2(mnem "q", pfx(REX_W, 0x81), op)

#define MONOP(op) 	family(__MONOP, op)
#define __MONOP(mnem, op) MNEM2(mnem, 0xf7, op)

#define MONOP64(op) 	family(__MONOPq, op)
#define __MONOPq(mnem, op) MNEM2(mnem "q", pfx(REX_W, 0xf7), op)

#define CONDJ(op) 	family(__CONDJ, op)
#define __CONDJ(mnem, op) MNEM("j" mnem, pfx(0x0f, 0x80|op))

#define SETCC(op) 	family(__SETCC, op)
#define __SETCC(mnem, op) MNEM2("set" mnem, pfx(0x0f, 0x90|op), 0)

#define REX_(op)	family(__REX, op)
#define __REX(mnem, op) MNEM(mnem, pfx(REX, op))

#define REX2_(op)	family2(__REX2, op)
#define __REX2(mnem, op, op2) MNEM2(mnem, pfx(REX, op), op2)

#define REXW_(op)	family(__REXW, op)
#define __REXW(mnem, op) MNEM(mnem "q", pfx(REX_W, op))

#define REXW2_(op)	family2(__REXW2, op)
#define __REXW2(mnem, op, op2) MNEM2(mnem "q", pfx(REX_W, op), op2)


/* Instruction formats */

code_addr ibeg;

/* opcode -- one, two or three opcode bytes */
static void opcode(unsigned op) {
     ibeg = pc;
     while (op != 0) {
          byte(op & 0xff);
          op >>= 8;
     }
}

#ifndef M64X32
#define check_rex(r, p)
#else
#define check_rex(r, p) if (isrex(r)) rex(p)

#define isprefix(x) ((x) == 0x66 || (x) == 0x67 || (x) == 0xf2 || (x) == 0xf3)

#define ADDR32 0x67

/* prefix -- insert prefix byte */
static void prefix(int pfx) {
     memmove(ibeg+1, ibeg, pc-ibeg);
     *ibeg = pfx;
     pc++;
}

/* rex -- insert REX prefix */
static void rex(int pfx) {
     code_addr p = ibeg;

     // Skip over any legacy prefix
     while (isprefix(*p)) p++;

     // Make space for a REX byte if needed
     if ((*p&0xf0) != REX) {
          memmove(p+1, p, pc-p);
          *p = REX;
          pc++;
     }

     // Install the prefix
     *p |= pfx;
}
#endif

/* packreg -- modify opcode with register */
static void packreg(int reg) {
     modify(register(reg));
     check_rex(reg, REX_B);
}

/* modrm -- a (mode, reg, r/m) triple */
static void modrm(int mode, int reg, int rm) {
     byte((mode<<6) | (register(reg)<<3) | register(rm));
     check_rex(reg, REX_R);
     check_rex(rm, REX_B);
}

/* sib -- a (scale, index, base) triple */
static void sib(int scale, int index, int base) {
     byte((scale<<6) | (register(index)<<3) | register(base));
     check_rex(index, REX_X);
     check_rex(base, REX_B);
}

#define signed8(x) ((x) >= -128 && (x) < 128)

/* memory -- memory operand */
static void memory(int ra, int rb, int d, int rx, int s) {
     /* Encode the register ra and the memory address [rb+d+rx<<s]

     Most of the time (with no indexing), we need a (mode, reg, r/m) 
     triple, where mode determines the size of displacement 
     (0 = none, 1 = byte, 2 = word), and r/m is the base register.
     but there are special cases involving the registers rSP=4
     and rBP=5:

     A: (0,ra,rb)		[rb]			rb != rBP, rSP
     B: (0,ra,4)  (s,rx,rb)	[rb + rx<<s]		rx != rSP; rb != rBP
     C: (0,ra,4)  (s,rx,5)  d32	[d32 + rx<<s]		rx != rSP
     D: (0,ra,4)  (s,4,rb) 	[rb]			rb != rBP
     E: (0,ra,4)  (s,4,5)   d32	[d32]	
     F: (0,ra,5)            d32	[d32] on i386; [pc + d32] on amd64
     G: (1,ra,4)  (s,rx,rb) d8	[rb + d8 + rx<<s]	rx != rSP
     H: (1,ra,4)  (s,4,rb)  d8	[rb + d8]
     I: (1,ra,rb)           d8	[rb + d8]		rb != rSP
     J: (2,ra,4)  (s,rx,rb) d32	[rb + d32 + rx<<s]	rx != rSP
     K: (2,ra,4)  (s,4,rb)  d32	[rb + d32]*
     L: (2,ra,rb)           d32	[rb + d32]*		rb != rSP 

     (On amd64, registers 12 and 13 also trigger the special cases,
     but we don't use them.) 

     Where it's possible that a negative offset is taken from a
     register, we use an addr32 prefix to ensure the top 32 bits
     of the address are zero on amd64. */

#ifdef M64X32
#define SXTOFF prefix(ADDR32)
#else
#define SXTOFF (void) 0
#endif

     if (rb == NOREG && s == 0) {
          rb = rx; rx = NOREG;
     }

     if (rb == NOREG && rx == NOREG) {
	  // Absolute [d], case {F}
#ifndef M64X32
	  modrm(0, ra, 5), word(d);
#else
          // Use PC-relative addressing
          modrm(0, ra, 5), word((code_addr) (address) d - pc - 4);
          // Was: modrm(0, ra, 4), sib(0, 4, 5), word(d);
#endif
     } else if (rb == rSP && rx == NOREG) {
	  // Special cases to use rSP as base [rSP+d]
	  if (d == 0)
	       modrm(0, ra, 4), sib(0, 4, rSP); // {E}
	  else if (signed8(d))
	       modrm(1, ra, 4), sib(0, 4, rSP), byte(d); // {H}, untested
	  else
	       SXTOFF, modrm(2, ra, 4),
                    sib(0, 4, rSP), word(d); // {K}, untested
     } else if (rx == NOREG) {
	  // Base + Displacement [rb+d]
	  if (d == 0 && rb != rBP)
	       modrm(0, ra, rb); // {A}
	  else if (signed8(d))
	       modrm(1, ra, rb), byte(d); // {I}
	  else
	       SXTOFF, modrm(2, ra, rb), word(d); // {L}
     } else if (rb == NOREG) {
          // Baseless addressing [d+rx<<s]
          modrm(0, ra, 4), sib(s, rx, 5), word(d); // {C}
     } else {
          // Scaled addressing [rb+d+rx<<s]
          if (d == 0 && rb != rBP)
               SXTOFF, modrm(0, ra, 4), sib(s, rx, rb); // {B}
          else if (signed8(d))
               SXTOFF, modrm(1, ra, 4), sib(s, rx, rb), byte(d); // {G}
          else
               SXTOFF, modrm(2, ra, 4), sib(s, rx, rb), word(d); // {J}
     }
}


/* ASSEMBLY ROUTINES for various instructions formats.  
   These each produce debugging ouput.

   An instruction can have:
   * a second opcode (2)
   * a register packed with the opcode (reg)
   * up to two registers in an additional byte (r, rr)
   * an 8/32 bit immediate (i8)
   * a 32 bit immediate (i)
   * a memory address (m)
   * a branch target (lab) */

/* instr -- fixed instruction, opcode only */
static void instr(OPDECL) {
     vm_debug2("%s", mnem);
     opcode(op);
     vm_done();
}

/* instr_reg -- opcode packed with register */
static void instr_reg(OPDECL, int reg) {
     vm_debug2("%s %s", mnem, regname[reg]);
     opcode(op), packreg(reg);
     vm_done();
}

#define x_byte 0x02

#ifndef M64X32
/* instr_imm -- opcode plus 8/32 bit immediate */
static void instr_imm(OPDECL, int imm) {
     vm_debug2("%s #%s", mnem, fmt_val(imm));
     if (signed8(imm))
          opcode(op), modify(x_byte), byte(imm);
     else
          opcode(op), word(imm);
     vm_done();
}
#endif

/* instr_regi32 -- opcode packed with register plus a 32-bit immediate */
static void instr_regi32(OPDECL, int r, int imm) {
     vm_debug2("%s %s, #%s", mnem, regname[r], fmt_val(imm));
     opcode(op), packreg(r), word(imm);
     vm_done();
}

/* instr_regi64 -- opcode packed with register plus a 64-bit immediate */
static void instr_regi64(OPDECL, int r, uint64 imm) {
     vm_debug2("%s %s, #%s", mnem, regname[r], fmt_val64(imm));
     opcode(op), packreg(r), qword(imm);
     vm_done();
}

/* instr_rr -- opcode plus two registers */
static void instr_rr(OPDECL, int r1, int r2) {
     vm_debug2("%s %s, %s", mnem, regname[r1], regname[r2]);
     opcode(op), modrm(3, r1, r2);
     vm_done();
}

/* General form of instr2_r for shifts and floating point ops */
static void instr2_fmt(ifdebug(char *fmt) OPDECL2, int r) {
     vm_debug2(fmt, mnem, regname[r]);
     opcode(op), modrm(3, op2, r);
     vm_done();
}

/* instr2_r -- two opcodes and a register */
#define instr2_r(ispec, r) instr2_fmt(ifdebug("%s %s") ispec, r)

/* instr2_ri8 -- twin opcode plus register and 8-bit immediate */
static void instr2_ri8(OPDECL2, int rm, int imm) {
     vm_debug2("%s %s, #%s", mnem, regname[rm], fmt_val(imm));
     opcode(op), modrm(3, op2, rm), byte(imm);
     vm_done();
}

/* instruction with 2 opcodes and 8/32 bit immediate */
static void instr2_ri(OPDECL2, int rm, int imm) {
     vm_debug2("%s %s, #%s", mnem, regname[rm], fmt_val(imm));
     if (signed8(imm))
	  opcode(op), modify(x_byte), modrm(3, op2, rm), byte(imm);
     else
	  opcode(op), modrm(3, op2, rm), word(imm);
     vm_done();
}

/* instruction with 2 registers and 8/32 bit immediate */
static void instr_rri(OPDECL, int reg, int rm, int imm) {
     vm_debug2("%s %s, %s, #%s", mnem, regname[reg], regname[rm], fmt_val(imm));
     if (signed8(imm))
	  opcode(op), modify(x_byte), modrm(3, reg, rm), byte(imm);
     else
	  opcode(op), modrm(3, reg, rm), word(imm);
     vm_done();
}

/* instr_rm -- register and memory operand with indexing */
static void instr_rm(OPDECL, int rt, int rs, int imm, int rx, int s) {
     vm_debug2("%s %s, %s", mnem, regname[rt], fmt_addr(rs, imm, rx, s));
     opcode(op), memory(rt, rs, imm, rx, s);
     vm_done();
}

/* instr_st -- store with indexing */
static void instr_st(OPDECL, int rt, int rb, int imm, int ri, int s) {
     vm_debug2("%s %s, %s", mnem, fmt_addr(rb, imm, ri, s), regname[rt]);
     opcode(op), memory(rt, rb, imm, ri, s);
     vm_done();
}

/* instr_tgt -- opcode plus branch target */
static void instr_tgt(OPDECL, code_addr tgt) {
     code_addr r;
     vm_debug2("%s %s", mnem, fmt_val((unsigned) (address) tgt));
     assert(tgt != NULL);
     opcode(op), r = pc, word(0);
     vm_patch(r, tgt);
     vm_done();
}

/* instr_lab -- opcode plus label */
static void instr_lab(OPDECL, vmlabel lab) {
     code_addr r;
     vm_debug2("%s %s", mnem, fmt_lab(lab));
     opcode(op), r = pc, word(0);
     vm_branch(BRANCH, r, lab);
     vm_done();
}

#ifndef M64X32
/* instr_abs -- opcode plus reg and absolute label */
static void instr_abs(OPDECL, int rd, vmlabel lab) {
     code_addr r;
     vm_debug2("%s %s, #%s", mnem, regname[rd], fmt_lab(lab));
     opcode(op), packreg(rd), r = pc, word(0);
     vm_branch(ABS, r, lab);
     vm_done();
}
#else
/* instr_rel -- opcode plus reg and relative label */
static void instr_rel(OPDECL, int rd, vmlabel lab) {
     code_addr r;
     vm_debug2("%s %s, #%s", mnem, regname[rd], fmt_lab(lab));
     opcode(op), modrm(0, rd, 5), r = pc, word(0);
     vm_branch(BRANCH, r, lab);
     vm_done();
}
#endif

#ifdef USE_SSE
/* instr_sr -- opcode plus two registers, swapped */
static void instr_sr(OPDECL, int r1, int r2) {
     vm_debug2("%s %s, %s", mnem, regname[r2], regname[r1]);
     opcode(op), modrm(3, r1, r2);
     vm_done();
}
#endif

#ifndef USE_SSE
/* instr_fpm -- floating point load or store */
static void instr_fpm(OPDECL2, int rs, int imm, int rx, int s) {
     vm_debug2("%s %s", mnem, fmt_addr(rs, imm, rx, s));
     opcode(op), memory(op2, rs, imm, rx, s);
     vm_done();
}
#endif


/* SPECIFIC INSTRUCTIONS */

#define add_i(rd, imm)	  instr2_ri(ALUOP_i(opADD), rd, imm)
#define sub_i(rd, imm)	  instr2_ri(ALUOP_i(opSUB), rd, imm)
#define add64_i(rd, imm)  instr2_ri(ALUOP64_i(opADD), rd, imm)
#define sub64_i(rd, imm)  instr2_ri(ALUOP64_i(opSUB), rd, imm)

#define push_r(r)	  instr_reg(opPUSH_r, r)
#define push_i(imm)	  instr_imm(opPUSH_i, imm)
#define pop(r)		  instr_reg(opPOP, r)
#define fld_r(r) 	  instr_reg(opFLD_r, r)
#define fst_r(r) 	  instr_reg(opFST_r, r)
#define fstp_r(r) 	  instr_reg(opFSTP_r, r)

#ifndef M64X32
#define inc_r(r)          instr_reg(opINC_r, r)
#define dec_r(r)          instr_reg(opDEC_r, r)
#else
#define inc_r(r)  	  instr2_r(opINC_m, r)
#define dec_r(r)          instr2_r(opDEC_m, r)
#endif

/* shift instruction with one operand in CL */
static void shift2cl_r(OPDECL, int r) {
     instr2_fmt(ifdebug("%s %s, cl") MNEM2(mnem, xSHIFT_r, op), r);
}

/* shift by constant */
static void shift2_i(OPDECL, int rd, int imm) {
     if (imm == 1)
	  instr2_r(MNEM2(mnem, xSHIFT_1, op), rd);
     else
	  instr2_ri8(MNEM2(mnem, xSHIFT_i, op), rd, imm);
}

/* SYNTHETIC INSTRUCTIONS */

/* Optional move */
static void move_r(OPDECL, int rd, int rs) {
     if (rd != rs) 
          instr_rr(OP, rd, rs);
}

#define move(rd, rs)    move_r(opMOVL_r, rd, rs)
#define move64(rd, rs)  move_r(REXW_(opMOVL_r), rd, rs)

/* Load constant */
static void move_i(int rd, int imm) {
     if (imm == 0)
          instr_rr(ALUOP(opXOR), rd, rd);
     else
          instr_regi32(opMOVL_i, rd, imm);
}

#define move_i64(rd, imm)  instr_regi64(opMOVQ_i, rd, imm)

/* Swap registers */
#define swap(r1, r2) instr_rr(opXCHG, r1, r2)

/* Load word */
#define load(rd, rs, off)  instr_rm(opMOVL_r, rd, rs, off, NOREG, 0)

/* shift instructions (3 registers) */
static void shift3_r(OPDECL, int rd, int rs1, int rs2) {
     int rx = rd;               /* Working register */

     if (rd == rCX || (rd != rs1 && rd == rs2)) {
	  rx = (rs2 == rAX ? rDX : rAX);
	  push_r(rx);
     }

     move(rx, rs1);
     if (rs2 == rCX)
          shift2cl_r(OP, rx);
     else {
          push_r(rCX); move(rCX, rs2); 
          shift2cl_r(OP, rx); pop(rCX);
     }

     if (rx != rd) {
          move(rd, rx); pop(rx);
     }
}

/* shift with 2 registers and an immediate */
static void shift3_i(OPDECL, int rd, int rs, int imm) {
     move(rd, rs);
     shift2_i(OP, rd, imm);
}

/* store character from any register */
static void storec(int rt, int rb, int imm, int rx, int s) {
     if (is8bit(rt))
          instr_st(opMOVB_m, rt, rb, imm, rx, s);
     else {
#ifdef M64X32
          instr_st(REX_(opMOVB_m), rt, rb, imm, rx, s);
#else
	  int rz = ( (rb != rAX && rx != rAX) ? rAX :
                     (rb != rDX && rx != rDX) ? rDX : rCX );
	  push_r(rz); move(rz, rt);
          if (rb == rSP) imm += 4; // Compensate for push
	  instr_st(opMOVB_m, rz, rb, imm, rx, s);
	  pop(rz);
#endif
     }
}

/* unary ALU operation (2 registers) */
static void gmonop(OPDECL2, OPDECL_(move), int rd, int rs) {
     move_r(OP_(move), rd, rs);
     instr2_r(OP2, rd);
}

#define monop(op, rd, rs) \
     gmonop(op, opMOVL_r, rd, rs)
#define neg64(rd, rs) \
     gmonop(MONOP64(opNEG), REXW_(opMOVL_r), rd, rs)

/* commutative ALU operation (3 register operands) */
static void gcommute(OPDECL, OPDECL_(move), int rd, int rs1, int rs2) {
     if (rd == rs2)
	  instr_rr(OP, rd, rs1);
     else {
	  move_r(OP_(move), rd, rs1);
	  instr_rr(OP, rd, rs2);
     }
}

#define commute(op, rd, rs1, rs2) \
     gcommute(op, opMOVL_r, rd, rs1, rs2)
#define commute64(op, rd, rs1, rs2) \
     gcommute(op, REXW_(opMOVL_r), rd, rs1, rs2)

/* subtract (3 registers) */
static void gsubtract(OPDECL, OPDECL2_(neg), OPDECL_(move),
                      int rd, int rs1, int rs2) {
     if (rd == rs2) {
          instr_rr(OP, rd, rs1);
	  instr2_r(OP2_(neg), rd);
     } else {
	  move_r(OP_(move), rd, rs1);
	  instr_rr(OP, rd, rs2);
     }
}

#define subtract(rd, rs1, rs2) \
     gsubtract(ALUOP(opSUB), MONOP(opNEG), opMOVL_r, rd, rs1, rs2)
#define subtract64(rd, rs1, rs2) \
     gsubtract(ALUOP64(opSUB), MONOP64(opNEG), REXW_(opMOVL_r), rd, rs1, rs2)

/* binary ALU operation, 2 registers + immediate */
#define binop3_i(op, rd, rs, imm) \
     move(rd, rs), instr2_ri(op, rd, imm)

/* Conditional branch, 2 registers */
#define branch_r(op, rs1, rs2, lab) \
     instr_rr(ALUOP(opCMP), rs1, rs2), instr_lab(op, lab)
#define branch64_r(op, rs1, rs2, lab) \
     instr_rr(ALUOP64(opCMP), rs1, rs2), instr_lab(op, lab)

/* Set flags by comparing register and immediate */
static void gcomp_i(OPDECL2_(cmp), OPDECL_(test), int rs, int imm) {
     if (imm == 0)
	  instr_rr(OP_(test), rs, rs);
     else
	  instr2_ri(OP2_(cmp), rs, imm);
}

/* Integer compare */
#define comp_i(rs, imm) \
     gcomp_i(ALUOP_i(opCMP), opTEST, rs, imm)
#define comp64_i(rs, imm) \
     gcomp_i(ALUOP64_i(opCMP), opTESTq, rs, imm)

/* Conditional branch, register + immediate */
#define branch_i(op, rs, imm, lab) \
     comp_i(rs, imm), instr_lab(op, lab)
#define branch64_i(op, rs, imm, lab) \
     comp64_i(rs, imm), instr_lab(op, lab)

static void setcc_r(OPDECL2, int rd) {
     if (is8bit(rd)) {
	  instr2_r(OP2, rd); /* Compute boolean result */
          instr_rr(opMOVZBL_r, rd, rd); /* Zero-extend */
     } else {
#ifdef M64X32
          instr2_r(REX2_(OP2), rd);
          instr_rr(REX_(opMOVZBL_r), rd, rd);
#else
	  push_r(rAX);
	  instr2_r(OP2, rAX); /* Compute boolean result */
	  instr_rr(opMOVZBL_r, rd, rAX); /* Move and zero-extend */
	  pop(rAX);
#endif
     }
}

/* setcc_equal -- boolean result if equal and not NaN */
void setcc_equal(int r) {
     vmlabel lab1 = vm_newlab();
     setcc_r(SETCC(opE), r);
     instr_lab(CONDJ(opNP), lab1);
     move_i(r, 0);
     vm_label(lab1);
}

/* jump_equal -- jump if equal and not NaN */
void jump_equal(vmlabel lab) {
     vmlabel lab1 = vm_newlab();
     instr_lab(CONDJ(opP), lab1);
     instr_lab(CONDJ(opE), lab);
     vm_label(lab1);
}

/* setcc_not_equal -- boolean result if not equal or NaN */
void setcc_not_equal(int r) {
     vmlabel lab1 = vm_newlab();
     setcc_r(SETCC(opNE), r);
     instr_lab(CONDJ(opNP), lab1);
     inc_r(r);
     vm_label(lab1);
}

/* jump_not_equal -- jump if not equal or NaN */
void jump_not_equal(vmlabel lab) {
     instr_lab(CONDJ(opP), lab);
     instr_lab(CONDJ(opNE), lab);
}     

/* comparison with boolean result (2 registers) */
#define compare_r(op, rd, rs1, rs2) \
     instr_rr(ALUOP(opCMP), rs1, rs2), setcc_r(op, rd)
#define compare64_r(op, rd, rs1, rs2) \
     instr_rr(ALUOP64(opCMP), rs1, rs2), setcc_r(op, rd)

/* comparison with boolean result (register + immediate) */
#define compare_i(op, rd, rs, imm) \
     comp_i(rs, imm), setcc_r(op, rd)
#define compare64_i(op, rd, rs, imm) \
     comp64_i(rs, imm), setcc_r(op, rd)


/* FLOATING POINT */

#ifdef USE_SSE
#define if387(x)

#define fmove_s(rd, rs)  move_r(opMOVSS_r, rd, rs)
#define fmove_d(rd, rs)  move_r(opMOVSD_r, rd, rs)
     
static void fneg_s(int rd, int rs) {
     static code_addr mask;

     static unsigned proto[4] = {
          0x80000000, 0, 0, 0
     };

     if (mask == NULL) {
          unsigned *p = (unsigned *) vm_literal_align(16, 16);
          memcpy(p, proto, 16);
          mask = (code_addr) p;
     }

     fmove_s(rd, rs);
     instr_rm(opXORPS, rd, NOREG, (int) (ptr) mask, NOREG, 0);
}

static void fneg_d(int rd, int rs) {
     static code_addr mask;

     static unsigned proto[4] = {
          0, 0x80000000, 0, 0
     };

     if (mask == NULL) {
          unsigned *p = (unsigned *) vm_literal_align(16, 16);
          memcpy(p, proto, 16);
          mask = (code_addr) p;
     }

     fmove_d(rd, rs);
     instr_rm(opXORPD, rd, NOREG, (int) (ptr) mask, NOREG, 0);
}

static void flop3(OPDECL, OPDECL_(move), int rd, int rs1, int rs2) {
     if (rd != rs2) {
          if (rd == rs1)
               instr_rr(OP_(move), rd, rs1);
          instr_rr(OP, rd, rs2);
     } else {
          instr_rr(OP_(move), rF5, rs2);
          instr_rr(OP_(move), rd, rs1);
          instr_rr(OP, rd, rF5);
     }
}
          
#define flop3_s(op, rd, rs1, rs2) \
     flop3(op, opMOVSS_r, rd, rs1, rs2)
#define flop3_d(op, rd, rs1, rs2) \
     flop3(op, opMOVSD_r, rd, rs1, rs2)
          
static void flop3c(OPDECL, OPDECL_(move), int rd, int rs1, int rs2) {
     if (rd == rs2)
          instr_rr(OP, rd, rs1);
     else {
          if (rd != rs1)
               instr_rr(OP_(move), rd, rs1);
          instr_rr(OP, rd, rs2);
     }
}

#define flop3c_s(op, rd, rs1, rs2) \
     flop3c(op, opMOVSS_r, rd, rs1, rs2)
#define flop3c_d(op, rd, rs1, rs2) \
     flop3c(op, opMOVSD_r, rd, rs1, rs2)

#define fcomp_s(rs1, rs2)  instr_rr(opUCOMISS, rs1, rs2)
#define fcomp_d(rs1, rs2)  instr_rr(opUCOMISD, rs1, rs2)

#define fload_s(rt, rb, imm, rx, s)             \
     instr_rm(opMOVSS_r, rt, rb, imm, rx, s)
#define fload_d(rt, rb, imm, rx, s)             \
     instr_rm(opMOVSD_r, rt, rb, imm, rx, s)
#define fstore_s(rt, rb, imm, rx, s)            \
     instr_st(opMOVSS_m, rt, rb, imm, rx, s)
#define fstore_d(rt, rb, imm, rx, s)            \
     instr_st(opMOVSD_m, rt, rb, imm, rx, s)

#define fmove_to_s(rd, rs)    instr_rr(opMOVD_r, rd, rs)
#define fmove_from_s(rd, rs)  instr_sr(opMOVD_m, rs, rd)
#define fmove_to_d(rd, rs)    instr_rr(opMOVQ_r, rd, rs)
#define fmove_from_d(rd, rs)  instr_sr(opMOVQ_m, rs, rd)

#else
#define if387(x) x

/* we treat the 387's stack as a set of ordinary registers,
   using fld and fst instructions to pull things to the top of the stack
   when we need to operate on them.   We do the best we can without trying
   to track register state. */

/* Optional floating-point move */
static void fmove_s(int rd, int rs) {
     if (rd == rs) return;

     if (rs == rF0)
	  fst_r(rd);
     else {
	  fld_r(rs);
	  fstp_r(rd+1);
     }
}

#define fmove_d(rd, rs) fmove_s(rd, rs)

/* Unary floating point, 2 registers */
static void fmonop(OPDECL, int rd, int rs) {
     if (rs == rF0 && rd == rF0)
	  instr(OP);
     else {
	  fld_r(rs); 
	  instr(OP); 
	  fstp_r(rd+1);
     }
}

/* Much confusion surrounds the FSUBR and FDIVR instructions, first
   because the mnemonics in Intel assembly language don't reflect the
   opcodes behind them, and second because of the well-known bug in
   AT&T assemblers.  For FSUB and FSUBR, there are six opcodes for
   register-to-register subtractions:

   opcode	   effect		Intel assembler	     debug output

1  FLOP_0 FSUB     r0 := r0 - ri	FSUB ST(0), ST(i)    fsub F0, Fi	
2  FLOP_0 FSUBR    r0 := ri - r0	FSUBR ST(0), ST(i)   fsubr F0, Fi	
3  FLOP_r FSUB     ri := r0 - ri	FSUBR ST(i), ST(0)*  fsubr Fi, F0
4  FLOP_r FSUBR    ri := ri - r0	FSUB ST(i), ST(0)    fsub Fi, F0	
5  FLOPP_r FSUB    ri := r0 - ri; pop	FSUBRP ST(i), ST(0)  fsubrp Fi, F0	
6  FLOPP_r FSUBR   ri := ri - r0; pop	FSUBP ST(i), ST(0)   fsubp Fi, F0	

   * The AT&T assembler uses "fsub %st(i), %st" here, which ought to mean
     r0 := ri - r0.

   Internally I've given the names FLOP_0, FLOP_r and FLOPP_r to the bytes
   D8, DC and DE respectively, and FSUB and FSUBR to E0 and E8. */

/* Binary floating point, 2 registers */
static void flop2(OPDECL, int op_r, int rd, int rs) {
     if (rd == rF0)
	  /* 1: st0 := st0 op stX */
	  instr2_fmt(ifdebug("%s rF0, %s") MNEM2(mnem, xFLOP_0, op), rs);
     else if (rs == rF0)
	  /* 4: stX := stX op st0 */
	  instr2_fmt(ifdebug("%s %s, rF0") MNEM2(mnem, xFLOP_r, op_r), rd);
     else {
	  /* stX := stX op stY becomes push stY; 6: stX := stX op st0; pop*/
	  fld_r(rs);
          instr2_fmt(ifdebug("%sp %s, rF0") MNEM2(mnem, xFLOPP_r, op_r), rd+1);
     }
}

/* Binary floating point, 3 registers */
static void flop3(OPDECL, OPDECL_(r), int rd, int rs1, int rs2) {
     if (rd == rs1)
          flop2(OP, op_r, rd, rs2);
     else if (rd == rs2)
	  flop2(OP_(r), op, rd, rs1);
     else {
	  /* stX = stY op stZ */
	  fld_r(rs1);
	  flop2(OP, op_r, rF0, rs2+1);
	  fstp_r(rd+1);
     }
}

/* Load float or double from memory */
#define loadf(op, rt, rb, imm, rx, s) \
     instr_fpm(op, rb, imm, rx, s), fstp_r(rt+1)
#define fload_s(rt, rb, imm, rx, s)  loadf(opFLDS_m, rt, rb, imm, rx, s)
#define fload_d(rt, rb, imm, rx, s)  loadf(opFLDL_m, rt, rb, imm, rx, s)

/* Store float or double to memory */
static void storef(OPDECL2, OPDECL2_(p),
                   int rt, int rb, int imm, int rx, int s) {
     if (rt == rF0)
	  instr_fpm(OP2, rb, imm, rx, s);
     else {
	  fld_r(rt); 
	  instr_fpm(OP2_(p), rb, imm, rx, s); 
     }
}

#define fstore_s(rt, rb, imm, rx, s) \
     storef(opFSTS_m, opFSTPS_m, rt, rb, imm, rx, s)
#define fstore_d(rt, rb, imm, rx, s) \
     storef(opFSTL_m, opFSTPL_m, rt, rb, imm, rx, s)

#define fmove_to_s(rd, rs)                      \
     push_r(rs), fload_s(rd, rSP, 0, NOREG, 0), pop(rs)
#define fmove_from_s(rd, rs) \
     /* Make a stack slot, then overwrite it. */ \
     push_r(rd), fstore_s(rs, rSP, 0, NOREG, 0), pop(rd)
#define fmove_to_d(rd, rs) \
     push_r(rs), fload_d(rd, rSP, 0, NOREG, 0), pop(rs)
#define fmove_from_d(rd, rs) \
     push_r(rd), fstore_d(rs, rSP, 0, NOREG, 0), pop(rd)


/* Floating point comparison, setting integer flags */
static void fcomp_d(int rs1, int rs2) {
     if (rs1 == rF0)
	  instr_reg(opFUCOMI_r, rs2);
     else {
	  fld_r(rs1);
	  instr_reg(opFUCOMIP_r, rs2+1);
     }
}
#endif

/* Floating point branch */
#define fbranch_s(op, rs1, rs2, lab) \
     fcomp_s(rs1, rs2), instr_lab(op, lab)
#define fbranch_d(op, rs1, rs2, lab) \
     fcomp_d(rs1, rs2), instr_lab(op, lab)

/* Floating point comparison with boolean result */
#define fcompare_s(op, rd, rs1, rs2) \
     fcomp_s(rs1, rs2), setcc_r(op, rd)
#define fcompare_d(op, rd, rs1, rs2) \
     fcomp_d(rs1, rs2), setcc_r(op, rd)


/* STACK FRAMES */

static int locals;             /* Size of local space */
static int nargs;              /* Effective number of outgoing args */

#ifndef M64X32

/*
On X86, just before a call instruction, the stack layout is like this:

old sp:	incoming args (at sp0+locals+20)
	return address
	saved bp
	saved bx
	saved si
    	saved di
sp0:    local space
	blank space
sp:	outgoing args

Here, sp0 denotes the sp position just after entry, used in implementing
the GETARG instruction.

On the Mac, sp must be 16-byte aligned at this point, so 
nargs + locals + blank + 5 must be a multiple of 4 in words.
*/

static void prep_call(int n) {
#ifdef MACOS
     int blank = 3 - n % 4;
     if (blank > 0) sub_i(rSP, blank * sizeof(int));
     nargs = n + blank;
#else
     nargs = n;
#endif
}

static void post_call(void) {
     if (nargs > 0) 
          add_i(rSP, nargs * sizeof(int));
}

#define arg_r(r)  push_r(r)
#define arg_i(a)  push_i(a)

static void call_r(int ra) {
     instr2_r(opCALL, ra);
     post_call();
}

static void call_a(void *a) {
     instr_tgt(opCALL_i, (code_addr) a);
     post_call();
}     

int vm_prelude(int n, int locs) {
     code_addr entry = pc;
     locals = (locs+3)&~3;
     push_r(rBP); push_r(rBX); push_r(rSI); push_r(rDI); 
     if (locals > 0) sub_i(rSP, locals);
     return (int) entry;
}

static void retn(void) {
     if (locals > 0) add_i(rSP, locals);
     pop(rDI); pop(rSI); pop(rBX); pop(rBP);
     instr(opRET);
}

#else
/*
On Linux/amd64, the frame layout is like this, with 8 byte slots:

old sp:
        return address
        saved rbp
        saved rbx
        saved r15
        saved r14
        saved args if n > 1
        blank space
sp:     locals

If there is only one incoming arg (the most common case) we don't bother 
to save it in the stack, as the GETARG instruction will save it immediately.
The ABI requires sp to be a multiple of 16 when another routine is called, 
so 8 bytes of blank space is needed unless n is odd and > 1.  The incoming 
args are addressible at sp+locs+blank.  (Note: we only support one arg 
at present.)

Outgoing arguments are passed in rDI, rSI, rDX.  Register arguments are left
where they are until the call, then permuted into place.

On Win64, the frame layout is like this, with 8 byte slots:

old sp: 32-byte shadow area
        return address
        saved rbp
        saved rbx
        saved r15
        saved r14
	saved rsi
	saved rdi
        blank space
sp:	outgoing shadow area

Outgoing arguments are passed in rCX, rDX, r8.
*/

static int inargs;

static int argnum;              /* Last argument pushed */
static int argreg[3];   	/* Whether each arg is a register */
static int argval[3];           /* Value for each arg, or reg  */
static int funreg;              /* Register containing function address */

static void prep_call(int n) {
     nargs = n; argnum = n; funreg = NOREG;
}

static void arg_r(int r) {
     argnum--;
     argreg[argnum] = 1;
     argval[argnum] = r;
}

static void arg_i(int a) {
     argnum--;
     argreg[argnum] = 0;
     argval[argnum] = a;
}

static int use_count(int r) {
     int count = 0;
     for (int i = 0; i < nargs; i++) {
          if (argreg[i] && argval[i] == r)
               count++;
     }
     if (funreg == r) count++;
     return count;
}

/* Replace one register with another in argument list and return count */
static void subst_reg(int r1, int r2) {
     for (int i = 0; i < nargs; i++) {
          if (argreg[i] && argval[i] == r1)
               argval[i] = r2;
     }
     if (funreg == r1) funreg = r2;
}

static int scr_reg[] = { r9, r10, r11 };

static int scratch_reg(void) {
     for (int i = 0; i < 3; i++) {
          if (use_count(scr_reg[i]) == 0)
               return scr_reg[i];
     }

     vm_panic("can't find a scratch register");
     return 0;
}

static int out[] = {
#ifndef WINDOWS
     rDI, rSI, rDX
#else
     rCX, rDX, r8
#endif
};

/* Move args into correct registers */
static void move_args() {
     // Permute the registers
     for (int i = 0; i < nargs; i++) {
          if (argreg[i] && argval[i] != out[i]) {
               if (use_count(out[i]) > 0) {
                    int r = scratch_reg();
                    move(r, out[i]);
                    subst_reg(out[i], r);
               }
               move(out[i], argval[i]);
               argval[i] = out[i];
          }
     }

     // Fill in constant args
     for (int i = 0; i < nargs; i++) {
          if (!argreg[i]) {
               if (funreg == out[i]) {
                    move(rAX, funreg);
                    funreg = rAX;
               }
               move_i(out[i], argval[i]);
          }
     }
}

static void call_r(int ra) {
     funreg = ra;
     move_args();
     instr2_r(opCALL, funreg);
}

static void call_a(void *a) {
     move_args();
     move_i64(rAX, (uint64) a);
     instr2_r(opCALL, rAX);
}     

int vm_prelude(int n, int locs) {
     code_addr entry = pc;
     inargs = n;
     push_r(rBP); push_r(rBX); push_r(r15); push_r(r14);
#ifdef WINDOWS     
     if (n > 1) vm_panic("sorry, only one parameter allowed today");
     if (locs > 0) vm_panic("sorry, no local variables allowed");
     locals = 0;
     push_r(rSI); push_r(rDI);
     sub64_i(rSP, 40);
#else
     int argsp = 0;
     if (n > 1) {
          argsp = 8*n;
          if (n > 2) push_r(rDX);
          push_r(rSI); push_r(rDI);
     }
     int space = locs + argsp + 24;
     space = (space + 15) & ~0xf;
     locals = space - 24;
     if (locals > argsp)
          sub64_i(rSP, locals - argsp);
#endif
     return (int) (address) entry;
}

static void retn(void) {
#ifdef WINDOWS
          add64_i(rSP, 40);
	  pop(rDI); pop(rSI);
#else
	  add64_i(rSP, locals);
#endif
	  pop(r14); pop(r15); pop(rBX); pop(rBP);
          instr(opRET);
}
#endif


/* TRANSLATION ROUTINES */

#define badop() vm_unknown(__FUNCTION__, op)

void vm_gen1r(operation op, vmreg rega) {
     int ra = rega->vr_reg;

     vm_debug1(op, 1, rega->vr_name);     
     vm_space(0);

     switch (op) {
     case JUMP: 
	  instr2_r(opJMP, ra); break;

     case ARG:
          arg_r(ra); break;

     case CALL:
          call_r(ra); break;
          
#ifdef USE_SSE
     case ZEROf:
          instr_rr(opXORPS, ra, ra); break;
     case ZEROd:
          instr_rr(opXORPD, ra, ra); break;
#else
     case ZEROf:
     case ZEROd:
          instr(opFLDZ); fstp_r(ra+1); break;
#endif

     default:
	  badop();
     }
}

void vm_gen1a(operation op, void *a) {
     vm_debug1(op, 1, fmt_val64((uint64) a));
     vm_space(0);

     switch (op) {
     case CALL:
          call_a(a);
          break;

     default:
	  badop();
     }
}

void vm_gen1i(operation op, int a) {
     vm_debug1(op, 1, fmt_val(a));
     vm_space(0);

     switch (op) {
     case PREP:
	  prep_call(a);
	  break;

     case ARG:
          arg_i(a);
          break;

     default:
	  badop();
     }
}

void vm_gen1j(operation op, vmlabel lab) {
     vm_debug1(op, 1, fmt_lab(lab));     
     vm_space(0);

     switch (op) {
     case JUMP: 
	  instr_lab(opJMP_i, lab);
          break;

     default:
	  badop();
     }
}

static void vm_load_store(operation op, int ra, int rb, int c, int rx, int s);

void vm_gen2rr(operation op, vmreg rega, vmreg regb) {
     int ra = rega->vr_reg, rb = regb->vr_reg;

     vm_debug1(op, 2, rega->vr_name, regb->vr_name);
     vm_space(0);

     switch (op) {
     case MOV:
	  if (isfloat(ra) && isfloat(rb))
	       fmove_s(ra, rb);
          else if (isfloat(ra))
	       fmove_to_s(ra, rb);
	  else if (isfloat(rb))
               fmove_from_s(ra, rb);
	  else
	       move(ra, rb); 
	  break;

#ifdef M64X32
     case MOVq:
	  if (isfloat(ra) && isfloat(rb))
	       fmove_d(ra, rb);
	  else if (isfloat(ra))
	       fmove_to_d(ra, rb);
          else if (isfloat(rb))
               fmove_from_d(ra, rb);
          else
	       move64(ra, rb); 
          break;

     case SXTq:
          instr_rr(opMOVSXD_r, ra, rb);
          break;
     case NEGq:
          neg64(ra, rb); break;
#endif

     case NEG:    
	  monop(MONOP(opNEG), ra, rb); break;
     case NOT:    
	  monop(MONOP(opNOT), ra, rb); break;
     case CONVis: 
	  instr_rr(opMOVSWL_r, ra, rb); break;


#ifdef USE_SSE
     case NEGf:
          fneg_s(ra, rb); break;
     case NEGd:
          fneg_d(ra, rb); break;

     case CONVif:
          instr_rr(opPXOR, ra, ra);
          instr_rr(opCVTSI2SS, ra, rb);
          break;
     case CONVfi:
          instr_rr(opCVTTSS2SI, ra, rb);
          break;
     case CONVid:
          instr_rr(opPXOR, ra, ra);
          instr_rr(opCVTSI2SD, ra, rb);
          break;
     case CONVdi:
          instr_rr(opCVTTSD2SI, ra, rb);
          break;
     case CONVdf:
          instr_rr(opCVTSD2SS, ra, rb);
          break;
     case CONVfd:
          instr_rr(opCVTSS2SD, ra, rb);
          break;

#else
     case NEGf:
     case NEGd:
	  fmonop(opFCHS, ra, rb); break;

     case CONVif:
     case CONVid:
	  push_r(rb); loadf(opFILDL_m, ra, rSP, 0, NOREG, 0); pop(rb);
          break;
     case CONVfi:
     case CONVdi:
          push_r(ra); fld_r(rb);
          instr_fpm(opFISTTPS_m, rSP, 0, NOREG, 0); pop(ra);
          break;
     case CONVfd:
     case CONVdf:
          fmove_d(ra, rb); break;
#endif

     default:
          vm_load_store(op, ra, rb, 0, NOREG, 0);
     }
}

void vm_gen2ri(operation op, vmreg rega, int b) {
     int ra = rega->vr_reg;

     vm_debug1(op, 2, rega->vr_name, fmt_val(b));
     vm_space(0);

     switch (op) {
     case MOV: 
          move_i(ra, b); break;

     case GETARG: 
#ifndef M64X32
	  load(ra, rSP, 4*b+locals+20); 
#else
#ifndef WINDOWS
          if (inargs == 1)
               move(ra, rDI);
          else
               load(ra, rSP, locals - 8*(inargs-b));
#else
	  move(ra, rCX);
#endif
#endif
          break;

     default:
          vm_load_store(op, ra, NOREG, b, NOREG, 0);
     }
}

void vm_gen2ra(operation op, vmreg rega, void *b) {
     int ra = rega->vr_reg;

     vm_debug1(op, 2, rega->vr_name, fmt_val64((uint64) b));
     vm_space(0);

     switch (op) {
     case MOV: 
          move_i64(ra, (uint64) b); break;
          break;

     default:
          badop();
     }
}

void vm_gen2rj(operation op, vmreg rega, vmlabel b) {
     int ra = rega->vr_reg;
     
     vm_debug1(op, 2, rega->vr_name, fmt_lab(b));
     vm_space(0);

     switch (op) {
     case MOV:
#ifdef M64X32
          instr_rel(opLEA64, ra, b);
#else
          instr_abs(opMOVL_i, ra, b);
#endif
          break;

     default:
          badop();
     }
}

void vm_gen3rrr(operation op, vmreg rega, vmreg regb, vmreg regc) {
     int ra = rega->vr_reg, rb = regb->vr_reg, rc = regc->vr_reg;

     vm_debug1(op, 3, rega->vr_name, regb->vr_name, regc->vr_name);
     vm_space(0);

     switch (op) {
     case ADD: 
          commute(ALUOP(opADD), ra, rb, rc); break;
     case AND: 
	  commute(ALUOP(opAND), ra, rb, rc); break;
     case XOR: 
	  commute(ALUOP(opXOR), ra, rb, rc); break;
     case OR: 
	  commute(ALUOP(opOR), ra, rb, rc); break;
     case SUB: 
	  subtract(ra, rb, rc); break;
     case MUL: 
	  commute(opIMUL_r, ra, rb, rc); break;

     case LSH: 
	  shift3_r(opSHL, ra, rb, rc); break;
     case RSH: 
	  shift3_r(opSAR, ra, rb, rc); break;
     case RSHu: 
	  shift3_r(opSHR, ra, rb, rc); break;
     case ROR:
          shift3_r(opROR, ra, rb, rc); break;

     case EQ: 
	  compare_r(SETCC(opE), ra, rb, rc); break;
     case GE:
	  compare_r(SETCC(opGE), ra, rb, rc); break;
     case GT: 
	  compare_r(SETCC(opG), ra, rb, rc); break;
     case LE:
	  compare_r(SETCC(opLE), ra, rb, rc); break;
     case LT: 
	  compare_r(SETCC(opL), ra, rb, rc); break;
     case NE:
	  compare_r(SETCC(opNE), ra, rb, rc); break;

#ifdef M64X32
     case EQq: 
	  compare64_r(SETCC(opE), ra, rb, rc); break;
     case GEq:
	  compare64_r(SETCC(opGE), ra, rb, rc); break;
     case GTq: 
	  compare64_r(SETCC(opG), ra, rb, rc); break;
     case LEq:
	  compare64_r(SETCC(opLE), ra, rb, rc); break;
     case LTq: 
	  compare64_r(SETCC(opL), ra, rb, rc); break;
     case NEq:
	  compare64_r(SETCC(opNE), ra, rb, rc); break;
#endif

#ifdef USE_SSE
     case ADDf:
          flop3c_s(opADDSS, ra, rb, rc); break;
     case SUBf:
          flop3_s(opSUBSS, ra, rb, rc); break;
     case MULf:
          flop3c_s(opMULSS, ra, rb, rc); break;
     case DIVf:
          flop3_s(opDIVSS, ra, rb, rc); break;

     case ADDd:
          flop3c_d(opADDSD, ra, rb, rc); break;
     case SUBd:
          flop3_d(opSUBSD, ra, rb, rc); break;
     case MULd:
          flop3c_d(opMULSD, ra, rb, rc); break;
     case DIVd:
          flop3_d(opDIVSD, ra, rb, rc); break;
#else
     case ADDf:
     case ADDd:
	  flop3(opFADD, opFADD, ra, rb, rc); break;
     case SUBf:
     case SUBd:
	  flop3(opFSUB, opFSUBR, ra, rb, rc); break;
     case MULf:
     case MULd:
	  flop3(opFMUL, opFMUL, ra, rb, rc); break;
     case DIVf:
     case DIVd:
	  flop3(opFDIV, opFDIVR, ra, rb, rc); break;
#endif

     case EQd:
     if387(case EQf:)
          fcomp_d(rb, rc); setcc_equal(ra); break;
     case NEd:
     if387(case NEf:)
          fcomp_d(rb, rc); setcc_not_equal(ra); break;
     case GEd:
     if387(case GEf:)
	  fcompare_d(SETCC(opAE), ra, rb, rc); break;
     case GTd:
     if387(case GTf:)
	  fcompare_d(SETCC(opA), ra, rb, rc); break;
     case LEd:
     if387(case LEf:)
	  fcompare_d(SETCC(opAE), ra, rc, rb); break;
     case LTd:
     if387(case LTf:)
	  fcompare_d(SETCC(opA), ra, rc, rb); break;
          
#ifdef USE_SSE
     case EQf:
          fcomp_s(rb, rc); setcc_equal(ra); break;
     case NEf:
          fcomp_s(rb, rc); setcc_not_equal(ra); break;
     case GEf:
	  fcompare_s(SETCC(opAE), ra, rb, rc); break;
     case GTf:
	  fcompare_s(SETCC(opA), ra, rb, rc); break;
     case LEf:
	  fcompare_s(SETCC(opAE), ra, rc, rb); break;
     case LTf:
	  fcompare_s(SETCC(opA), ra, rc, rb); break;
#endif

#ifdef M64X32
     case ADDq:
          commute64(ALUOP64(opADD), ra, rb, rc); break;
     case SUBq:
          subtract64(ra, rb, rc); break;
     case MULq:
          commute64(REXW_(opIMUL_r), ra, rb, rc); break;
#endif
           
     default:
          vm_load_store(op, ra, rb, 0, rc, 0);
     }
}

void vm_gen4rrrs(operation op, vmreg rega, vmreg regb, vmreg regc, int s) {
     int ra = rega->vr_reg, rb = regb->vr_reg, rc = regc->vr_reg;

     vm_debug1(op, 4, rega->vr_name, regb->vr_name, regc->vr_name, fmt_val(s));
     vm_space(0);
     
     switch (op) {
     case ADD:
          instr_rm(opLEA, ra, rb, 0, rc, s);
          break;

     default:
          vm_load_store(op, ra, rb, 0, rc, s);
     }
}

static void vm_load_store(operation op, int ra,
                           int rb, int c, int rx, int s) {
     switch(op) {
     case LDW:
	  if (isfloat(ra)) 
               fload_s(ra, rb, c, rx, s);
          else 
               instr_rm(opMOVL_r, ra, rb, c, rx, s);
	  break;
     case LDSu: 
          instr_rm(opMOVZWL_r, ra, rb, c, rx, s); break;
     case LDBu: 
	  instr_rm(opMOVZBL_r, ra, rb, c, rx, s); break;
     case LDS:
	  instr_rm(opMOVSWL_r, ra, rb, c, rx, s); break;
     case LDB:
          instr_rm(opMOVSBL_r, ra, rb, c, rx, s); break;
     case STW: 
	  if (isfloat(ra)) 
               fstore_s(ra, rb, c, rx, s); 
          else 
               instr_st(opMOVL_m, ra, rb, c, rx, s);
	  break;
     case STS: 
          instr_st(opMOVW_m, ra, rb, c, rx, s); break;
     case STB: 
	  storec(ra, rb, c, rx, s); break;

#ifndef M64X32
     case LDQ: 
          assert(isfloat(ra));
          fload_d(ra, rb, c, rx, s);
          break;
     case STQ:    
          assert(isfloat(ra));
          fstore_d(ra, rb, c, rx, s);
          break;
#else
     case LDQ: 
          if (isfloat(ra))
               fload_d(ra, rb, c, rx, s);
          else
               instr_rm(REXW_(opMOVL_r), ra, rb, c, rx, s);
          break;
     case STQ:    
          if (isfloat(ra))
               fstore_d(ra, rb, c, rx, s);
          else
               instr_st(REXW_(opMOVL_m), ra, rb, c, rx, s);
          break;
#endif

     default:
	  badop();
     }
}

void vm_gen3rri(operation op, vmreg rega, vmreg regb, int c) {
     int ra = rega->vr_reg, rb = regb->vr_reg;

     vm_debug1(op, 3, rega->vr_name, regb->vr_name, fmt_val(c));
     vm_space(0);

     switch (op) {
     case ADD: 
          move(ra, rb);
          if (c == 1)
               inc_r(ra);
          else
               instr2_ri(ALUOP_i(opADD), ra, c);
          break;

     case SUB: 
          move(ra, rb);
          if (c == 1)
               dec_r(ra);
          else
               instr2_ri(ALUOP_i(opSUB), ra, c);
          break;

#ifdef M64X32
     case ADDq:
          move64(ra, rb);
          if (c == 1)
               instr2_r(REXW2_(opINC_m), ra);
          else
               instr2_ri(ALUOP64_i(opADD), ra, c);
          break;

     case SUBq:
          move64(ra, rb);
          if (c == 1)
               instr2_r(REXW2_(opDEC_m), ra);
          else
               instr2_ri(ALUOP64_i(opSUB), ra, c);
          break;
#endif

     case AND: 
	  binop3_i(ALUOP_i(opAND), ra, rb, c); break;
     case OR: 
	  binop3_i(ALUOP_i(opOR), ra, rb, c); break;

     case XOR: 
	  binop3_i(ALUOP_i(opXOR), ra, rb, c); break;
     case MUL:
	  instr_rri(opIMUL_i, ra, rb, c); break;
#ifdef M64X32
     case MULq:
          instr_rri(REXW_(opIMUL_i), ra, rb, c); break;
#endif

     case LSH: 
	  shift3_i(opSHL, ra, rb, c); break;
     case RSH: 
	  shift3_i(opSAR, ra, rb, c); break;
     case RSHu: 
	  shift3_i(opSHR, ra, rb, c); break;
     case ROR:
          shift3_i(opROR, ra, rb, c); break;

     case EQ:
	  compare_i(SETCC(opE), ra, rb, c); break;
     case GE:
	  compare_i(SETCC(opGE), ra, rb, c); break;
     case GT: 
	  compare_i(SETCC(opG), ra, rb, c); break;
     case LE:
	  compare_i(SETCC(opLE), ra, rb, c); break;
     case LT: 
	  compare_i(SETCC(opL), ra, rb, c); break;
     case NE:
	  compare_i(SETCC(opNE), ra, rb, c); break;

#ifdef M64X32
     case EQq:
	  compare64_i(SETCC(opE), ra, rb, c); break;
     case GEq:
	  compare64_i(SETCC(opGE), ra, rb, c); break;
     case GTq:
	  compare64_i(SETCC(opG), ra, rb, c); break;
     case LEq:
	  compare64_i(SETCC(opLE), ra, rb, c); break;
     case LTq:
	  compare64_i(SETCC(opL), ra, rb, c); break;
     case NEq:
	  compare64_i(SETCC(opNE), ra, rb, c); break;
#endif

     default:
          vm_load_store(op, ra, rb, c, NOREG, 0);
     }
}

void vm_gen3rrj(operation op, vmreg rega, vmreg regb, vmlabel lab) {
     int ra = rega->vr_reg, rb = regb->vr_reg;

     vm_debug1(op, 3, rega->vr_name, regb->vr_name, fmt_lab(lab));
     vm_space(0);

     switch (op) {
     case BEQ: 
          branch_r(CONDJ(opE), ra, rb, lab); break;
     case BGE: 
	  branch_r(CONDJ(opGE), ra, rb, lab); break;
     case BGT: 
	  branch_r(CONDJ(opG), ra, rb, lab); break;
     case BLE: 
	  branch_r(CONDJ(opLE), ra, rb, lab); break;
     case BLT: 
	  branch_r(CONDJ(opL), ra, rb, lab); break;
     case BNE: 
	  branch_r(CONDJ(opNE), ra, rb, lab); break;
     case BLTu: 
	  branch_r(CONDJ(opB), ra, rb, lab); break;
     case BGEu:
	  branch_r(CONDJ(opAE), ra, rb, lab); break;
     case BGTu:
	  branch_r(CONDJ(opA), ra, rb, lab); break;
     case BLEu:
	  branch_r(CONDJ(opBE), ra, rb, lab); break;

#ifdef M64X32
     case BEQq: 
	  branch64_r(CONDJ(opE), ra, rb, lab); break;
     case BGEq: 
	  branch64_r(CONDJ(opGE), ra, rb, lab); break;
     case BGTq: 
	  branch64_r(CONDJ(opG), ra, rb, lab); break;
     case BLEq: 
	  branch64_r(CONDJ(opLE), ra, rb, lab); break;
     case BLTq: 
	  branch64_r(CONDJ(opL), ra, rb, lab); break;
     case BNEq: 
	  branch64_r(CONDJ(opNE), ra, rb, lab); break;
#endif

/* Care is needed with floating point comparisons to
   ensure the correct treatment of NaN values.  The P bit
   indicates whether values were unordered in a comparison.

Result of UCOMI
	<	=	>	Unord
ZCP     010     100     000     111     O = S = 0

Thunder					x86
-------					---     
BEQ     F       T       F       F       not JP and JE
BNE     T       F       T       T       JP or JNE
BLT     T       F       F       F       swap JA
BLE     T       T       F       F       swap JAE
BGT     F       F       T       F       JA
BGE     F       T       T       F       JAE
BNLT    F       T       T       T       swap JBE
BNLE    F       F       T       T       swap JB
BNGT    T       T       F       T       JBE
BNGE    T       F       F       T       JB
*/

     case BEQd:
     if387(case BEQf:)
          fcomp_d(ra, rb); jump_equal(lab); break;
          break;
     case BNEd:
     if387(case BNEf:)
          fcomp_d(ra, rb); jump_not_equal(lab); break;
     case BGEd:
     if387(case BGEf:)
	  fbranch_d(CONDJ(opAE), ra, rb, lab); break;
     case BGTd:
     if387(case BGTf:)
	  fbranch_d(CONDJ(opA), ra, rb, lab); break;
     case BLEd:
     if387(case BLEf:)
	  fbranch_d(CONDJ(opAE), rb, ra, lab); break;
     case BLTd:
     if387(case BLTf:)
	  fbranch_d(CONDJ(opA), rb, ra, lab); break;
     case BNGEd:
     if387(case BNGEf:)
          fbranch_d(CONDJ(opB), ra, rb, lab); break;
     case BNGTd:
     if387(case BNGTf:)
          fbranch_d(CONDJ(opBE), ra, rb, lab); break;
     case BNLTd:
     if387(case BNLTf:)
          fbranch_d(CONDJ(opBE), rb, ra, lab); break;
     case BNLEd:
     if387(case BNLEf:)
          fbranch_d(CONDJ(opB), ra, rb, lab); break;

#ifdef USE_SSE
     case BEQf:
          fcomp_s(ra, rb); jump_equal(lab); break;
     case BNEf:
          fcomp_s(ra, rb); jump_not_equal(lab); break;
     case BGEf:
	  fbranch_s(CONDJ(opAE), ra, rb, lab); break;
     case BGTf:
	  fbranch_s(CONDJ(opA), ra, rb, lab); break;
     case BLEf:
	  fbranch_s(CONDJ(opAE), rb, ra, lab); break;
     case BLTf:
	  fbranch_s(CONDJ(opA), rb, ra, lab); break;
     case BNGEf:
          fbranch_s(CONDJ(opB), ra, rb, lab); break;
     case BNGTf:
          fbranch_s(CONDJ(opBE), ra, rb, lab); break;
     case BNLTf:
          fbranch_s(CONDJ(opBE), rb, ra, lab); break;
     case BNLEf:
          fbranch_s(CONDJ(opB), ra, rb, lab); break;
#endif     

     default:
	  badop();
     }
}

void vm_gen3rij(operation op, vmreg rega, int b, vmlabel lab) {
     int ra = rega->vr_reg;

     vm_debug1(op, 3, rega->vr_name, fmt_val(b), fmt_lab(lab));
     vm_space(0);

     switch (op) {
     case BEQ: 
	  branch_i(CONDJ(opE), ra, b, lab); break;
     case BGEu: 
	  branch_i(CONDJ(opAE), ra, b, lab); break;
     case BGE: 
	  branch_i(CONDJ(opGE), ra, b, lab); break;
     case BGT: 
	  branch_i(CONDJ(opG), ra, b, lab); break;
     case BLE: 
	  branch_i(CONDJ(opLE), ra, b, lab); break;
     case BLTu: 
	  branch_i(CONDJ(opB), ra, b, lab); break;
     case BLT: 
	  branch_i(CONDJ(opL), ra, b, lab); break;
     case BNE: 
	  branch_i(CONDJ(opNE), ra, b, lab); break;
     case BGTu:
	  branch_i(CONDJ(opA), ra, b, lab); break;
     case BLEu:
	  branch_i(CONDJ(opBE), ra, b, lab); break;

#ifdef M64X32
     case BEQq:
          branch64_i(CONDJ(opE), ra, b, lab); break;
     case BGEq:
          branch64_i(CONDJ(opGE), ra, b, lab); break;
     case BGTq:
          branch64_i(CONDJ(opG), ra, b, lab); break;
     case BLEq:
          branch64_i(CONDJ(opLE), ra, b, lab); break;
     case BLTq:
          branch64_i(CONDJ(opL), ra, b, lab); break;
     case BNEq:
          branch64_i(CONDJ(opNE), ra, b, lab); break;
#endif

     default:
	  badop();
     }
}

void vm_patch(code_addr loc, code_addr lab) {
     int *p = ((int *) loc);
     *p = lab - loc - 4;
}

#ifdef M64X32
int vm_tramp(funptr f) {
     code_addr p = vm_literal(12);
     code_addr q = p;

     // mov ax, #addr64
     *q++ = 0x48; *q++ = 0xb8;
     * (funptr *) q = f; q += 8;
     // jmp ax
     *q++ = 0xff; *q++ = 0xe0;

     return (int) (ptr) p;
}
#endif

void vm_postlude(void) {
     retn();
}

void vm_chain(code_addr p) {
     instr_tgt(opJMP_i, p);
}

#ifdef DEBUG
int vm_print(code_addr p) {
     printf("%02x", *p);
     return 1;
}
#endif
