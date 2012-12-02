/*
 * vm386.c
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

#define MNEMONIC
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "config.h"
#include "vm.h"

/* Register assignment */

/* These are the i386 registers */
#define EAX		0
#define ECX		1
#define EDX		2
#define EBX		3
#define ESP		4
#define EBP		5
#define ESI		6
#define EDI		7
#define NOREG		-1

/* And these are the floating point registers */
#define FPR0		0x10
#define FPR1		0x11
#define FPR2		0x12
#define FPR3		0x13
#define FPR4		0x14
#define FPR5		0x15

/* Here's how they correspond to VM registers */
static int equiv[] = {
     EBX, ESI, EDI, EBP,	/* V0 V1 V2 V3 */
     EAX, EDX, ECX,		/* R0 R1 R2 */
     FPR0, FPR1, FPR2, FPR3, FPR4, FPR5,
     EAX, NOREG			/* RET ZERO */
};

/* The first four registers have 8-bit fields like AL */
#define is8bit(r) ((r) <= EBX)

#define isfloat(r) (((r)&0x10) != 0)

/* Instructions */

/* ALU operation codes for the 386 */
#define aluADD 0
#define aluOR 1
#define aluADC 2
#define aluSBB 3
#define aluAND 4
#define aluSUB 5
#define aluXOR 6
#define aluCMP 7

#define aluNOT 2
#define aluNEG 3

/* Condition codes for branches */
#define testB 2
#define testAE 3
#define testE 4
#define testNE 5
#define testBE 6
#define testA 7
#define testS 8
#define testNS 9
#define testL 12
#define testGE 13
#define testLE 14
#define testG 15

/* Operation codes for shifts */
#define shiftL 4
#define shiftR 7
#define shiftRU 5

/* Floating point operation codes */
#define fADD 0
#define fMUL 1
#define fSUB 4
#define fSUBR 5
#define fDIV 6
#define fDIVR 7

/* Instruction opcodes */
#define xFLOP_0 0xd8		// Flop with result in ST(0)
#define xFLOP_r 0xdc		// Flop with result in ST(r)
#define xFLOPP_r 0xde		// Flop with result popped to ST(r)
#define xFCHS 0xd9e0
#define xFILDL_m 0xdb00
#define xFLD_r 0xd9c0
#define xFLDS_m 0xd900
#define xFLDL_m 0xdd00
#define xFST_r 0xddd0
#define xFSTP_r 0xddd8
#define xFSTS_m 0xd902
#define xFSTL_m 0xdd02
#define xFSTPS_m 0xd903
#define xFSTPL_m 0xdd03
#define xFUCOM_r 0xdde0
#define xFUCOMP_r 0xdde8
#define xFNSTSW 0xdfe0

#define xMOVL_r 0x8b		// Move to register
#define xMOVZWL_r 0x0fb7
#define xMOVSWL_r 0x0fbf
#define xMOVZBL_r 0x0fb6
#define xMOVSBL_r 0x0fbe
#define xMOVL_m 0x89		// Move to memory
#define xMOVW_m 0x6689
#define xMOVB_m 0x88
#define xMOVL_i 0xb8		// Load immediate into register

#define xIMUL_i 0x69 		// Integer multiply
#define xMONOP 0xf7 		// Unary ALU op
#define xALU_r(op) (op<<3)|0x3
#define xALU_i 0x81
#define xIMUL_r 0x0faf
#define xPUSH 0x50
#define xPOP 0x58
#define xRET 0xc3
#define xJMPL_i 0xe9
#define xJMPL_r 0xff
#define xCALL_i 0xe8
#define xSHIFT_r 0xd3
#define xSHIFT_1 0xd1
#define xSHIFT_i 0xc1
#define xFLDZ 0xd9ee
#define xSAHF 0x9e
#define xJMPCC 0x0f80
#define xSETCC 0x0f90
#define xTEST 0x85


/* Code generation routines */

#ifndef CODEPAGE
#define CODEPAGE 4096	      /* Size of each code buffer */
#endif
#define MARGIN 16	      /* Safety margin for swiching buffers */

static int frame;
static int nargs;

static code_addr codebuf, limit;

static union {
     code_addr _uc;
     int *_int;
} _pc;

#define pc _pc._uc

static void check_space(int space);

/* byte -- contribute a byte to the object code */
static void byte(int x) {
     *_pc._uc++ = x & 0xff;
}

/* word -- contribute a whole word */
static void word(int x) {
     *_pc._int++ = x;
}


/* Instruction formats */

/* opcode -- one or two opcode bytes */
static void opcode(int op) {
     if (op < 256)
	  byte(op);
     else {
	  // The byte order is swapped so the MSB comes first
	  byte(op>>8); byte(op&0xff);
     }
}

/* addr -- a (mode, reg, r/m) triple */
static void addr(int mode, int reg, int rm) {
     byte((mode<<6) | ((reg&0x7)<<3) | (rm&0x7));
}

/* The extra SIB byte used in some modes has the same 2+3+3 layout */
#define sib(s, i, b) addr(s, i, b)

/* target -- A branch target */
static void target(code_addr d) {
     int *p = _pc._int;
     word(0);
     *p = d - pc;
}

#define signed8(x) ((x) >= -128 && (x) < 128)

/* memory operand */
static void memory(int r, int b, int d) {
     /* Most of the time (with no indexing), we need a (mode, reg,
	r/m) triple, where mode determines the size of displacement (0
	= none, 1 = byte, 2 = word), and r/m is the base register.
	But there are special cases involving the registers ESP = 4
	and EBP = 5:

	mode = 0:
	    Indirect addressing [reg(r/m)].  For [EBP], use (1, r, EBP)
	    with d = byte(0).
	    
	mode = 1 or 2:
	    Based addressing [d+reg(r/m)]. For [d+ESP] use
	    (mode, r, 4) followed by SIB = (0, 4, ESP).

	mode = 0 and r/m = 5:
	    Absolute addressing [d].

	r/m = 4:
	    The triple is followed by another byte containing three
	    values (s, i, b) to encode the address [reg(b)+d+reg(i)*2^s].
	    But we must not have i = ESP, and there are further special 
	    cases:
	    
	mode = 0, r/m = 4, b = 5:
	    Baseless addressing [d+reg(i)*2^s]

	mode = 0, r/m = 4, i = 4, b != 5:
  	    Based addressing [d+reg(b)]. */

     if (b == NOREG)
	  // Absolute [d]
	  addr(0, r, 5), word(d);
     else if (b == ESP) {
	  // Special case to use ESP as base [ESP+d]
	  if (d == 0)
	       addr(0, r, 4), sib(0, 4, ESP);
	  else if (signed8(d))
	       addr(1, r, 4), sib(0, 4, ESP), byte(d);
	  else
	       addr(2, r, 4), sib(0, 4, ESP), word(d);
     } 
     else {
	  // Base + Displacement [b+d]
	  if (d == 0 && b != EBP)
	       addr(0, r, b);
	  else if (signed8(d))
	       addr(1, r, b), byte(d);
	  else
	       addr(2, r, b), word(d);
     }
}

/* op_reg -- opcode packed with register */
#define op_reg(op, reg) \
     opcode(op + ((reg)&0x7))

#define op_rr(op, r1, r2) \
     opcode(op), addr(3, r1, r2)

/* op_imm -- an instruction with 8 or 32-bit immediate */
static void op_imm(int op, int reg, int rm, int imm) {
     if (signed8(imm))
	  opcode(op|0x2), addr(3, reg, rm), byte(imm);
     else 
	  opcode(op), addr(3, reg, rm), word(imm);
}

/* op_mem -- load or store */
static void op_mem(int op, int rt, int rs, int imm) {
     opcode(op), memory(rt, rs, imm);
}

#define op_lab(op, lab) \
     opcode(op), target(lab)

/* flop_mem -- floating point load or store */
static void flop_mem(int op, int rs, int imm) {
     opcode(op>>8); memory(op, rs, imm);
}


/* Specific instructions and families */

/* binary ALU operation (2 registers) */
#define binop_r(op, rd, rs) 	op_rr(xALU_r(op), rd, rs)

/* binary ALU operation with immediate */
#define binop_i(op, rd, imm) 	op_imm(xALU_i, op, rd, imm)

#define add_i(rd, imm) 		binop_i(aluADD, rd, imm)
#define sub_i(rd, imm)		binop_i(aluSUB, rd, imm)
#define mul_i(rd, rs, imm)	op_imm(xIMUL_i, rs, rd, imm)

#define monop_r(op, rd) 	opcode(xMONOP), addr(3, op, rd);

/* Moves: movl_rm means 'move register to memory' and movl_mr means
   'move memory to register', etc. */
#define movl_rr(rd, rs)		op_rr(xMOVL_r, rd, rs);
#define movzbl_rr(rd, rs) 	op_rr(xMOVZBL_r, rd, rs)
#define movswl_rr(rd, rs)       op_rr(xMOVSWL_r, rd, rs)
#define movl_ir(rd, imm) 	op_reg(xMOVL_i, rd), word(imm)

#define movzbl_mr(rt, rs, imm)	op_mem(xMOVZBL_r, rt, rs, imm)
#define movsbl_mr(rt, rs, imm)	op_mem(xMOVSBL_r, rt, rs, imm)
#define movzwl_mr(rt, rs, imm)	op_mem(xMOVZWL_r, rt, rs, imm)
#define movswl_mr(rt, rs, imm)  op_mem(xMOVSWL_r, rt, rs, imm)
#define movl_mr(rt, rs, imm)	op_mem(xMOVL_r, rt, rs, imm)

#define movb_rm(rt, rs, imm) 	op_mem(xMOVB_m, rt, rs, imm)
#define movw_rm(rt, rs, imm) 	op_mem(xMOVW_m, rt, rs, imm)
#define movl_rm(rt, rs, imm) 	op_mem(xMOVL_m, rt, rs, imm)

#define push(r) 		op_reg(xPUSH, r)
#define pop(r) 			op_reg(xPOP, r)

/* shift instruction with one operand in CL */
#define shift2cl_r(op, rd) 	op_rr(xSHIFT_r, op, rd)

/* shift by constant */
static void shift2_i(int op, int rd, int imm) {
     if (imm == 1)
	  opcode(xSHIFT_1), addr(3, op, rd);
     else
	  opcode(xSHIFT_i), addr(3, op, rd), byte(imm);
}

/* Jump on condition codes */
#define jumpcc(op, lab) 	op_lab(xJMPCC+op, lab)

/* Set register accorfing to condition */
#define setcc(op, rd) 		op_rr(xSETCC+op, 0, rd);

/* Set condition codes from register */
#define test_r(rs1, rs2) 	op_rr(xTEST, rs1, rs2)

#define ret() opcode(xRET)
#define jump_r(rd) op_rr(xJMPL_r, 4, rd)
#define call_r(rd) op_rr(xJMPL_r, 2, rd)
#define jump_i(lab) op_lab(xJMPL_i, lab)
#define call_i(lab) op_lab(xCALL_i, lab)

#define fld_r(r) op_reg(xFLD_r, r)
#define fst_r(r) op_reg(xFST_r, r)
#define fstp_r(r) op_reg(xFSTP_r, r)
#define fldz() opcode(xFLDZ)

#define sahf() opcode(xSAHF)
#define fnstsw() opcode(xFNSTSW)


/* Synthetic instructions */

/* Optional move */
static void move(int rd, int rs) {
     if (rd != rs) movl_rr(rd, rs);
}

/* Optional floating-point move */
static void movef(int rd, int rs) {
     if (rd == rs) return;

     if (rs == FPR0)
	  fst_r(rd);
     else {
	  fld_r(rs);
	  fstp_r(rd+1);
     }
}

/* shift instructions (3 registers) */
static void shift3_r(int op, int rd, int rs1, int rs2) {
     if (rd == ECX || (rd != rs1 && rd == rs2)) {
	  int rx = (rs2 == EAX ? EDX : EAX);
	  push(rx); shift3_r(op, rx, rs1, rs2); move(rd, rx); pop(rx);
     } else {
	  move(rd, rs1);
	  if (rs2 == ECX)
	       shift2cl_r(op, rd);
	  else {
	       push(ECX); move(ECX, rs2); shift2cl_r(op, rd); pop(ECX);
	  }
     }
}

/* shift with 2 registers and an immediate */
static void shift3_i(int op, int rd, int rs, int imm) {
     move(rd, rs);
     shift2_i(op, rd, imm);
}

/* store character from any register */
static void storec(int rt, int rs, int imm) {
     if (is8bit(rt))
	  movb_rm(rt, rs, imm);
     else {
	  int rx = (rs == EAX ? EDX : EAX);
	  push(rx); move(rx, rt);
	  movb_rm(rx, rs, imm);
	  pop(rx);
     }
}

/* unary ALU operation (2 registers) */
static void monop(int op, int rd, int rs) {
     move(rd, rs);
     monop_r(op, rd);
}

/* commutative ALU operation (3 register operands) */
static void binop3c_r(int op, int rd, int rs1, int rs2) {
     if (rd == rs2)
	  binop_r(op, rd, rs1);
     else {
	  move(rd, rs1);
	  binop_r(op, rd, rs2);
     }
}

/* non-commutative ALU op (3 registers) */
static void binop3_r(int op, int rd, int rs1, int rs2) {
     if (rd != rs1 && rd == rs2) {
	  int rx = (rs2 == EAX ? EDX : EAX);
	  push(rx); binop3_r(op, rx, rs1, rs2); move(rd, rx); pop(rx);
     } else {
	  move(rd, rs1);
	  binop_r(op, rd, rs2);
     }
}

/* subtract (3 registers) */
static void subtract_r(int rd, int rs1, int rs2) {
     if (rd == rs2) {
	  binop_r(aluSUB, rd, rs1);
	  monop(aluNEG, rd, rd);
     } else {
	  move(rd, rs1);
	  binop_r(aluSUB, rd, rs2);
     }
}

/* multiply (3 registers) */
static void multiply_r(int rd, int rs1, int rs2) {
     if (rd == rs2)
	  op_rr(xIMUL_r, rd, rs1);
     else {
	  move(rd, rs1);
	  op_rr(xIMUL_r, rd, rs2);
     }
}

/* binary ALU operation, 2 registers + immediate */
static void binop3_i(int op, int rd, int rs, int imm) {
     move(rd, rs);
     binop_i(op, rd, imm);
}


/* routines for branch instructions return the location for patching */

/* Conditional branch, 2 registers */
static code_addr branch_r(int op, int rs1, int rs2, code_addr lab) {
     binop_r(aluCMP, rs1, rs2);
     jumpcc(op, lab);
     return pc-4;
}

/* Conditional branch, register + immediate */
static code_addr branch_i(int op, int rs, int imm, code_addr lab) {
     binop_i(aluCMP, rs, imm);
     jumpcc(op, lab);
     return pc-4;
}

/* Ditto with special case for imm=0 */
static code_addr branch_i0(int op, int op0, int rs, int imm, code_addr lab) {
     if (imm != 0)
	  return branch_i(op, rs, imm, lab);
     else {
	  test_r(rs, rs);
	  jumpcc(op0, lab);
	  return pc-4;
     }
}

/* Unary floating point, 2 registers */
static void fmonop(int op, int rd, int rs) {
     if (rs == FPR0 && rd == FPR0)
	  opcode(op);
     else {
	  fld_r(rs); 
	  opcode(op); 
	  fstp_r(rd+1);
     }
}

/* Much confusion surrounds the FSUBR and FDIVR instructions, first
   because the mnemonics in Intel assembly language don't reflect the
   opcodes behind them, and second because of the well-known bug in
   AT&T assemblers.  For FSUB and FSUBR, there are six opcodes for
   register-to-register subtractions:

   opcode	effect			Intel assembler		Our code

   D8 E0+i	r0 := r0 - ri		FSUB ST(0), ST(i)	FLOP_0 SUB
   DC E8+i	ri := ri - r0		FSUB ST(i), ST(0)	FLOP_r SUBR
   DE E8+i	ri := ri - r0; pop	FSUBP ST(i), ST(0)	FLOPP_r SUBR

   D8 E8+i	r0 := ri - r0		FSUBR ST(0), ST(i)	FLOP_0 SUBR
   DC E0+i	ri := r0 - ri		FSUBR ST(i), ST(0)	FLOP_r SUB
   DE E0+i	ri := r0 - ri; pop	FSUPRP ST(i), ST(0)	FLOPP_r SUB

   Here I've given the names FLOP_0, FLOP_r and FLOPP_r to the bytes
   D8, DC and DE respectively, and FSUB and FSUBR to E0 and E8. That
   is to say, I've reproduced the AT&T bug faithfully! */

/* Binary floating point, 2 registers */
static void flop2(int op, int opr, int rd, int rs) {
     if (rd == FPR0)
	  /* st0 = st0 op stX */
	  op_rr(xFLOP_0, op, rs);
     else if (rs == FPR0)
	  /* stX = stX op st0 */
	  op_rr(xFLOP_r, opr, rd);
     else {
	  /* stX = stX op stY */
	  fld_r(rs);
	  op_rr(xFLOPP_r, opr, rd+1);
     }
}

/* Binary floating point, 3 registers */
static void flop3(int op, int opr, int rd, int rs1, int rs2) {
     if (rd == rs1)
	  flop2(op, opr, rd, rs2);
     else if (rd == rs2)
	  flop2(opr, op, rd, rs1);
     else {
	  /* stX = stY op stZ */
	  fld_r(rs1);
	  op_rr(xFLOP_0, op, rs2+1);
	  fstp_r(rd+1);
     }
}

#define flop(op, rd, rs1, rs2)  flop3(op, op##R, rd, rs1, rs2)
#define fADDR fADD
#define fMULR fMUL

/* Load float or double from memory */
static void loadf(int op, int rt, int rs, int imm) {
     flop_mem(op, rs, imm); fstp_r(rt+1);
}

#define floads(rt, rs, imm) 	loadf(xFLDS_m, rt, rs, imm)
#define floadl(rt, rs, imm) 	loadf(xFLDL_m, rt, rs, imm)
#define fiload(rt, rs, imm) 	loadf(xFILDL_m, rt, rs, imm)

/* Store float or double to memory */
static void storef(int op, int rt, int rs, int imm) {
     if (rt == FPR0)
	  flop_mem(op, rs, imm);
     else {
	  fld_r(rt); 
	  flop_mem(op+1, rs, imm); /* +1 for FSTP instead of FST */
     }
}

#define fstores(rt, rs, imm) 	storef(xFSTS_m, rt, rs, imm)
#define fstorel(rt, rs, imm) 	storef(xFSTL_m, rt, rs, imm)
#define fzero(rd) 		fldz(), fstp_r(rd+1)

/* Floating point branch */
static code_addr fbranch(int op, int rs1, int rs2, code_addr lab) {
     if (rs1 == FPR0)
	  op_reg(xFUCOM_r, rs2);
     else {
	  fld_r(rs1); 
	  op_reg(xFUCOMP_r, rs2+1);
     }
     push(EAX);
     fnstsw();
     sahf();
     pop(EAX);
     jumpcc(op, lab);		/* Jump on integer flags */
     return pc-4;
}

/* comparison with boolean result (2 registers) */
static void compare_r(int op, int rd, int rs1, int rs2) {
     if (is8bit(rd)) {
	  binop_r(aluCMP, rs1, rs2);
	  movl_ir(rd, 0);
	  setcc(op, rd);
     } else {
	  push(EAX);
	  binop_r(aluCMP, rs1, rs2);
	  movl_ir(EAX, 0);
	  setcc(op, EAX);
	  move(rd, EAX);
	  pop(EAX);
     }
}

/* comparison with boolean result (register + immediate) */
static void compare_i(int op, int rd, int rs, int imm) {
     if (is8bit(rd)) {
	  binop_i(aluCMP, rs, imm);
	  movl_ir(rd, 0);
	  setcc(op, rd);
     } else {
	  push(EAX);
	  binop_i(aluCMP, rs, imm);
	  movl_ir(EAX, 0);
	  setcc(op, EAX);
	  move(rd, EAX);
	  pop(EAX);
     }
}

/* ditto with special case for imm=0 */
static void compare_i0(int op, int op0, int rd, int rs, int imm) {
     if (imm != 0)
	  compare_i(op, rd, rs, imm);
     else if (is8bit(rd)) {
	  test_r(rs, rs);
	  movl_ir(rd, 0);
	  setcc(op, rd);
     } else {
	  push(EAX);
	  test_r(rs, rs);
	  movl_ir(EAX, 0);
	  setcc(op0, EAX);
	  move(rd, EAX);
	  pop(EAX);
     }
}

/* floating point comparison with boolean result */
static void fcompare(int op, int rd, int rs1, int rs2) {
     /* Set floating condx codes */
     if (rs1 == FPR0)
	  op_reg(xFUCOM_r, rs2);
     else {
	  fld_r(rs1);
	  op_reg(xFUCOMP_r, rs2+1);
     }

     if (rd == EAX) {
	  fnstsw();		/* Move flags to AX */
	  sahf();		/* Set integer flags from AH */
	  setcc(op, EAX);
	  movzbl_rr(EAX, EAX);
     } else {
	  push(EAX); 
	  fnstsw();
	  sahf();
	  setcc(op, EAX);
	  movzbl_rr(rd, EAX);
	  pop(EAX);
     }
}	  


/* Translation routines */

static void unknown(const char *where, operation op) {
     fprintf(stderr, "Unknown op -- %s %s", where, mnemonic[op]);
     exit(2);
}

#define badop() unknown(__FUNCTION__, op)

/*
We don't use space on the C stack for locals, and don't bother 
to keep the stack pointer aligned. So just before a call instruction,
the stack layout is like this:

old sp:	incoming args (at bp+8)
	return address
bp:	saved bp
	saved bx
	saved si
	saved di
	blank space
sp:	outgoing args

On the Mac, sp must be 16-byte aligned at this point, so 
nargs + blank + 5 must be a multiple of 4 in words.
*/

static void prep_call(int n) {
#ifdef MACOS
     int blank = 3 - n % 4;
     if (blank > 0) sub_i(ESP, blank * sizeof(int));
     nargs = n + blank;
#else
     nargs = n;
#endif
}

void vm_gen0(operation op) {
     vm_dbg("%s", mnemonic[op]);

     check_space(0);

     switch (op) {
     case RET: 
	  pop(EDI); pop(ESI); pop(EBX); pop(EBP); 
	  ret();
	  break;

     default:
	  badop();
     }
}

void vm_gen1r(operation op, vmreg a) {
     int ra = equiv[a];

     vm_dbg("%s %s", mnemonic[op], rname[a]);

     check_space(0);

     switch (op) {
     case JUMP: 
	  jump_r(ra); break;

     case ARG:
	  push(ra);
	  break;

     case CALL:
	  call_r(ra);
	  if (nargs > 0) add_i(ESP, nargs * sizeof(int));
	  nargs = 0;
	  break;

     case RETVAL: 
	  move(ra, EAX); break;

     case ZEROF:
	  fzero(ra); break;

     default:
	  badop();
     }
}

void vm_gen1i(operation op, int a) {
     vm_dbg("%s %d", mnemonic[op], a);

     check_space(0);

     switch (op) {
     case PREP:
	  prep_call(a);
	  break;

     case CALL:
	  call_i((code_addr) a);
	  if (nargs > 0) add_i(ESP, nargs * sizeof(int));
	  nargs = 0;
	  break;

     default:
	  badop();
     }
}

code_addr vm_gen1b(operation op, code_addr lab) {
     vm_dbg("%s ---", mnemonic[op]);

     check_space(0);

     switch (op) {
     case JUMP: 
	  jump_i(lab);
	  return pc-4;

     default:
	  badop();
	  return NULL;
     }
}

void vm_gen2rr(operation op, vmreg a, vmreg b) {
     int ra = equiv[a], rb = equiv[b];

     vm_dbg("%s %s, %s", mnemonic[op], rname[a], rname[b]);

     check_space(0);

     switch (op) {
     case MOV:
	  if (isfloat(ra) && isfloat(rb)) {
	       movef(ra, rb);
	  } else if (isfloat(ra)) {
	       push(rb); floads(ra, ESP, 0); pop(rb);
	  } else if (isfloat(rb)) {
	       push(ra); fstores(rb, ESP, 0); pop(ra);
	  } else {
	       move(ra, rb); 
	  }
	  break;

     case NEG:    
	  monop(aluNEG, ra, rb); break;
     case NOT:    
	  monop(aluNOT, ra, rb); break;
     case NEGF:
	  fmonop(xFCHS, ra, rb); break;

     case CONVIF:
	  push(rb); fiload(ra, ESP, 0); pop(rb); break;
     case CONVIC: 
	  binop3_i(aluAND, ra, rb, 0xff); break;
     case CONVIS: 
	  movswl_rr(ra, rb);
	  break;

     default:
	  badop();
     }
}

void vm_gen2ri(operation op, vmreg a, int b) {
     int ra = equiv[a];

     vm_dbg("%s %s, %d", mnemonic[op], rname[a], b);

     check_space(0);

     switch (op) {
     case MOV: 
	  if (b == 0)
	       binop_r(aluXOR, ra, ra); 
	  else 
	       movl_ir(ra, b); 
	  break;
     case GETARG: 
	  movl_mr(ra, EBP, b); break;
     default:
	  badop();
     }
}

void vm_gen3rrr(operation op, vmreg a, vmreg b, vmreg c) {
     int ra = equiv[a], rb = equiv[b], rc = equiv[c];

     vm_dbg("%s %s, %s, %s", mnemonic[op], rname[a], rname[b], rname[c]);

     check_space(0);

     switch (op) {
     case ADD: 
	  binop3c_r(aluADD, ra, rb, rc); break;
     case ADDC:
	  binop3c_r(aluADC, ra, rb, rc); break;
     case AND: 
	  binop3c_r(aluAND, ra, rb, rc); break;
     case XOR: 
	  binop3c_r(aluXOR, ra, rb, rc); break;
     case OR: 
	  binop3c_r(aluOR, ra, rb, rc); break;
     case SUB: 
	  subtract_r(ra, rb, rc); break;
     case SUBC:
	  binop3_r(aluSBB, ra, rb, rc); break;
     case MUL: 
	  multiply_r(ra, rb, rc); break;

     case LSH: 
	  shift3_r(shiftL, ra, rb, rc); break;
     case RSH: 
	  shift3_r(shiftR, ra, rb, rc); break;
     case RSHU: 
	  shift3_r(shiftRU, ra, rb, rc); break;

     case ADDF:
	  flop(fADD, ra, rb, rc); break;
     case SUBF:
	  flop(fSUB, ra, rb, rc); break;
     case MULF:
	  flop(fMUL, ra, rb, rc); break;
     case DIVF:
	  flop(fDIV, ra, rb, rc); break;

     case EQ: 
	  compare_r(testE, ra, rb, rc); break;
     case GEQ:
	  compare_r(testGE, ra, rb, rc); break;
     case GT: 
	  compare_r(testG, ra, rb, rc); break;
     case LEQ:
	  compare_r(testLE, ra, rb, rc); break;
     case LT: 
	  compare_r(testL, ra, rb, rc); break;
     case NEQ:
	  compare_r(testNE, ra, rb, rc); break;

     case EQF:
	  fcompare(testE, ra, rb, rc); break;
     case GEQF:
	  fcompare(testAE, ra, rb, rc); break;
     case GTF:
	  fcompare(testA, ra, rb, rc); break;
     case LEQF:
	  fcompare(testBE, ra, rb, rc); break;
     case LTF:
	  fcompare(testB, ra, rb, rc); break;
     case NEQF:
	  fcompare(testNE, ra, rb, rc); break;

     default:
	  badop();
     }
}

void vm_gen3rri(operation op, vmreg a, vmreg b, int c) {
     int ra = equiv[a], rb = equiv[b];

     vm_dbg("%s %s, %s, %d", mnemonic[op], rname[a], rname[b], c);

     check_space(0);

     switch (op) {
     case ADD: 
	  binop3_i(aluADD, ra, rb, c); break;
     case ADDC:
	  binop3_i(aluADC, ra, rb, c); break;
     case AND: 
	  binop3_i(aluAND, ra, rb, c); break;
     case OR: 
	  binop3_i(aluOR, ra, rb, c); break;
     case SUB: 
	  binop3_i(aluSUB, ra, rb, c); break;
     case SUBC:
	  binop3_i(aluSBB, ra, rb, c); break;
     case XOR: 
	  binop3_i(aluXOR, ra, rb, c); break;
     case MUL:
	  mul_i(ra, rb, c); break;

     case LSH: 
	  shift3_i(shiftL, ra, rb, c); break;
     case RSH: 
	  shift3_i(shiftR, ra, rb, c); break;
     case RSHU: 
	  shift3_i(shiftRU, ra, rb, c); break;

     case LDW:
	  if (isfloat(ra)) floads(ra, rb, c); else movl_mr(ra, rb, c); 
	  break;
     case LDSU: 
	  movzwl_mr(ra, rb, c); break;
     case LDCU: 
	  movzbl_mr(ra, rb, c); break;
     case LDS:
	  movswl_mr(ra, rb, c); break;
     case LDC:
	  movsbl_mr(ra, rb, c); break;
     case STW: 
	  if (isfloat(ra)) fstores(ra, rb, c); else movl_rm(ra, rb, c); 
	  break;
     case STS: 
	  movw_rm(ra, rb, c); break;
     case STC: 
	  storec(ra, rb, c); break;
     case LDD: 
	  floadl(ra, rb, c); break;
     case STD:    
	  fstorel(ra, rb, c); break;

     case EQ:
	  compare_i0(testE, testE, ra, rb, c); break;
     case GEQ:
	  compare_i0(testGE, testNS, ra, rb, c); break;
     case GT: 
	  compare_i(testG, ra, rb, c); break;
     case LEQ:
	  compare_i(testLE, ra, rb, c); break;
     case LT: 
	  compare_i0(testL, testS, ra, rb, c); break;
     case NEQ:
	  compare_i0(testNE, testNE, ra, rb, c); break;

     default:
	  badop();
     }
}

code_addr vm_gen3rrb(operation op, vmreg a, vmreg b, code_addr lab) {
     int ra = equiv[a], rb = equiv[b];

     vm_dbg("%s %s, %s, ---", mnemonic[op], rname[a], rname[b]);

     check_space(0);

     switch (op) {
     case BEQ: 
	  return branch_r(testE, ra, rb, lab);
     case BGEQ: 
	  return branch_r(testGE, ra, rb, lab);
     case BGT: 
	  return branch_r(testG, ra, rb, lab);
     case BLEQ: 
	  return branch_r(testLE, ra, rb, lab);
     case BLT: 
	  return branch_r(testL, ra, rb, lab);
     case BNEQ: 
	  return branch_r(testNE, ra, rb, lab);
     case BLTU: 
	  return branch_r(testB, ra, rb, lab);
     case BGEQU:
	  return branch_r(testAE, ra, rb, lab);
     case BGTU:
	  return branch_r(testA, ra, rb, lab);
     case BLEQU:
	  return branch_r(testBE, ra, rb, lab);

     case BEQF:
	  return fbranch(testE, ra, rb, lab);
     case BGEQF:
	  return fbranch(testAE, ra, rb, lab);
     case BGTF:
	  return fbranch(testA, ra, rb, lab);
     case BLEQF:
	  return fbranch(testBE, ra, rb, lab);
     case BLTF:
	  return fbranch(testB, ra, rb, lab);
     case BNEQF:
	  return fbranch(testNE, ra, rb, lab);

     default:
	  badop();
	  return NULL;
     }
}

code_addr vm_gen3rib(operation op, vmreg a, int b, code_addr lab) {
     int ra = equiv[a];

     vm_dbg("%s %s, %d, ---", mnemonic[op], rname[a], b);

     check_space(0);

     switch (op) {
     case BEQ: 
	  return branch_i0(testE, testE, ra, b, lab);
     case BGEQU: 
	  return branch_i(testAE, ra, b, lab);
     case BGEQ: 
	  return branch_i0(testGE, testNS, ra, b, lab);
     case BGT: 
	  return branch_i(testG, ra, b, lab);
     case BLEQ: 
	  return branch_i(testLE, ra, b, lab);
     case BLTU: 
	  return branch_i(testB, ra, b, lab);
     case BLT: 
	  return branch_i0(testL, testS, ra, b, lab);
     case BNEQ: 
	  return branch_i0(testNE, testNE, ra, b, lab);
     case BGTU:
	  return branch_i(testA, ra, b, lab);
     case BLEQU:
	  return branch_i(testBE, ra, b, lab);
     default:
	  badop();
	  return NULL;
     }
}


/* Miscellaneous interface routines */

void vm_patch(code_addr loc, code_addr lab) {
     int *p = ((int *) loc);
     *p = lab - loc - 4;
}

code_addr vm_label(void) {
     check_space(0);
     return pc;
}

void vm_prolog(int n) {
     frame = 8;
     push(EBP); move(EBP, ESP);
     push(EBX); push(ESI); push(EDI); 
}

int vm_arg(void) {
     int off = frame;
     frame += sizeof(int);
     return off;
}

void vm_flush(code_addr start, code_addr end) { 
     // Do nothing: there is only one cache, and we called mprotect earlier.
}

code_addr *vm_jumptable(int n) {
     check_space(n * sizeof(code_addr));
     limit -= n * sizeof(code_addr);
     return (code_addr *) limit;
}

#ifdef USE_MPROTECT
#include <sys/mman.h>
#endif

static void check_space(int space) {
     if (codebuf == NULL || pc + space > limit - MARGIN) {
	  code_addr p = vm_alloc(CODEPAGE);
#ifdef USE_MPROTECT
	  if (mprotect(p, CODEPAGE, PROT_READ|PROT_WRITE|PROT_EXEC) < 0) {
	       perror("mprotect failed");
	       exit(2);
	  }
#endif
	  if (codebuf != NULL) jump_i(p);

	  codebuf = p; limit = p + CODEPAGE;
	  pc = codebuf;
     }
}
