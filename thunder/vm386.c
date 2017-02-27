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

#include "config.h"
#include "vm.h"
#include "vminternal.h"

/* Register assignment */

/* These are the i386 registers, numbered according to the binary encoding */
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
#define FPR0		010
#define FPR1		011
#define FPR2		012
#define FPR3		013
#define FPR4		014
#define FPR5		015

#define register(r) ((r)&0x7)

struct _vmreg
     reg_i0 = { "I0", EAX },
     reg_i1 = { "I1", ECX },
     reg_i2 = { "I2", EDX },
     reg_f0 = { "F0", FPR0 },
     reg_f1 = { "F1", FPR1 },
     reg_f2 = { "F2", FPR2 },
     reg_f3 = { "F3", FPR3 },
     reg_f4 = { "F4", FPR4 },
     reg_f5 = { "F5", FPR5 },
     reg_zz = { "ZERO", NOREG };

#ifndef M64X32

struct _vmreg
     reg_v0 = { "V0", EBX },
     reg_v1 = { "V1", ESI },
     reg_v2 = { "V2", EDI },
     reg_v3 = { "V3", EBP };

/* Register layout for use by JIT client */
const int nvreg = 4, nireg = 3;
const vmreg vreg[] = { &reg_v0, &reg_v1, &reg_v2, &reg_v3 }; /* Callee-save */
const vmreg ireg[] = { &reg_i0, &reg_i1, &reg_i2 };      /* Caller-save */

#define IJUMP_SHIFT 2

#else

/* This version includes a crude native amd64 port (enabled with M64X32)
   that still uses 32-bit addresses.  

* We rely on the host software to supply an implementation of vm_alloc
  that allocates storage in the bottom 4GB of the address space.  This
  is easily achieved on Linux with the MAP_32BITS flag to mmap.

* The top 16 integer registers are not made available.  For simple JIT
  applications, they do not give much benefit, but they could be added
  with code to compile the appropriate REX prefixes.  That would, to be
  fair, allow us to reserve the argument registers EDI, ESI and EDX.

* We still use x87 for floating point.

* All addresses that are passed to the code generation interface must
  fit in 32 bits.  That means global data storage needs to be in the bottom
  4GB of memory.  For function addresses, we use wrappers, so that the
  CALL instruction is in effect an indirect call, receiving the 32-bit
  address of cell that contains the 64-bit address of the function.
  It's up to the host software to create and manage these indirection
  cells. */

struct _vmreg
     reg_i3 = { "I3", ESI },
     reg_i4 = { "I4", EDI },
     reg_v0 = { "V0", EBX },
     reg_v1 = { "V1", EBP };

const int nvreg = 2, nireg = 5;
const vmreg vreg[] = { &reg_v0, &reg_v1 }; /* Callee-save */
const vmreg ireg[] = { &reg_i0, &reg_i1, &reg_i2, &reg_i3, &reg_i4 };
                                           /* Caller-save */

#define IJUMP_SHIFT 3

#endif

const int nfreg = 6;
const vmreg freg[] = { &reg_f0, &reg_f1, &reg_f2,        /* Floating point */
                       &reg_f3, &reg_f4, &reg_f5 };
const vmreg ret = &reg_i0, zero = &reg_zz;

/* The first four registers have 8-bit fields like AL */
#define is8bit(r) ((r) <= EBX)

#define isfloat(r) (((r)&010) != 0)

/* DEBUGGING OUTPUT: for consistency with the VM we are implementing,
   instructions are displayed with the destination at the left: thus
   for a := b - c, we would produce "mov EAX, EBX; sub EAX, ECX".  This is
   different from the AT&T syntax normally used in Unix, and more like
   the Intel syntax.  We display loads and stores using the syntax
   "movl EAX, 12(EBP)" for a load and "movl 16(EBP), EAX" for a store,
   unlike the monstrous notation used by Intel. */

#ifdef DEBUG
static char *_regname[] = {
     "none",
     "EAX", "ECX", "EDX", "EBX", "ESP", "EBP", "ESI", "EDI",
     "FPR0", "FPR1", "FPR2", "FPR3", "FPR4", "FPR5"
};

static char **regname = &_regname[1];

static char *fmt_addr_s(int rs, int imm, int shift) {
     static char buf[32];

     if (rs == NOREG)
          sprintf(buf, "%#x", imm);
     else if (shift > 0)
          sprintf(buf, "%s(%s<<%d)", fmt_val(imm), regname[rs], shift);
     else
          sprintf(buf, "%s(%s)", fmt_val(imm), regname[rs]);

     return buf;
}

#define fmt_addr(rs, imm) fmt_addr_s(rs, imm, 0)

/* When Thunder is compiled for debugging, numeric opcodes are accompanied
   by textual mnemonics as they are passed around, and the act of generating
   an instrruction is optionally augmented with printing the instruction in
   assembly-language form.  Depending on whether DEBUG is defined, the macro
   MNEM(mnem, op) expands to either the pair of arguments "mnem, op" or just
   "op" on its own.  Other macros provide declarations for corresponding
   formal parameters of code-generating routines, etc. */

#define OPDECL const char *mnem, int op
#define OPDECL2 const char *mnem, int op, int op2
#define OPDECL_p const char *mnem_p, int op_p
#define OPDECL_r const char *mnem_r, int op_r
#define OP mnem, op
#define OP_p mnem_p, op_p
#define ifdebug(text) text,

#define MNEM(mnem, op) mnem, op
#define MNEM2(mnem, op, op2) mnem, op, op2

#else

#define OPDECL int op
#define OPDECL2 int op, int op2
#define OPDECL_p int op_p
#define OPDECL_r int op_r
#define OP op
#define OP_p op_p
#define ifdebug(text)

#define MNEM(mnem, op) op
#define MNEM2(mnem, op, op2) op, op2

#endif


/* Instructions */

#define REX(op)		((op)|0x4000)
#define REX_W(op)       ((op)|0x4800)

/* ALU operation codes for the 386 */
#define aluADD 		0		/* Binary */
#define aluSUB 		5
#define aluCMP 		7

#define opADD 		MNEM("add", aluADD)
#define opOR 		MNEM("or", 1)
#define opAND		MNEM("and", 4)
#define opSUB 		MNEM("sub", aluSUB)
#define opXOR 		MNEM("xor", 6)
#define opCMP 		MNEM("cmp", aluCMP)

#define opNOT 		MNEM("not", 2)  /* Unary */
#define opNEG 		MNEM("neg", 3)

/* Condition codes for branches */
#define testB 		2               /* unsigned < */
#define testAE 		3		/* unsigned >= */
#define testE 		4               /* = */
#define testNE 		5		/* != */
#define testBE 		6		/* unsigned <= */
#define testA 		7               /* unsigned > */
#define testS 		8               /* negative */
#define testNS 		9		/* non-negative */
#define testL 		12		/* signed < */
#define testGE 		13		/* signed >= */
#define testLE 		14		/* signed <= */
#define testG 		15		/* signed > */
	
/* Operation codes for shifts */
#define opRotateR 	MNEM("ror", 1)
#define opShiftL 	MNEM("shl", 4)
#define opShiftRU 	MNEM("shr", 5)
#define opShiftR 	MNEM("sar", 7)

/* Floating point operation codes */
#define opFadd 		MNEM("fadd", 0)
#define opFmul 		MNEM("fmul", 1)
#define opFsub 		MNEM("fsub", 4)
#define opFsubR 	MNEM("fsubr", 5)
#define opFdiv 		MNEM("fdiv", 6)
#define opFdivR 	MNEM("fdivr", 7)

/* Instruction opcodes */
#define xFLOP_0 	0xd8		// Flop with result in ST(0)
#define xFLOP_r 	0xdc		// Flop with result in ST(r)
#define xFLOPP_r 	0xde		// Flop with result popped to ST(r)

/* Floating point instructions */
#define opFCHS 		MNEM("fchs", 0xd9e0)
#define opFILDL_m 	MNEM("fildl", 0xdb00)
#define opFLD_r 	MNEM("fld", 0xd9c0)
#define opFLDS_m	MNEM("flds", 0xd900)
#define opFLDL_m	MNEM("fldl", 0xdd00)
#define opFST_r 	MNEM("fst", 0xddd0)
#define opFSTP_r 	MNEM("fstp", 0xddd8)
#define opFSTS_m 	MNEM("fsts", 0xd902)
#define opFSTL_m 	MNEM("fstl", 0xdd02)
#define opFSTPS_m	MNEM("fstps", 0xd903)
#define opFSTPL_m	MNEM("fstpl", 0xdd03)
#define opFUCOMI_r 	MNEM("fucomi", 0xdbe8)
#define opFUCOMIP_r 	MNEM("fucomip", 0xdfe8)

/* Integer moves */
#define opMOVL_r 	MNEM("movl", 0x8b) // Move to register
#define opMOVQ_r	MNEM("movq", REX_W(0x8b))
#define opMOVZWL_r 	MNEM("movzwl", 0x0fb7)
#define opMOVSWL_r 	MNEM("movswl", 0x0fbf)
#define opMOVZBL_r 	MNEM("movzbl", 0x0fb6)
#define opMOVSBL_r 	MNEM("movsbl", 0x0fbe)
#define opMOVSXD_r      MNEM("movsxd", REX_W(0x63))
#define opMOVL_m 	MNEM("movl", 0x89) // Move to memory
#define opMOVW_m 	MNEM("movw", 0x6689)
#define opMOVB_m 	MNEM("movb", 0x88)
#define opMOVB2_m 	MNEM("movb***", REX(0x88))
#define opMOVL_i 	MNEM("movl", 0xb8) // Load immediate into register

#define xMONOP 0xf7 		// Unary ALU op
#define x_byte 0x2
#define xALU_r(op) (op<<3)|0x3
#define xALUq_r(op) REX_W(xALU_r(op))
#define xALU_i 0x81
#define xALUq_i REX_W(xALU_i)
#define xSHIFT_r 0xd3
#define xSHIFT_1 0xd1
#define xSHIFT_i 0xc1

#define opIMUL_i MNEM("imul", 0x69) // Integer multiply
#define opIMUL_r MNEM("imul", 0x0faf)
#define opINC_r MNEM("inc", 0x40)
#define opDEC_r MNEM("dec", 0x48)
#define opINC_m MNEM2("inc", 0xff, 0)
#define opDEC_m MNEM2("dec", 0xff, 1)     
#define opPUSH_r MNEM("push", 0x50)
#define opPUSH_i MNEM("push", 0x68)
#define opPOP MNEM("pop", 0x58)
#define opRET MNEM("ret", 0xc3)
#define opJMP MNEM2("jmp", 0xff, 4)
#define opJMP_i MNEM("jmp", 0xe9)
#define opCALL MNEM2("call", 0xff, 2)
#define opCALL_i MNEM("call", 0xe8)
#define opFLDZ MNEM("fldz", 0xd9ee)
#define opTEST MNEM("test", 0x85)

/* Conditional branches */
#define xJMPCC 0x0f80
#define opJE MNEM("je", xJMPCC|testE)
#define opJB MNEM("jb", xJMPCC|testB)
#define opJAE MNEM("jae", xJMPCC|testAE)
#define opJNE MNEM("jne", xJMPCC|testNE)
#define opJBE MNEM("jbe", xJMPCC|testBE)
#define opJA MNEM("ja", xJMPCC|testA)
#define opJL MNEM("jl", xJMPCC|testL)
#define opJGE MNEM("jge", xJMPCC|testGE)
#define opJLE MNEM("jle", xJMPCC|testLE)
#define opJG MNEM("jg", xJMPCC|testG)

/* Comparisons with boolean result */
#define xSETCC 0x0f90
#define opSETE MNEM("sete", xSETCC|testE)
#define opSETB MNEM("setb", xSETCC|testB)
#define opSETAE MNEM("setae", xSETCC|testAE)
#define opSETNE MNEM("setne", xSETCC|testNE)
#define opSETBE MNEM("setbe", xSETCC|testBE)
#define opSETA MNEM("seta", xSETCC|testA)
#define opSETL MNEM("setl", xSETCC|testL)
#define opSETGE MNEM("setge", xSETCC|testGE)
#define opSETLE MNEM("setle", xSETCC|testLE)
#define opSETG MNEM("setg", xSETCC|testG)


/* Instruction formats */

/* opcode -- one or two opcode bytes */
static void opcode(int op) {
     if (op < 256)
	  byte(op);
     else {
	  // The byte order as written is swapped
	  byte(op>>8); byte(op&0xff);
     }
}

/* addr -- a (mode, reg, r/m) triple */
static void addr(int mode, int reg, int rm) {
     byte((mode<<6) | (register(reg)<<3) | register(rm));
}

/* The extra SIB byte used in some modes has the same 2+3+3 layout */
#define sib(s, i, b) addr(s, i, b)

#define signed8(x) ((x) >= -128 && (x) < 128)

/* memory operand */
static void memory(int ra, int rb, int d) {
     /* Encode the register ra and the memory address [rb+d]

        Most of the time (with no indexing), we need a (mode, reg, r/m) 
        triple, where mode determines the size of displacement 
        (0 = none, 1 = byte, 2 = word), and r/m is the base register.
	But there are special cases involving the registers ESP = 4
	and EBP = 5:

	mode = 0:
	    Indirect addressing [rb].  
            For [EBP], use (1, ra, EBP) with d = byte(0).
	    
	mode = 1 or 2:
	    Based addressing [rb+d]. 
            For [ESP+d] use (mode, ra, 4) followed by SIB = (0, 4, ESP).

	mode = 0 and r/m = 5:
	    Absolute addressing [d] (x86 only; on amd64, this gives 
            PC-relative addressing).

	r/m = 4:
	    The triple is followed by another byte containing three
	    values (s, i, b) to encode the address [reg(b)+d+reg(i)*2^s].
	    But i = ESP and b = EBP give further special cases:
	    
	mode = 0, r/m = 4, b = 5:
	    Baseless addressing [d+reg(i)*2^s]

        mode = 0, r/m = 4, i = 4, b = 5
            Absolute addressing [d], useful for amd64

	mode = 0, r/m = 4, i = 4, b != 5:
  	    Based addressing [d+reg(b)].     */

     if (rb == NOREG) {
	  // Absolute [d]
#ifndef M64X32
	  addr(0, ra, 5), word(d);
#else
          addr(0, ra, 4), sib(0, 4, 5), word(d);
#endif
     } else if (rb == ESP) {
	  // Special case to use ESP as base [ESP+d]
	  if (d == 0)
	       addr(0, ra, 4), sib(0, 4, ESP);
	  else if (signed8(d))
	       addr(1, ra, 4), sib(0, 4, ESP), byte(d);
	  else
	       addr(2, ra, 4), sib(0, 4, ESP), word(d);
     }  else {
	  // Base + Displacement [rb+d]
	  if (d == 0 && rb != EBP)
	       addr(0, ra, rb);
	  else if (signed8(d))
	       addr(1, ra, rb), byte(d);
	  else
	       addr(2, ra, rb), word(d);
     }
}

/* memory operand with shift */
static void memory_s(int ra, int rb, int d, int s) {
     // Assuming rb != ESP and d needs 32 bits
     addr(0, ra, 4), sib(s, rb, 5), word(d);
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
}

/* instr_reg -- opcode packed with register */
static void instr_reg(OPDECL, int reg) {
     vm_debug2("%s %s", mnem, regname[reg]);
     opcode(op|register(reg));
}

/* instr_imm -- opcode plus 8/32 bit immediate */
static void instr_imm(OPDECL, int imm) {
     vm_debug2("%s #%s", mnem, fmt_val(imm));
     if (signed8(imm)) {
          opcode(op|x_byte); byte(imm);
     } else {
          opcode(op); word(imm);
     }
}

/* instr_regi32 -- opcode packed with register plus a 32-bit immediate */
static void instr_regi32(OPDECL, int r, int imm) {
     vm_debug2("%s %s, #%s", mnem, regname[r], fmt_val(imm));
     opcode(op|register(r)); word(imm);
}

/* instr_rr -- opcode plus two registers */
static void instr_rr(OPDECL, int r1, int r2) {
     vm_debug2("%s %s, %s", mnem, regname[r1], regname[r2]);
     opcode(op); addr(3, r1, r2);
}

static void aluop_rr(OPDECL, int r1, int r2) {
     instr_rr(MNEM(mnem, xALU_r(op)), r1, r2);
}

/* General form of instr2_r for shifts and floating point ops */
static void instr2_fmt(ifdebug(char *fmt) OPDECL2, int r) {
     vm_debug2(fmt, mnem, regname[r]);
     opcode(op); addr(3, op2, r);
}

/* instr2_r -- two opcodes and a register */
#define instr2_r(ispec, r) instr2_fmt(ifdebug("%s %s") ispec, r)

/* instr2_ri8 -- twin opcode plus register and 8-bit immediate */
static void instr2_ri8(OPDECL2, int rm, int imm) {
     vm_debug2("%s %s, #%s", mnem, regname[rm], fmt_val(imm));
     opcode(op); addr(3, op2, rm); byte(imm);
}

/* instruction with 2 opcodes, memory operand */
static void instr2_m(OPDECL2, int rs, int imm) {
     vm_debug2("%s *%s", mnem, fmt_addr(rs, imm));
     opcode(op); memory(op2, rs, imm);
}

/* instruction with 2 opcodes, memory operand and shift */
static void instr2_ms(OPDECL2, int rs, int imm, int s) {
     vm_debug2("%s *%s", mnem, fmt_addr_s(rs, imm, s));
     opcode(op); memory_s(op2, rs, imm, s);
}

/* instruction with 2 opcodes and 8/32 bit immediate */
static void instr2_ri(OPDECL2, int rm, int imm) {
     vm_debug2("%s %s, #%s", mnem, regname[rm], fmt_val(imm));
     if (signed8(imm)) {
	  opcode(op|x_byte); addr(3, op2, rm); byte(imm);
     } else {
	  opcode(op); addr(3, op2, rm); word(imm);
     }
}

/* instruction with 2 registers and 8/32 bit immediate */
static void instr_rri(OPDECL, int reg, int rm, int imm) {
     vm_debug2("%s %s, %s, #%s", mnem, regname[reg], regname[rm], fmt_val(imm));
     if (signed8(imm)) {
	  opcode(op|x_byte); addr(3, reg, rm); byte(imm);
     } else {
	  opcode(op); addr(3, reg, rm); word(imm);
     }
}

/* instr_rm -- register and memory operand */
static void instr_rm(OPDECL, int rt, int rs, int imm) {
     vm_debug2("%s %s, %s", mnem, regname[rt], fmt_addr(rs, imm));
     opcode(op); memory(rt, rs, imm);
}

/* instr_st -- alternate form of instr_rm for store instruction */
static void instr_st(OPDECL, int rt, int rs, int imm) {
     vm_debug2("%s %s, %s", mnem, fmt_addr(rs, imm), regname[rt]);
     opcode(op); memory(rt, rs, imm);
}

/* instr_lab -- opcode plus branch target */
static code_addr instr_lab(OPDECL, code_addr lab) {
     code_addr r;
     vm_debug2("%s ...", mnem);
     opcode(op); r = pc; word(0);
     if (lab != NULL) vm_patch(r, lab);
     return r;
}

/* instr_m -- floating point load or store */
static void instr_m(OPDECL, int rs, int imm) {
     vm_debug2("%s %s", mnem, fmt_addr(rs, imm));
     opcode(op>>8); memory(op, rs, imm);
}


/* SPECIFIC INSTRUCTIONS */

#define add_i(rd, imm)	instr2_ri(MNEM2("add", xALU_i, aluADD), rd, imm)
#define sub_i(rd, imm)	instr2_ri(MNEM2("sub", xALU_i, aluSUB), rd, imm)

#define add64_i(rd, imm)  instr2_ri(MNEM2("addq", xALUq_i, aluADD), rd, imm)
#define sub64_i(rd, imm)  instr2_ri(MNEM2("subq", xALUq_i, aluSUB), rd, imm)
#define add64_rr(r1, r2)  instr_rr(MNEM("addq", xALUq_r(aluADD)), r1, r2)

#define push_r(r)	instr_reg(opPUSH_r, r)
#define push_i(imm)	instr_imm(opPUSH_i, imm)
#define pop(r)		instr_reg(opPOP, r)
#define fld_r(r) 	instr_reg(opFLD_r, r)
#define fst_r(r) 	instr_reg(opFST_r, r)
#define fstp_r(r) 	instr_reg(opFSTP_r, r)

/* shift instruction with one operand in CL */
#define shift2cl_r(ispec, r) \
     instr2_fmt(ifdebug("%s %s, cl") MNEM2(mnem, xSHIFT_r, op), r)

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

#define move(rd, rs)  move_r(opMOVL_r, rd, rs)
#define move64(rd, rs)  move_r(opMOVQ_r, rd, rs)

/* Load word */
#define load(rd, rs, off)  instr_rm(opMOVL_r, rd, rs, off)

/* shift instructions (3 registers) */
static void shift3_r(OPDECL, int rd, int rs1, int rs2) {
     int rx = rd;

     if (rd == ECX || (rd != rs1 && rd == rs2)) {
	  rx = (rs2 == EAX ? EDX : EAX);
	  push_r(rx);
     }

     move(rx, rs1);
     if (rs2 == ECX)
          shift2cl_r(OP, rx);
     else {
          push_r(ECX); move(ECX, rs2); 
          shift2cl_r(OP, rx); pop(ECX);
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
static void storec(int rt, int rs, int imm) {
     if (is8bit(rt))
          instr_st(opMOVB_m, rt, rs, imm);
     else {
#ifdef M64X32
          instr_st(opMOVB2_m, rt, rs, imm);
#else
	  int rx = (rs == EAX ? EDX : EAX);
	  push_r(rx); move(rx, rt);
	  instr_st(opMOVB_m, rx, rs, imm);
	  pop(rx);
#endif
     }
}

/* unary ALU operation (2 registers) */
static void monop(OPDECL, int rd, int rs) {
     move(rd, rs);
     instr2_r(MNEM2(mnem, xMONOP, op), rd);
}

/* commutative ALU operation (3 register operands) */
static void binop3c_r(OPDECL, int rd, int rs1, int rs2) {
     if (rd == rs2)
	  aluop_rr(OP, rd, rs1);
     else {
	  move(rd, rs1);
	  aluop_rr(OP, rd, rs2);
     }
}

#ifdef M64X32
static void add64_r(int rd, int rs1, int rs2) {
     if (rd == rs2)
          add64_rr(rd, rs1);
     else {
          move64(rd, rs1);
          add64_rr(rd, rs2);
     }
}
#endif

/* subtract (3 registers) */
static void subtract_r(int rd, int rs1, int rs2) {
     if (rd == rs2) {
	  aluop_rr(opSUB, rd, rs1);
	  monop(opNEG, rd, rd);
     } else {
	  move(rd, rs1);
	  aluop_rr(opSUB, rd, rs2);
     }
}

/* multiply (3 registers) */
static void multiply_r(int rd, int rs1, int rs2) {
     if (rd == rs2)
	  instr_rr(opIMUL_r, rd, rs1);
     else {
	  move(rd, rs1);
	  instr_rr(opIMUL_r, rd, rs2);
     }
}

/* binary ALU operation, 2 registers + immediate */
static void binop3_i(OPDECL, int rd, int rs, int imm) {
     move(rd, rs);
     instr2_ri(MNEM2(mnem, xALU_i, op), rd, imm);
}

/* routines for branch instructions return the location for patching */

/* Conditional branch, 2 registers */
static code_addr branch_r(OPDECL, int rs1, int rs2, code_addr lab) {
     aluop_rr(opCMP, rs1, rs2);
     return instr_lab(OP, lab);
}

/* Set flags by comparing register and immediate */
static void comp_i(int rs, int imm) {
     if (imm == 0)
	  instr_rr(opTEST, rs, rs);
     else
	  instr2_ri(MNEM2("cmp", xALU_i, aluCMP), rs, imm);
}

/* Conditional branch, register + immediate */
static code_addr branch_i(OPDECL, int rs, int imm, code_addr lab) {
     comp_i(rs, imm);
     return instr_lab(OP, lab);
}

static void setcc_r(OPDECL, int rd) {
     if (is8bit(rd)) {
	  instr2_r(MNEM2(mnem, op, 0), rd); /* Compute boolean result */
          instr_rr(opMOVZBL_r, rd, rd); /* Zero-extend */
     } else {
	  push_r(EAX);
	  instr2_r(MNEM2(mnem, op, 0), EAX); /* Compute boolean result */
	  instr_rr(opMOVZBL_r, rd, EAX); /* Move and zero-extend */
	  pop(EAX);
     }
}

/* comparison with boolean result (2 registers) */
static void compare_r(OPDECL, int rd, int rs1, int rs2) {
     aluop_rr(opCMP, rs1, rs2);
     setcc_r(OP, rd);
}

/* comparison with boolean result (register + immediate) */
static void compare_i(OPDECL, int rd, int rs, int imm) {
     comp_i(rs, imm);
     setcc_r(OP, rd);
}


/* FLOATING POINT: we treat the 387's stack as a set of ordinary registers,
   using fld and fst instructions to pull things to the top of the stack
   when we need to operate on them.   We do the best we can without trying
   to track register state. */

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

/* Unary floating point, 2 registers */
static void fmonop(OPDECL, int rd, int rs) {
     if (rs == FPR0 && rd == FPR0)
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
     if (rd == FPR0)
	  /* 1: st0 := st0 op stX */
	  instr2_fmt(ifdebug("%s F0, %s") MNEM2(mnem, xFLOP_0, op), rs);
     else if (rs == FPR0)
	  /* 4: stX := stX op st0 */
	  instr2_fmt(ifdebug("%s %s, F0") MNEM2(mnem, xFLOP_r, op_r), rd);
     else {
	  /* stX := stX op stY becomes push stY; 6: stX := stX op st0; pop*/
	  fld_r(rs);
          instr2_fmt(ifdebug("%sp %s, F0") MNEM2(mnem, xFLOPP_r, op_r), rd+1);
     }
}

/* Binary floating point, 3 registers */
static void flop3(OPDECL, OPDECL_r, int rd, int rs1, int rs2) {
     if (rd == rs1)
	  flop2(MNEM2(mnem, op, op_r), rd, rs2);
     else if (rd == rs2)
	  flop2(MNEM2(mnem_r, op_r, op), rd, rs1);
     else {
	  /* stX = stY op stZ */
	  fld_r(rs1);
	  flop2(MNEM2(mnem, op, op_r), FPR0, rs2+1);
	  fstp_r(rd+1);
     }
}

/* Load float or double from memory */
static void loadf(OPDECL, int rt, int rs, int imm) {
     instr_m(OP, rs, imm);
     fstp_r(rt+1);
}

#define floads(rt, rs, imm) 	loadf(opFLDS_m, rt, rs, imm)
#define floadl(rt, rs, imm) 	loadf(opFLDL_m, rt, rs, imm)

/* Store float or double to memory */
static void storef(OPDECL, OPDECL_p, int rt, int rs, int imm) {
     if (rt == FPR0)
	  instr_m(OP, rs, imm);
     else {
	  fld_r(rt); 
	  instr_m(OP_p, rs, imm); 
     }
}

#define fstores(rt, rs, imm) 	\
     storef(opFSTS_m, opFSTPS_m, rt, rs, imm)
#define fstorel(rt, rs, imm) 	\
     storef(opFSTL_m, opFSTPL_m, rt, rs, imm)
#define fzero(rd) 		\
     instr(opFLDZ), fstp_r(rd+1)

/* Floating point comparison, setting integer flags */
static void fcomp(int rs1, int rs2) {
     if (rs1 == FPR0)
	  instr_reg(opFUCOMI_r, rs2);
     else {
	  fld_r(rs1);
	  instr_reg(opFUCOMIP_r, rs2+1);
     }
}

/* Floating point branch */
static code_addr fbranch(OPDECL, int rs1, int rs2, code_addr lab) {
     fcomp(rs1, rs2);
     return instr_lab(OP, lab);   /* Jump on integer flags */
}

/* floating point comparison with boolean result */
static void fcompare(OPDECL, int rd, int rs1, int rs2) {
     fcomp(rs1, rs2);
     setcc_r(OP, rd);
}	  

/* STACK FRAMES */

#ifndef M64X32

/*
We don't use space on the C stack for locals, and don't bother 
to keep the stack pointer always aligned. So just before a call 
instruction, the stack layout is like this:

old sp:	incoming args (at sp0+20)
	return address
	saved bp
	saved bx
	saved si
sp0:	saved di
	blank space
sp:	outgoing args

Here, sp0 denotes the sp position just after entry, used in implementing
the GETARG instruction.

On the Mac, sp must be 16-byte aligned at this point, so 
nargs + blank + 5 must be a multiple of 4 in words.
*/

static int nargs;               /* Effective number of outgoing args */

static void prep_call(int n) {
#ifdef MACOS
     int blank = 3 - n % 4;
     if (blank > 0) sub_i(ESP, blank * sizeof(int));
     nargs = n + blank;
#else
     nargs = n;
#endif
}

static void post_call(void) {
     if (nargs > 0) 
          add_i(ESP, nargs * sizeof(int));
}

void call_r(int ra) {
     instr2_r(opCALL, ra);
     post_call();
}

code_addr vm_prelude(int n) {
     code_addr entry = pc;
     push_r(EBP); push_r(EBX); push_r(ESI); push_r(EDI); 
     return entry;
}

#else

/*
On AMD64, the frame layout is like this:

old sp:
        return address
        saved bp
        saved bx
        saved args if n > 1
sp:     blank space

If there is only one incoming arg (the most common case) we don't bother 
to save it in the stack, as the GETARG instruction will save it immediately.
The ABI requires sp to be a multiple of 16 when another routine is called, 
so 8 bytes of blank space is needed unless n is odd and > 1.  The incoming 
args are addressible at sp+blank+16.  (Note: we only support one arg 
at present.)

Outgoing arguments are passed in EDI, ESI, EDX.  If nargs = 1, then we can 
move the argument into EDI directly; otherwise outgoing arguments are pushed 
on the stack, then popped into the argument registers before the call.
Messy but effective!
*/

static int nargs;               /* Number of outgoing args */
static int argreg;              /* One outgoing argument kept in a register */

static void prep_call(int n) {
     nargs = n; argreg = NOREG;
}

/* Move args from stack into registers */
static void move_args() {
     /* Args were pushed from right to left, to the first arg is on
        top of the stack. */
     if (nargs >= 1) {
          if (argreg != NOREG)
               move(EDI, argreg);
          else
               pop(EDI);
     }
     if (nargs >= 2) pop(ESI);
     if (nargs >= 3) pop(EDX);
}

void call_r(int ra) {
     // An indirect call
     if ((nargs == 1 && ra == EDI) || nargs > 1) {
          // Protect routine address from overwriting
          if (argreg != EAX)
               move(EAX, ra), ra = EAX;
          else
               move(ECX, ra), ra = ECX;
     }
     move_args();
     instr2_m(opCALL, ra, 0);
}

code_addr vm_prelude(int n) {
     code_addr entry = pc;
     push_r(EBP); push_r(EBX); sub64_i(ESP, 8);
     if (n > 1) vm_panic("sorry, only one parameter allowed today");
     return entry;
}

#endif

/* TRANSLATION ROUTINES */

#define badop() vm_unknown(__FUNCTION__, op)

void vm_gen0(operation op) {
     vm_debug1(op, 0);
     vm_space(0);

     switch (op) {
     case RET: 
#ifndef M64X32
	  pop(EDI); pop(ESI); pop(EBX); pop(EBP);
#else
          add64_i(ESP, 8); pop(EBX); pop(EBP);
#endif
          instr(opRET);
	  break;

     default:
	  badop();
     }
}

void vm_gen1r(operation op, vmreg rega) {
     int ra = rega->vr_reg;

     vm_debug1(op, 1, rega->vr_name);     
     vm_space(0);

     switch (op) {
     case JUMP: 
	  instr2_r(opJMP, ra); break;

     case ARG:
          /* Args get pushed from right to left */
#ifndef M64X32
	  push_r(ra);
#else
          if (argreg != NOREG) push_r(argreg);
          argreg = ra;
#endif
	  break;

     case CALL:
          call_r(ra);
          break;
          
     case ZEROF:
     case ZEROD:
	  fzero(ra); break;

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
#ifdef M64X32
          if (argreg != NOREG) {
               push_r(argreg); argreg = NOREG;
          }
#endif
          push_i(a);
          break;

     case CALL:
#ifndef M64X32
          instr_lab(opCALL_i, (code_addr) a);
          post_call();
#else
          /* Indirect call via gate */
          move_args();
          instr_rm(opCALL, NOREG, a);
#endif
          break;

     default:
	  badop();
     }
}

void vm_gen1j(operation op, vmlabel lab) {
     code_addr loc;

     vm_debug1(op, 1, fmt_lab(lab));     
     vm_space(0);

     switch (op) {
     case JUMP: 
	  loc = instr_lab(opJMP_i, NULL);
          vm_branch(BRANCH, loc, lab);
          break;

     default:
	  badop();
     }
}

void vm_gen2rr(operation op, vmreg rega, vmreg regb) {
     int ra = rega->vr_reg, rb = regb->vr_reg;

     vm_debug1(op, 2, rega->vr_name, regb->vr_name);
     vm_space(0);

     switch (op) {
     case MOV:
	  if (isfloat(ra) && isfloat(rb)) {
	       movef(ra, rb);
	  } else if (isfloat(ra)) {
	       push_r(rb); floads(ra, ESP, 0); pop(rb);
	  } else if (isfloat(rb)) {
	       /* Make a stack slot, then overwrite it. */
	       push_r(ra); fstores(rb, ESP, 0); pop(ra);
	  } else {
	       move(ra, rb); 
	  }
	  break;

     case SXTOFF:
#ifndef M64X32
          move(ra, rb);
#else
          instr_rr(opMOVSXD_r, ra, rb);
#endif
          break;

     case NEG:    
	  monop(opNEG, ra, rb); break;
     case NOT:    
	  monop(opNOT, ra, rb); break;
     case NEGF:
     case NEGD:
	  fmonop(opFCHS, ra, rb); break;

     case CONVIF:
     case CONVID:
	  push_r(rb); loadf(opFILDL_m, ra, ESP, 0); pop(rb); break;
     case CONVIC: 
	  binop3_i(opAND, ra, rb, 0xff); break;
     case CONVIS: 
	  instr_rr(opMOVSWL_r, ra, rb); break;
     case CONVFD:
     case CONVDF:
          movef(ra, rb); break;

     default:
	  badop();
     }
}

void vm_gen2ri(operation op, vmreg rega, int b) {
     int ra = rega->vr_reg;

     vm_debug1(op, 2, rega->vr_name, fmt_val(b));
     vm_space(0);

     switch (op) {
     case MOV: 
          if (b == 0)
	       aluop_rr(opXOR, ra, ra);
          else
	       instr_regi32(opMOVL_i, ra, b);
	  break;

     case GETARG: 
#ifndef M64X32
	  load(ra, ESP, 4*b+20); 
#else
          move(ra, EDI);
#endif
          break;

     case LDKW:
          if (isfloat(ra))
               floads(ra, NOREG, b);
          else
               instr_regi32(opMOVL_i, ra, * (int *) (unsigned long) b);
          break;

     case IJUMP:
          instr2_ms(opJMP, ra, b, IJUMP_SHIFT);
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
	  binop3c_r(opADD, ra, rb, rc); break;
     case AND: 
	  binop3c_r(opAND, ra, rb, rc); break;
     case XOR: 
	  binop3c_r(opXOR, ra, rb, rc); break;
     case OR: 
	  binop3c_r(opOR, ra, rb, rc); break;
     case SUB: 
	  subtract_r(ra, rb, rc); break;
     case MUL: 
	  multiply_r(ra, rb, rc); break;

     case LSH: 
	  shift3_r(opShiftL, ra, rb, rc); break;
     case RSH: 
	  shift3_r(opShiftR, ra, rb, rc); break;
     case RSHU: 
	  shift3_r(opShiftRU, ra, rb, rc); break;
     case ROR:
          shift3_r(opRotateR, ra, rb, rc); break;

     case ADDF:
     case ADDD:
	  flop3(opFadd, opFadd, ra, rb, rc); break;
     case SUBF:
     case SUBD:
	  flop3(opFsub, opFsubR, ra, rb, rc); break;
     case MULF:
     case MULD:
	  flop3(opFmul, opFmul, ra, rb, rc); break;
     case DIVF:
     case DIVD:
	  flop3(opFdiv, opFdivR, ra, rb, rc); break;

     case EQ: 
	  compare_r(opSETE, ra, rb, rc); break;
     case GEQ:
	  compare_r(opSETGE, ra, rb, rc); break;
     case GT: 
	  compare_r(opSETG, ra, rb, rc); break;
     case LEQ:
	  compare_r(opSETLE, ra, rb, rc); break;
     case LT: 
	  compare_r(opSETL, ra, rb, rc); break;
     case NEQ:
	  compare_r(opSETNE, ra, rb, rc); break;

     case EQF:
     case EQD:
	  fcompare(opSETE, ra, rb, rc); break;
     case GEQF:
     case GEQD:
	  fcompare(opSETAE, ra, rb, rc); break;
     case GTF:
     case GTD:
	  fcompare(opSETA, ra, rb, rc); break;
     case LEQF:
     case LEQD:
	  fcompare(opSETBE, ra, rb, rc); break;
     case LTF:
     case LTD:
	  fcompare(opSETB, ra, rb, rc); break;
     case NEQF:
     case NEQD:
	  fcompare(opSETNE, ra, rb, rc); break;

     case ADDOFF:
#ifndef M64X32
	  binop3c_r(opADD, ra, rb, rc);
#else
          add64_r(ra, rb, rc);
#endif
          break;
           
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
          if (c == 1) {
               move(ra, rb);
#ifndef M64X32
               instr_reg(opINC_r, ra);
#else
               /* The shorter form has become REX */
               instr2_r(opINC_m, ra);
#endif
          } else {
               binop3_i(opADD, ra, rb, c);
          }
          break;
     case AND: 
	  binop3_i(opAND, ra, rb, c); break;
     case OR: 
	  binop3_i(opOR, ra, rb, c); break;
     case SUB: 
          if (c == 1) {
               move(ra, rb);
#ifndef M64X32
               instr_reg(opDEC_r, ra);
#else
               instr2_r(opDEC_m, ra);
#endif
          } else {
               binop3_i(opSUB, ra, rb, c);
          }
          break;
     case XOR: 
	  binop3_i(opXOR, ra, rb, c); break;
     case MUL:
	  instr_rri(opIMUL_i, ra, rb, c); break;

     case LSH: 
	  shift3_i(opShiftL, ra, rb, c); break;
     case RSH: 
	  shift3_i(opShiftR, ra, rb, c); break;
     case RSHU: 
	  shift3_i(opShiftRU, ra, rb, c); break;
     case ROR:
          shift3_i(opRotateR, ra, rb, c); break;

     case LDW:
	  if (isfloat(ra)) 
               floads(ra, rb, c); 
          else 
               load(ra, rb, c); 
	  break;
     case LDSU: 
          instr_rm(opMOVZWL_r, ra, rb, c); break;
     case LDCU: 
	  instr_rm(opMOVZBL_r, ra, rb, c); break;
     case LDS:
	  instr_rm(opMOVSWL_r, ra, rb, c); break;
     case LDC:
          instr_rm(opMOVSBL_r, ra, rb, c); break;
     case STW: 
	  if (isfloat(ra)) 
               fstores(ra, rb, c); 
          else 
               instr_st(opMOVL_m, ra, rb, c);
	  break;
     case STS: 
          instr_st(opMOVW_m, ra, rb, c); break;
     case STC: 
	  storec(ra, rb, c); break;
     case LDD: 
	  floadl(ra, rb, c); break;
     case STD:    
	  fstorel(ra, rb, c); break;

     case EQ:
	  compare_i(opSETE, ra, rb, c); break;
     case GEQ:
	  compare_i(opSETGE, ra, rb, c); break;
     case GT: 
	  compare_i(opSETG, ra, rb, c); break;
     case LEQ:
	  compare_i(opSETLE, ra, rb, c); break;
     case LT: 
	  compare_i(opSETL, ra, rb, c); break;
     case NEQ:
	  compare_i(opSETNE, ra, rb, c); break;

     default:
	  badop();
     }
}

void vm_gen3rrj(operation op, vmreg rega, vmreg regb, vmlabel lab) {
     int ra = rega->vr_reg, rb = regb->vr_reg;
     code_addr loc;

     vm_debug1(op, 3, rega->vr_name, regb->vr_name, fmt_lab(lab));
     vm_space(0);

     switch (op) {
     case BEQ: 
	  loc = branch_r(opJE, ra, rb, NULL); break;
     case BGEQ: 
	  loc = branch_r(opJGE, ra, rb, NULL); break;
     case BGT: 
	  loc = branch_r(opJG, ra, rb, NULL); break;
     case BLEQ: 
	  loc = branch_r(opJLE, ra, rb, NULL); break;
     case BLT: 
	  loc = branch_r(opJL, ra, rb, NULL); break;
     case BNEQ: 
	  loc = branch_r(opJNE, ra, rb, NULL); break;
     case BLTU: 
	  loc = branch_r(opJB, ra, rb, NULL); break;
     case BGEQU:
	  loc = branch_r(opJAE, ra, rb, NULL); break;
     case BGTU:
	  loc = branch_r(opJA, ra, rb, NULL); break;
     case BLEQU:
	  loc = branch_r(opJBE, ra, rb, NULL); break;

     case BEQF:
     case BEQD:
	  loc = fbranch(opJE, ra, rb, NULL); break;
     case BGEQF:
     case BGEQD:
	  loc = fbranch(opJAE, ra, rb, NULL); break;
     case BGTF:
     case BGTD:
	  loc = fbranch(opJA, ra, rb, NULL); break;
     case BLEQF:
     case BLEQD:
	  loc = fbranch(opJBE, ra, rb, NULL); break;
     case BLTF:
     case BLTD:
	  loc = fbranch(opJB, ra, rb, NULL); break;
     case BNEQF:
     case BNEQD:
	  loc = fbranch(opJNE, ra, rb, NULL); break;

     default:
	  badop();
          return;
     }

     vm_branch(BRANCH, loc, lab);
}

void vm_gen3rij(operation op, vmreg rega, int b, vmlabel lab) {
     int ra = rega->vr_reg;
     code_addr loc;

     vm_debug1(op, 3, rega->vr_name, fmt_val(b), fmt_lab(lab));
     vm_space(0);

     switch (op) {
     case BEQ: 
	  loc = branch_i(opJE, ra, b, NULL); break;
     case BGEQU: 
	  loc = branch_i(opJAE, ra, b, NULL); break;
     case BGEQ: 
	  loc = branch_i(opJGE, ra, b, NULL); break;
     case BGT: 
	  loc = branch_i(opJG, ra, b, NULL); break;
     case BLEQ: 
	  loc = branch_i(opJLE, ra, b, NULL); break;
     case BLTU: 
	  loc = branch_i(opJB, ra, b, NULL); break;
     case BLT: 
	  loc = branch_i(opJL, ra, b, NULL); break;
     case BNEQ: 
	  loc = branch_i(opJNE, ra, b, NULL); break;
     case BGTU:
	  loc = branch_i(opJA, ra, b, NULL); break;
     case BLEQU:
	  loc = branch_i(opJBE, ra, b, NULL); break;

     default:
	  badop();
          return;
     }

     vm_branch(BRANCH, loc, lab);
}

void vm_patch(code_addr loc, code_addr lab) {
     int *p = ((int *) loc);
     *p = lab - loc - 4;
}

void vm_postlude(void) {
}

void vm_chain(code_addr p) {
     instr_lab(opJMP_i, p);
}
