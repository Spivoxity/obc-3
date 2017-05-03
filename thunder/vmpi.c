/*
 * vmpi.c
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
#include <stdarg.h>
#include <string.h>
#include "config.h"

#include "vm.h"
#include "vminternal.h"

// ---------------- REGISTERS ----------------

/* Register numbers */
#define R0  0
#define R1  1
#define R2  2
#define R3  3
#define R4  4
#define R5  5
#define R6  6
#define R7  7
#define R8  8
#define R9  9
#define R10 10
#define FP  11
#define IP  12
#define SP  13
#define LR  14
#define PC  15
#define NOREG -1
#define R(i) (i)

/* Use only even floating-point registers, numbered to suit the 
   instruction format */
#define F0   0x10
#define F2   0x11
#define F4   0x12
#define F6   0x13
#define F8   0x14
#define F10  0x15
#define F12  0x16
#define F14  0x17

struct _vmreg
     reg_i0 = { "I0", R3 },
     reg_v0 = { "V0", R4 },
     reg_v1 = { "V1", R5 },
     reg_v2 = { "V2", R6 },
     reg_v3 = { "V3", R7 },
     reg_v4 = { "V4", R8 },
     reg_v5 = { "V5", R9 },
     reg_v6 = { "V6", R10 },
     reg_f0 = { "F0", F0 },
     reg_f1 = { "F1", F2 },
     reg_f2 = { "F2", F4 },
     reg_f3 = { "F3", F6 },
     reg_f4 = { "F4", F8 },
     reg_f5 = { "F5", F10 },
     reg_f6 = { "F6", F12 },
     reg_rr = { "RET", R0 },
     reg_zz = { "ZERO", NOREG },
     reg_sp = { "BASE", SP };

const int nvreg = 7, nireg = 8, nfreg = 7;
const vmreg ireg[] = {
     &reg_v0, &reg_v1, &reg_v2, &reg_v3, &reg_v4, &reg_v5, &reg_v6,
     &reg_i0
};
const vmreg freg[] = {
     &reg_f0, &reg_f1, &reg_f2, &reg_f3,
     &reg_f4, &reg_f5, &reg_f6
};
const vmreg ret = &reg_rr, zero = &reg_zz, base = &reg_sp;

#define isfloat(r) (((r)&0x10) != 0)

#define UBIT   (0x08<<20) // Add the offset
#define DBIT   (0x04<<20) // Use odd FP register

#ifdef DEBUG

char *_regname[] = {
     "none", 
     "r0", "r1", "r2", "r3", "r4", "r5", "r6", "r7", "r8", "r9", "r10", 
     "fp", "ip", "sp", "lr", "pc",
     "f0", "f2", "f4", "f6", "f8", "f10", "f12", "f14"
};

char **regname = &_regname[1];

static char *fmt_addr(int rs, int imm, int op) {
     static char buf[32];

     if (imm == 0)
	  sprintf(buf, "[%s]", regname[rs]);
     else
          sprintf(buf, "[%s, #%d]", regname[rs], (op&UBIT ? imm : -imm));

     return buf;
}

// Macros that allow each opcode to be passed around with the 
// corresponding mnemonic attached to it.

#define MNEM(mnem, op) mnem, op

#define OPDECL const char *mnem, int op
#define OPDECL2 const char *mnem2, int op2
#define OP mnem, op
#define OP2 mnem2, op2
#define NULLOP NULL, 0

#define _GETOP(mnem, op) op
#define GETOP(op) _GETOP(op)

/* In a call SETBIT(op, bit), the argument op will have the form 
   'mnem, opcode'.  This trick (exploiting details of the CPP macro
   expansion process) lets us separate the mnemonic and opcode
   as arguments of _SETBIT. */
#define _SETBIT(mnem, op, bit) mnem, (op|bit) 
#define SETBIT(op, bit) _SETBIT(op, bit)

#else

#define MNEM(mnem, op) op

#define OPDECL int op
#define OPDECL2 int op2
#define OP op
#define OP2 op2
#define NULLOP 0
#define GETOP(op) op
#define SETBIT(op, bit) (op|bit)

#endif


// ---------------- OPCODES ----------------

/* ALU operations */
#define aluAND 0
#define aluEOR 2
#define aluSUB 4
#define aluRSB 6
#define aluADD 8
#define aluADC 10
#define aluSBC 12
#define aluRSC 14
#define aluTST 16
#define aluTEQ 18
#define aluCMP 21 // Sets condition codes
#define aluCMN 23 // Ditto
#define aluORR 24
#define aluMOV 26
#define aluBIC 28
#define aluMVN 30

/* Condition codes */
#define condEQ 0
#define condNE 1
#define condHS 2
#define condLO 3
#define condMI 4
#define condPL 5
#define condVS 6
#define condVC 7
#define condHI 8
#define condLS 9
#define condGE 10
#define condLT 11
#define condGT 12
#define condLE 13
#define condAL 14

/* Coprocessor codes */
#define cpSGL 10
#define cpDBL 11


// ---------------- INSTRUCTIONS ----------------

/*
Rough instruction layout:
     [31..28] Condition
     [27..20] Opcode
     [19..16] Rn or third opcode
     [15..12] Rd
     [11..8]  Rs or coprocessor number
     [7..4]   Second opcode
     [3..0]   Rm
*/

#define opcode(cond, op, op2, op3, cp) \
     ((cond)<<28 | (op)<<20 | (op2)<<4 | (op3)<<16 | (cp)<<8)

// Ordinary operations with 1, 2, 3 opcode fields
#define opn(x)             opcode(condAL, x, 0, 0, 0)
#define opn2(x, y)         opcode(condAL, x, y, 0, 0)
#define opn3(x, y, z)      opcode(condAL, x, y, z, 0)

// Conditional version of opn
#define opnc(cond, x)      opcode(cond,   x, 0, 0, 0)

// Floating point operations
#define opf(x, cp)         opcode(condAL, x, 0, 0, cp)
#define opf2(x, y, cp)     opcode(condAL, x, y, 0, cp)
#define opf3(x, y, z, cp)  opcode(condAL, x, y, z, cp)

#define fmt_instr(op, rd, rn, imm) \
     ((op) | (rn)<<16 | (rd)<<12 | (imm))

#define instr(op, rd, rn, imm) \
     word(fmt_instr(op, rd, rn, imm))

#define instr4(op, rd, rn, rm, rs) \
     instr(op, rd, rn, (rm) | (rs)<<8)

#define reg(r) ((r)&0xf)

#define opADD    MNEM("add",    opn(aluADD))
#define opAND    MNEM("and",    opn(aluAND))
#define opBIC    MNEM("bic",    opn(aluBIC))
#define opASR    MNEM("asr",    opn2(aluMOV, 0x4))
#define opB      MNEM("b",      opnc(condAL, 0xa0))
#define opBEQ    MNEM("beq",    opnc(condEQ, 0xa0))
#define opBGE    MNEM("bge",    opnc(condGE, 0xa0))
#define opBGT    MNEM("bgt",    opnc(condGT, 0xa0))
#define opBHI    MNEM("bhi",    opnc(condHI, 0xa0))
#define opBHS    MNEM("bhs",    opnc(condHS, 0xa0))
#define opBLE    MNEM("ble",    opnc(condLE, 0xa0))
#define opBLO    MNEM("blo",    opnc(condLO, 0xa0))
#define opBLS    MNEM("bls",    opnc(condLS, 0xa0))
#define opBLT    MNEM("blt",    opnc(condLT, 0xa0))
#define opBNE    MNEM("bne",    opnc(condNE, 0xa0))
#define opBLX    MNEM("blx",    opn2(0x12, 0x3))
#define opBX     MNEM("bx",     opn2(0x12, 0x1))
#define opCMN    MNEM("cmn",    opn(aluCMN))
#define opCMP    MNEM("cmp",    opn(aluCMP))
#define opEOR    MNEM("eor",    opn(aluEOR))
#define opFADDD  MNEM("faddd",  opf2(0xe3, 0x0, cpDBL))
#define opFADDS  MNEM("fadds",  opf2(0xe3, 0x0, cpSGL))
#define opFCMPD  MNEM("fcmpd",  opf3(0xeb, 0x4, 0x4, cpDBL))
#define opFCMPS  MNEM("fcmps",  opf3(0xeb, 0x4, 0x4, cpSGL))
#define opFCVTDS MNEM("fcvtds", opf3(0xeb, 0xc, 0x7, cpSGL))
#define opFCVTSD MNEM("fcvtsd", opf3(0xeb, 0xc, 0x7, cpDBL))
#define opFDIVD  MNEM("fdivd",  opf(0xe8, cpDBL))
#define opFDIVS  MNEM("fdivs",  opf(0xe8, cpSGL))
#define opFLDD   MNEM("fldd",   opf(0xd1, cpDBL))
#define opFLDS   MNEM("flds",   opf(0xd1, cpSGL))
#define opFMOVD  MNEM("fmovd",  opf3(0xeb, 0x4, 0, cpDBL))
#define opFMRS   MNEM("fmrs",   opf2(0xe1, 0x1, cpSGL))
#define opFMSR   MNEM("fmsr",   opf2(0xe0, 0x1, cpSGL))
#define opFMSTAT MNEM("fmstat", opf3(0xef, 0x1, 0x1, cpSGL))
#define opFMULD  MNEM("fmuld",  opf(0xe2, cpDBL))
#define opFMULS  MNEM("fmuls",  opf(0xe2, cpSGL))
#define opFNEGD  MNEM("fnegd",  opf3(0xeb, 0x4, 0x1, cpDBL))
#define opFNEGS  MNEM("fnegs",  opf3(0xeb, 0x4, 0x1, cpSGL))
#define opFSITOD MNEM("fsitod", opf3(0xeb, 0xc, 0x8, cpDBL))
#define opFSITOS MNEM("fsitos", opf3(0xeb, 0xc, 0x8, cpSGL))
#define opFTOSIZD MNEM("ftosizd", opf3(0xeb, 0xc, 0xd, cpDBL))
#define opFTOSIZS MNEM("ftosizd", opf3(0xeb, 0xc, 0xd, cpSGL))
#define opFSTD   MNEM("fstd",   opf(0xd0, cpDBL))
#define opFSTS   MNEM("fsts",   opf(0xd0, cpSGL))
#define opFSUBD  MNEM("fsubd",  opf2(0xe3, 0x4, cpDBL))
#define opFSUBS  MNEM("fsubs",  opf2(0xe3, 0x4, cpSGL))
#define opLDMFD  MNEM("ldmfd",  opn(0x89))
#define opLDR    MNEM("ldr",    opn(0x51))
#define opLDRB   MNEM("ldrb",   opn(0x55))
#define opLDRH   MNEM("ldrh",   opn2(0x11, 0xb))
#define opLDSB   MNEM("ldsb",   opn2(0x11, 0xd))
#define opLDSH   MNEM("ldsh",   opn2(0x11, 0xf))
#define opLSL    MNEM("lsl",    opn2(aluMOV, 0x0))
#define opLSR    MNEM("lsr",    opn2(aluMOV, 0x2))
#define opMOV    MNEM("mov",    opn(aluMOV))
#define opMOVEQ  MNEM("moveq",  opnc(condEQ, aluMOV))
#define opMOVGE  MNEM("movge",  opnc(condGE, aluMOV))
#define opMOVGT  MNEM("movgt",  opnc(condGT, aluMOV))
#define opMOVHI  MNEM("movhi",  opnc(condHI, aluMOV))
#define opMOVHS  MNEM("movhs",  opnc(condHS, aluMOV))
#define opMOVLE  MNEM("movle",  opnc(condLE, aluMOV))
#define opMOVLO  MNEM("movlo",  opnc(condLO, aluMOV))
#define opMOVLS  MNEM("movls",  opnc(condLS, aluMOV))
#define opMOVLT  MNEM("movlt",  opnc(condLT, aluMOV))
#define opMOVNE  MNEM("movne",  opnc(condNE, aluMOV))
#define opMOVT   MNEM("movt",   opn(0x34))
#define opMOVW   MNEM("movw",   opn(0x30))
#define opMUL    MNEM("mul",    opn2(0x00, 0x9))
#define opMVN    MNEM("mvn",    opn(aluMVN))
#define opORR    MNEM("orr",    opn(aluORR))
#define opROR    MNEM("ror",    opn2(aluMOV, 0x6))
#define opRSB    MNEM("rsb",    opn(aluRSB))
#define opSTMFDw MNEM("stmfd!", opn(0x92))
#define opSTRB   MNEM("strb",   opn(0x54))
#define opSTRH   MNEM("strh",   opn2(0x10, 0xb))
#define opSTR    MNEM("str",    opn(0x50))
#define opSUB    MNEM("sub",    opn(aluSUB))
#define opSXTH   MNEM("sxth",   opn3(0x6b, 0x7, 0xf))

#ifndef CODEPAGE
#define CODEPAGE 4096	      /* Size of each code buffer */
#endif
#define MARGIN 32	      /* Safety margin for switching buffers */

// ---------------- INSTRUCTION FORMATTING ----------------

#define IMMED (0x20<<20)
#define RSHIFT (1<<4) // Shift amount in Rs

#define immed(imm) ((imm)&0xff)
#define shift_imm(c) (((c)&0x1f)<<7)
#define imm12(imm) ((imm)&0xfff)

#ifdef DEBUG
static unsigned decode(unsigned imm) {
     int shift = 2 * (imm >> 8);
     imm &= 0xff;
     return (shift == 0 ? imm : (imm >> shift) | (imm << (32-shift)));
}
#endif

// rd := rn op rm
static void op_rrr(OPDECL, int rd, int rn, int rm) {
     vm_debug2("%s %s, %s, %s", mnem, regname[rd], regname[rn], regname[rm]);
     instr(op, reg(rd), reg(rn), reg(rm));
     vm_done();
}

// Multiply rn := rm * rs
static void op_mul(OPDECL, int rn, int rm, int rs) {
     vm_debug2("%s %s, %s, %s", mnem, regname[rn], regname[rm], regname[rs]);
     instr4(op, 0, reg(rn), reg(rm), reg(rs));
     vm_done();
}

// rd := rn op imm with shifted immediate
static void op_rri(OPDECL, int rd, int rn, int imm) {
     vm_debug2("%s %s, %s, #%s", mnem,
               regname[rd], regname[rn], fmt_val(decode(imm)));
     instr(op|IMMED, reg(rd), reg(rn), imm12(imm));
     vm_done();
}

// rd := rm shift rs
static void shift_r(OPDECL, int rd, int rm, int rs) {
     vm_debug2("mov %s, %s, %s %s", 
               regname[rd], regname[rm], mnem, regname[rs]);
     instr4(op|RSHIFT, reg(rd), 0, reg(rm), reg(rs));
     vm_done();
}

// rd := rm shift c
static void shift_i(OPDECL, int rd, int rm, int c) {
     vm_debug2("mov %s, %s, %s #%d", regname[rd], regname[rm], mnem, c);
     instr(op, reg(rd), 0, reg(rm)|shift_imm(c));
     vm_done();
}

// rd := op rm
static void op_rr(OPDECL, int rd, int rm) {
     vm_debug2("%s %s, %s", mnem, regname[rd], regname[rm]);
     instr(op, reg(rd), 0, reg(rm));
     vm_done();
}

static void cmp_r(OPDECL, int rn, int rm) {
     vm_debug2("%s %s, %s", mnem, regname[rn], regname[rm]);
     instr(op, 0, reg(rn), reg(rm));
     vm_done();
}

static void cmp_i(OPDECL, int rn, int imm) {
     vm_debug2("%s %s, #%s", mnem, regname[rn], fmt_val(decode(imm)));
     instr(op|IMMED, 0, reg(rn), imm12(imm));
     vm_done();
}

static void op_ri(OPDECL, int rd, int imm) {
     vm_debug2("%s %s, #%s", mnem, regname[rd], fmt_val(decode(imm)));
     instr(op|IMMED, reg(rd), 0, imm12(imm));
     vm_done();
}

// rd :=: mem[rn +/- off] -- must specify UBIT for addition
static void ldst_ri(OPDECL, int rd, int rn, int off) {
     vm_debug2("%s %s, %s", mnem, regname[rd], fmt_addr(rn, off, op));
     instr(op, reg(rd), reg(rn), imm12(off));
     vm_done();
}

#define RRBIT  (0x20<<20) // Double-reg indirect

// rd :=: mem[rn + rm]
static void ldst_rr(OPDECL, int rd, int rn, int rm) {
     vm_debug2("%s, %s, [%s, %s]", mnem, regname[rd], regname[rn], regname[rm]);
     instr(op|RRBIT|UBIT, reg(rd), reg(rn), reg(rm));
     vm_done();
}


// Fancy indexed loads and stores

#define IBIT  (0x04<<20)

#define offx(n) ((((n)&0xf0)<<4)|((n)&0xf))

// must specify UBIT for addition
static void ldstx_ri(OPDECL, int rd, int rn, int off) {
     vm_debug2("%s %s, %s", mnem, regname[rd], fmt_addr(rn, off, op));
     instr(op|IBIT, reg(rd), reg(rn), offx(off));
     vm_done();
}

static void ldstx_rr(OPDECL, int rd, int rn, int rm) {
     vm_debug2("%s %s, [%s, %s]", mnem, regname[rd], regname[rn], regname[rm]);
     instr(op|UBIT, reg(rd), reg(rn), reg(rm));
     vm_done();
}

#define bit(r) (1<<(r))
#define range(a, b) (((-1)<<a)&~(-1<<(b+1)))

// Branches

static void branch_i(OPDECL, int dest) {
     vm_debug2("%s %d", mnem, dest);
     instr(op, 0, 0, ((int) dest)&0xffffff);
     vm_done();
}

static void jump_r(OPDECL, int rm) {
     vm_debug2("%s %s", mnem, regname[rm]);
     instr4(op, 0xf, 0xf, reg(rm), 0xf);
     vm_done();
}

// Copy FP to int status
static void _fmstat(OPDECL) {
     vm_debug2("%s", mnem);
     instr(op, 0xf, 0, 0);
     vm_done();
}
#define fmstat() _fmstat(opFMSTAT)

// move from int reg to single-prec FP register rn := rd
static void _fmsr(OPDECL, int rn, int rd) {
     vm_debug2("%s %s, %s", mnem, regname[rn], regname[rd]);
     instr(op, reg(rd), reg(rn), 0);
     vm_done();
}
#define fmsr(rn, rd) _fmsr(opFMSR, rn, rd)

// move from single-prec FP register to int reg rd := rn
static void _fmrs(OPDECL, int rd, int rn) {
     vm_debug2("%s %s, %s", mnem, regname[rd], regname[rn]);
     instr(op, reg(rd), reg(rn), 0);
     vm_done();
}
#define fmrs(rd, rn) _fmrs(opFMRS, rd, rn)

// load/store single/double plus/minus -- must use UBIT for plus
static void ldst_f(OPDECL, int rd, int rn, int off) {
     vm_debug2("%s %s%s, %s", mnem, regname[rd], (op&DBIT ? "+1" : ""),
         fmt_addr(rn, off*4, op));
     instr(op, reg(rd), reg(rn), immed(off));
     vm_done();
}


// ---------------- LITERAL TABLE ----------------

#define MAXLITS 256

static int literals[MAXLITS];
static code_addr litloc[MAXLITS];
static int nlits;

code_addr make_literal(int val) {
     for (int i = 0; i < nlits; i++) {
          if (literals[i] == val)
               return litloc[i];
     }

     if (nlits >= MAXLITS)
          vm_panic("too many literals");
     
     code_addr loc = vm_literal(4);
     * (int *) loc = val;

     literals[nlits] = val;
     litloc[nlits] = loc;
     return loc;
}

/* load_literal -- put literal into a specified register */
static void load_literal(int reg, int val) {
     code_addr loc = make_literal(val);
     if (isfloat(reg)) vm_panic("load_literal");
     ldst_ri(SETBIT(opLDR, UBIT), reg, PC, loc - (pc+8));
}


// ---------------- VIRTUAL INSTRUCTIONS ----------------

static unsigned imm_field;      /* Formatted immediate field */

/* immediate -- try to format shifter operand */
static int immediate(int imm) {
     unsigned val = imm;
     int shift = 0;

     /* Try to find a shift that works, using ROL by minimum distance */
     while (val >= 256 && shift < 15) {
          val = (val >> 2) | (val << 30); shift++;
     }

     /* Compute immediate field; nonsense if val >= 256. Encoded using ROR. */
     imm_field = (((16-shift)&0xf) << 8) | val;
     // if (val < 256 && shift > 0) printf("Bingo! %u %d\n", val, shift);
     return (val < 256);
}
 
static void move_immed(int r, int imm) {
     if (immediate(imm))
          op_ri(opMOV, r, imm_field);
     else if (immediate(~imm))
          op_ri(opMVN, r, imm_field);
     else
          load_literal(r, imm);
}

static int const_reg(int imm) {
     move_immed(IP, imm);
     return IP;
}

void compare_immed(int rn, int imm) {
     if (immediate(imm))
	  cmp_i(opCMP, rn, imm_field);
     else if (immediate(-imm))
          cmp_i(opCMN, rn, imm_field);
     else {
	  int rm = const_reg(imm); // MVN may succeed where CMN fails
          cmp_r(opCMP, rn, rm);
     }
}

static void arith_signed(OPDECL, OPDECL2, int rd, int rn, int imm) {
     if (immediate(imm))
          op_rri(OP, rd, rn, imm_field);
     else if (op2 != 0 && immediate(-imm))
          op_rri(OP2, rd, rn, imm_field);
     else {
	  int rm = const_reg(imm); // Also might use MVN
          op_rrr(OP, rd, rn, rm);
     }
}

#define arith_immed(ispec, rd, rn, imm)	\
     arith_signed(ispec, NULLOP, rd, rn, imm)

#define add_immed(ra, rb, c) \
     arith_signed(opADD, opSUB, ra, rb, c)

static void arith_compl(OPDECL, OPDECL2, int rd, int rn, int imm) {
     if (immediate(imm))
          op_rri(OP, rd, rn, imm_field);
     else if (op2 != 0 && immediate(~imm))
          op_rri(OP2, rd, rn, imm_field);
     else {
	  int rm = const_reg(imm);
          op_rrr(OP, rd, rn, rm);
     }
}

static void boolcond(OPDECL, int r) {
     move_immed(r, 0);
     op_ri(OP, r, 1);
}

/* vm_patch -- patch offset into a branch */
void vm_patch(code_addr loc, code_addr lab) {
     /* I hope that if a branch crosses between code segments, the segments 
	have been allocated close enough to each other. */

     int off = lab - loc - 8;; // in bytes
     assert((off & 0x3) == 0);
     off >>= 2;
     if (off < -0x800000 || off >= 0x800000)
          vm_panic("branch offset out of range");
     int *p = ((int *) loc);
     *p = (*p & ~0xffffff) | (off & 0xffffff);
}

static void branch(OPDECL, vmlabel lab) {
     code_addr loc = pc;
     branch_i(OP, 0);
     vm_branch(BRANCH, loc, lab);
}

#define bool_reg(ispec, ra, rb, rc) \
     cmp_r(opCMP, rb, rc), boolcond(ispec, ra)

#define bool_immed(ispec, ra, rb, c) \
     compare_immed(rb, c), boolcond(ispec, ra)

#define bool_reg_f(ispec, ra, rb, rc) \
     op_rr(opFCMPS, rb, rc), fmstat(), boolcond(ispec, ra)

#define bool_reg_d(ispec, ra, rb, rc) \
     op_rr(opFCMPD, rb, rc), fmstat(), boolcond(ispec, ra)

#define br_reg(ispec, ra, rb, lab) \
     cmp_r(opCMP, ra, rb), branch(ispec, lab)

#define br_reg_f(ispec, ra, rb, lab) \
     op_rr(opFCMPS, ra, rb), fmstat(), branch(ispec, lab)
 
#define br_reg_d(ispec, ra, rb, lab) \
     op_rr(opFCMPD, ra, rb), fmstat(), branch(ispec, lab)
 
#define br_immed(ispec, ra, b, lab) \
     compare_immed(ra, b), branch(ispec, lab)

/* Loads and stores for word and unsigned byte */
static void load_store(OPDECL, int ra, int rb, int c) {
     if (rb == NOREG) {
	  int rc = const_reg(c);
          ldst_ri(SETBIT(OP, UBIT), ra, rc, 0);
          return;
     }

     if (c >= 0 && c < 4096)
          ldst_ri(SETBIT(OP, UBIT), ra, rb, c);
     else if (c < 0 && c > -4096)
          ldst_ri(OP, ra, rb, -c);
     else {
	  int rc = const_reg(c);
          ldst_rr(OP, ra, rb, rc);
     }
}

#define load_word(ra, rb, off) \
     load_store(opLDR, ra, rb, off)

/* Other integer loads and stores */
static void load_store_x(OPDECL, int ra, int rb, int c) {
     if (rb == NOREG) {
	  int rc = const_reg(c);
          ldstx_ri(SETBIT(OP, UBIT), ra, rc, 0);
          return;
     }

     if (c >= 0 && c < 256)
          ldstx_ri(SETBIT(OP, UBIT), ra, rb, c);
     else if (c < 0 && c > -256)
          ldstx_ri(OP, ra, rb, -c);
     else {
	  int rc = const_reg(c);
          ldstx_rr(OP, ra, rb, rc);
     }
}

/* Floating point loads and stores */
static void load_store_f(OPDECL, int ra, int rb, int c) {
     assert((c&3) == 0);

     if (rb == NOREG) {
	  int rc = const_reg(c);
          ldst_f(SETBIT(OP, UBIT), ra, rc, 0);
          return;
     }

     if (c >= 0 && c < 1024)
          ldst_f(SETBIT(OP, UBIT), ra, rb, c/4);
     else if (c < 0 && c > -1024)
	  ldst_f(OP, ra, rb, (-c)/4);
     else {
          add_immed(IP, rb, c);
          ldst_f(SETBIT(OP, UBIT), ra, IP, 0);
     }
}     

/* Load or store double, perhaps unaligned */
static void load_store_d(OPDECL, int ra, int rb, int c) {
     // Must avoid unaligned addressing.  (It's emulated by a trap
     // handler on the Pi, but that's very slow.)

     if (rb == NOREG) {
          int rc = const_reg(c);
          ldst_f(SETBIT(OP, UBIT), ra, rc, 0);
          ldst_f(SETBIT(OP, UBIT|DBIT), ra, rc, 1);
          return;
     }

     load_store_f(OP, ra, rb, c);
     load_store_f(SETBIT(OP, DBIT), ra, rb, c+4);
}

static void move_reg(int ra, int rb) {
     if (ra != rb) op_rr(opMOV, ra, rb);
}

static int argp;

static void proc_call(int ra) {
     assert(argp == 0);
     jump_r(opBLX, ra);
}

// Register map and patch chain for RET locations

static unsigned regmap = 0;

static void write_reg(int r) {
     if (! isfloat(r)) regmap |= bit(r);
}

static code_addr retchain = NULL;

static void retlink() {
     code_addr p = pc;
     word((int) retchain);
     retchain = p;
}


// ---------------- CODE GENERATION INTERFACE ----------------

#define badop() vm_unknown(__FUNCTION__, op)

void vm_gen0(operation op) {
     vm_debug1(op, 0);
     vm_space(0);

     switch (op) {
     case RET: 
          vm_debug2("ldmfd fp, ...\n");
          retlink();
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
	  jump_r(opBX, ra); break;

     case CALL:
          proc_call(ra); break;
          
     case ARG:
	  argp--;
	  move_reg(R(argp), ra);
	  break;

     case ZEROF:
          fmsr(ra, const_reg(0));
          break;

     case ZEROD:
	  fmsr(ra, const_reg(0));
          op_rr(opFCVTSD, ra, ra);
          break;

     default:
	  badop();
     }
}

void vm_gen1i(operation op, int a) {
     vm_debug1(op, 1, fmt_val(a));
     vm_space(0);

     switch (op) {
     case CALL:
	  proc_call(const_reg(a)); break;

     case PREP:
	  assert(a <= 3);
	  argp = a;
	  break;

     case ARG:
          argp--;
          move_immed(R(argp), a);
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
	  branch(opB, lab);
          break;

     default:
	  badop();
     }
}

static void vm_load_store(operation op, int ra, int rb, int c);

void vm_gen2rr(operation op, vmreg rega, vmreg regb) {
     int ra = rega->vr_reg, rb = regb->vr_reg;

     vm_debug1(op, 2, rega->vr_name, regb->vr_name);
     vm_space(0);

     switch (op) {
     case MOV:
          write_reg(ra);
	  if (isfloat(ra) && isfloat(rb))
	       op_rr(opFMOVD, ra, rb);
          else if (isfloat(ra))
	       fmsr(ra, rb);	// Can only happen via SYSTEM.VAL etc.
          else if (isfloat(rb))
               fmrs(ra, rb);	// Ditto
	  else
               move_reg(ra, rb); 
	  break;

     case SXTOFF:
          write_reg(ra);
          move_reg(ra, rb);
          break;
          
     case NEG:    
          write_reg(ra);
	  arith_immed(opRSB, ra, rb, 0); break;

     case NOT:    
          write_reg(ra);
	  op_rr(opMVN, ra, rb); break;

     case NEGF:
	  op_rr(opFNEGS, ra, rb); break;

     case NEGD:
	  op_rr(opFNEGD, ra, rb); break;

     case CONVIF:
	  fmsr(ra, rb); 
	  op_rr(opFSITOS, ra, ra); 
	  break;

     case CONVID:
	  fmsr(ra, rb); 
	  op_rr(opFSITOD, ra, ra); 
	  break;

     case CONVFI:
          op_rr(opFTOSIZS, F14, rb);
          fmrs(ra, F14);
          break;

     case CONVDI:
          op_rr(opFTOSIZD, F14, rb);
          fmrs(ra, F14);
          break;

     case CONVFD:
	  op_rr(opFCVTDS, ra, rb); break;

     case CONVDF:
	  op_rr(opFCVTSD, ra, rb); break;

     case SXT: 
          write_reg(ra);
	  op_rr(opSXTH, ra, rb); break;

     default:
          vm_load_store(op, ra, rb, 0);
     }
}

void vm_gen2ri(operation op, vmreg rega, int b) {
     int ra = rega->vr_reg;

     vm_debug1(op, 2, rega->vr_name, fmt_val(b));
     vm_space(0);

     switch (op) {
     case MOV: 
          write_reg(ra);
          move_immed(ra, b);
          break;

     case GETARG: 
          write_reg(ra);
	  move_reg(ra, R(b));
          break;

     case LDKW:
          write_reg(ra);
          // Floating-point loads only have a 1024-byte range,
          // so we use an integer register as a staging post.
	  if (isfloat(ra))
	       fmsr(ra, const_reg(* (int *) b));
	  else
	       move_immed(ra, * (int *) b);
	  break;

     default:
          vm_load_store(op, ra, NOREG, b);
     }
}

void vm_gen2rj(operation op, vmreg rega, vmlabel b) {
     int ra = rega->vr_reg;

     vm_debug1(op, 2, rega->vr_name, fmt_lab(b));
     vm_space(0);

     switch (op) {
     case MOV:
          write_reg(ra);
          code_addr r = vm_literal(4);
          load_store(opLDR, ra, PC, r - (pc+8));
          vm_branch(ABS, r, b);
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
     case ADDOFF:
          write_reg(ra);
	  op_rrr(opADD, ra, rb, rc); break;
     case AND: 
          write_reg(ra);
	  op_rrr(opAND, ra, rb, rc); break;
     case XOR: 
          write_reg(ra);
	  op_rrr(opEOR, ra, rb, rc); break;
     case OR: 
          write_reg(ra);
	  op_rrr(opORR, ra, rb, rc); break;
     case SUB: 
          write_reg(ra);
	  op_rrr(opSUB, ra, rb, rc); break;
     case MUL: 
          write_reg(ra);
	  op_mul(opMUL, ra, rb, rc); break;

     case LSH: 
          write_reg(ra);
	  shift_r(opLSL, ra, rb, rc); break;
     case RSH: 
          write_reg(ra);
	  shift_r(opASR, ra, rb, rc); break;
     case RSHU: 
          write_reg(ra);
	  shift_r(opLSR, ra, rb, rc); break;
     case ROR:
          write_reg(ra);
          shift_r(opROR, ra, rb, rc); break;

     case ADDF:
	  op_rrr(opFADDS, ra, rb, rc); break;
     case SUBF:
	  op_rrr(opFSUBS, ra, rb, rc); break;
     case MULF:
	  op_rrr(opFMULS, ra, rb, rc); break;
     case DIVF:
	  op_rrr(opFDIVS, ra, rb, rc); break;

     case ADDD:
	  op_rrr(opFADDD, ra, rb, rc); break;
     case SUBD:
	  op_rrr(opFSUBD, ra, rb, rc); break;
     case MULD:
	  op_rrr(opFMULD, ra, rb, rc); break;
     case DIVD:
	  op_rrr(opFDIVD, ra, rb, rc); break;

     case EQ: 
          write_reg(ra);
	  bool_reg(opMOVEQ, ra, rb, rc); break;
     case GEQ:
          write_reg(ra);
	  bool_reg(opMOVGE, ra, rb, rc); break;
     case GT: 
          write_reg(ra);
	  bool_reg(opMOVGT, ra, rb, rc); break;
     case LEQ:
          write_reg(ra);
	  bool_reg(opMOVLE, ra, rb, rc); break;
     case LT: 
          write_reg(ra);
	  bool_reg(opMOVLT, ra, rb, rc); break;
     case NEQ:
          write_reg(ra);
	  bool_reg(opMOVNE, ra, rb, rc); break;

     case EQF:
          write_reg(ra);
	  bool_reg_f(opMOVEQ, ra, rb, rc); break;
     case LTF:
          write_reg(ra);
	  bool_reg_f(opMOVLO, ra, rb, rc); break;
     case LEQF:
          write_reg(ra);
	  bool_reg_f(opMOVLS, ra, rb, rc); break;
     case GTF:
          write_reg(ra);
	  bool_reg_f(opMOVGT, ra, rb, rc); break;
     case GEQF:
          write_reg(ra);
	  bool_reg_f(opMOVGE, ra, rb, rc); break;
     case NEQF:
          write_reg(ra);
	  bool_reg_f(opMOVNE, ra, rb, rc); break;

     case EQD:
          write_reg(ra);
	  bool_reg_d(opMOVEQ, ra, rb, rc); break;
     case GEQD:
          write_reg(ra);
	  bool_reg_d(opMOVGE, ra, rb, rc); break;
     case GTD:
          write_reg(ra);
	  bool_reg_d(opMOVGT, ra, rb, rc); break;
     case LEQD:
          write_reg(ra);
	  bool_reg_d(opMOVLS, ra, rb, rc); break;
     case LTD:
          write_reg(ra);
	  bool_reg_d(opMOVLO, ra, rb, rc); break;
     case NEQD:
          write_reg(ra);
	  bool_reg_d(opMOVNE, ra, rb, rc); break;

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
          write_reg(ra);
          add_immed(ra, rb, c); break;
     case SUB: 
          write_reg(ra);
	  arith_signed(opSUB, opADD, ra, rb, c); break;
     case AND: 
          write_reg(ra);
	  arith_compl(opAND, opBIC, ra, rb, c); break;
     case OR: 
          write_reg(ra);
	  arith_immed(opORR, ra, rb, c); break;
     case XOR: 
          write_reg(ra);
	  arith_immed(opEOR, ra, rb, c); break;
     case MUL:
          write_reg(ra);
	  op_mul(opMUL, ra, rb, const_reg(c)); break;

     case LSH: 
          write_reg(ra);
	  shift_i(opLSL, ra, rb, c); break;
     case RSH: 
          write_reg(ra);
	  shift_i(opASR, ra, rb, c); break;
     case RSHU: 
          write_reg(ra);
	  shift_i(opLSR, ra, rb, c); break;
     case ROR:
          write_reg(ra);
          shift_i(opROR, ra, rb, c); break;

     case EQ:
          write_reg(ra);
	  bool_immed(opMOVEQ, ra, rb, c); break;
     case GEQ:
          write_reg(ra);
	  bool_immed(opMOVGE, ra, rb, c); break;
     case GT: 
          write_reg(ra);
	  bool_immed(opMOVGT, ra, rb, c); break;
     case LEQ:
          write_reg(ra);
	  bool_immed(opMOVLE, ra, rb, c); break;
     case LT: 
          write_reg(ra);
	  bool_immed(opMOVLT, ra, rb, c); break;
     case NEQ:
          write_reg(ra);
	  bool_immed(opMOVNE, ra, rb, c); break;

     default:
          vm_load_store(op, ra, rb, c);
     }
}

static void vm_load_store(operation op, int ra, int rb, int c) {
     switch(op) {
     case LDW:
          write_reg(ra);
	  if (! isfloat(ra)) 
               load_store(opLDR, ra, rb, c); 
          else 
               load_store_f(opFLDS, ra, rb, c);
	  break;

     case STW: 
	  if (! isfloat(ra)) 
               load_store(opSTR, ra, rb, c); 
          else
               load_store_f(opFSTS, ra, rb, c); 
	  break;

     case LDS:
          write_reg(ra);
	  load_store_x(opLDSH, ra, rb, c); break;
     case LDSU: 
          write_reg(ra);
          load_store_x(opLDRH, ra, rb, c); break;
     case STS: 
	  load_store_x(opSTRH, ra, rb, c); break;

     case LDC:
          write_reg(ra);
	  load_store_x(opLDSB, ra, rb, c); break;
     case LDCU: 
          write_reg(ra);
	  load_store(opLDRB, ra, rb, c); break;
     case STC: 
	  load_store(opSTRB, ra, rb, c); break;

     case LDQ: 
          load_store_d(opFLDS, ra, rb, c); break;
     case STQ:    
          load_store_d(opFSTS, ra, rb, c); break;

     default:
          badop();
     }
}

void vm_gen3rrj(operation op, vmreg rega, vmreg regb, vmlabel lab) {
     int ra = rega->vr_reg, rb = regb->vr_reg;

     vm_debug1(op, 3, rega->vr_name, regb->vr_name, fmt_lab(lab));
     vm_space(0);

     switch (op) {
     case BEQ: 
	  br_reg(opBEQ, ra, rb, lab); break;
     case BGEQ: 
	  br_reg(opBGE, ra, rb, lab); break;
     case BGT: 
	  br_reg(opBGT, ra, rb, lab); break;
     case BLEQ: 
	  br_reg(opBLE, ra, rb, lab); break;
     case BLT: 
	  br_reg(opBLT, ra, rb, lab); break;
     case BNEQ: 
	  br_reg(opBNE, ra, rb, lab); break;
     case BLTU: 
	  br_reg(opBLO, ra, rb, lab); break;
     case BGEQU:
	  br_reg(opBHS, ra, rb, lab); break;
     case BGTU:
	  br_reg(opBHI, ra, rb, lab); break;
     case BLEQU:
	  br_reg(opBLS, ra, rb, lab); break;

/*
Result of FCMP
	<	=	>	Unord
NZCV =	1000	0110	0010	0011

Keiko						ARM
-----						---
BEQ     F       T       F       F       Z	BEQ
BLT     T       F       F       F       !C	BLO (or BMI)
BLE     T       T       F       F       Z|!C    BLS
BGT     F       F       T       F       !Z&N=V  BGT
BGE     F       T       T       F       N=V	BGE
BNE     T       F       T       T       !Z	BNE
BNLT    F       T       T       T       C	BHS (or BPL)
BNLE    F       F       T       T       !Z&C    BHI
BNGT    T       T       F       T       Z|N!=V  BLE
BNGE    T       F       F       T       N!=V	BLT
*/

     case BEQF:
	  br_reg_f(opBEQ, ra, rb, lab); break;
     case BLTF:
	  br_reg_f(opBLO, ra, rb, lab); break;
     case BLEQF:
	  br_reg_f(opBLS, ra, rb, lab); break;
     case BGTF:
	  br_reg_f(opBGT, ra, rb, lab); break;
     case BGEQF:
	  br_reg_f(opBGE, ra, rb, lab); break;
     case BNEQF:
	  br_reg_f(opBNE, ra, rb, lab); break;
     case BNLTF:
	  br_reg_f(opBHS, ra, rb, lab); break;
     case BNLEQF:
	  br_reg_f(opBHI, ra, rb, lab); break;
     case BNGTF:
	  br_reg_f(opBLE, ra, rb, lab); break;
     case BNGEQF:
	  br_reg_f(opBLT, ra, rb, lab); break;

     case BEQD:
	  br_reg_d(opBEQ, ra, rb, lab); break;
     case BLTD:
	  br_reg_d(opBLO, ra, rb, lab); break;
     case BLEQD:
	  br_reg_d(opBLS, ra, rb, lab); break;
     case BGTD:
	  br_reg_d(opBGT, ra, rb, lab); break;
     case BGEQD:
	  br_reg_d(opBGE, ra, rb, lab); break;
     case BNEQD:
	  br_reg_d(opBNE, ra, rb, lab); break;
     case BNLTD:
	  br_reg_d(opBHS, ra, rb, lab); break;
     case BNLEQD:
	  br_reg_d(opBHI, ra, rb, lab); break;
     case BNGTD:
	  br_reg_d(opBLE, ra, rb, lab); break;
     case BNGEQD:
	  br_reg_d(opBLT, ra, rb, lab); break;

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
	  br_immed(opBEQ, ra, b, lab); break;
     case BGEQU: 
	  br_immed(opBHS, ra, b, lab); break;
     case BGEQ: 
	  br_immed(opBGE, ra, b, lab); break;
     case BGT: 
	  br_immed(opBGT, ra, b, lab); break;
     case BLEQ: 
	  br_immed(opBLE, ra, b, lab); break;
     case BLTU: 
	  br_immed(opBLO, ra, b, lab); break;
     case BLT: 
	  br_immed(opBLT, ra, b, lab); break;
     case BNEQ: 
	  br_immed(opBNE, ra, b, lab); break;
     case BGTU:
	  br_immed(opBHI, ra, b, lab); break;
     case BLEQU:
	  br_immed(opBLS, ra, b, lab); break;

     default:
	  badop();
     }
}

static code_addr cploc;
static code_addr entry;
static int locals;

code_addr vm_prelude(int n, int locs) {
     nlits = 0;
     regmap = 0;
     retchain = NULL;
     cploc = pc;
     locals = (locs+7)&~7;
     word(0);

     entry = pc;
     move_reg(IP, SP);
     vm_debug2("stmfd sp!, ...\n");
     word(0);
     move_reg(FP, SP);
     if (locals > 0) arith_immed(opSUB, SP, SP, locals);

     return entry;
}

void vm_chain(code_addr p) {
     code_addr loc = pc;
     branch_i(opB, 0);
     vm_patch(loc, p);
     nlits = 0;
}

int parity(short x) {
     x = (x ^ (x >> 8)) & 0xff;
     x = (x ^ (x >> 4)) & 0xf;
     x = (x ^ (x >> 2)) & 0x3;
     x = (x ^ (x >> 1)) & 0x1;
     return x;
}

/* vm_postlude -- finish compiling procedure */
void vm_postlude(void) {
     regmap &= range(4, 10);
     // Must save an even number of registers overall
     if (parity(regmap) == 0) regmap |= (regmap+0x10) & ~regmap;
     vm_debug2("regmap = %#x\n", regmap);

     // stmfd! sp, {r4-r10, fp, ip, lr}
     * (int *) (entry + 4) =
          fmt_instr(GETOP(opSTMFDw), 0, reg(SP),
		    regmap|bit(FP)|bit(IP)|bit(LR));

     for (code_addr p = retchain, q; p != NULL; p = q) {
          q = * (code_addr *) p;
          * (int *) p =
   	       fmt_instr(GETOP(opLDMFD), 0, reg(FP),
			 regmap|bit(FP)|bit(SP)|bit(PC));
     }
}

#ifdef DEBUG
int vm_print(code_addr p) {
     printf("%08x", * (unsigned *) p);
     return 4;
}
#endif
