/*
 * vmomega.c
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
#include <limits.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

// REGISTERS

#define R_0 0
#define R_1 1
#define R_V0 2  // Return values
#define R_V1 3
#define R_A0 4  // Arguments
#define R_A1 5
#define R_A2 6
#define R_A3 7
#define R_T0 8  // Temps
#define R_T1 9
#define R_T2 10
#define R_T3 11
#define R_T4 12
#define R_T5 13 
#define R_T6 14 
#define R_T7 15 
#define R_S0 16 // Register variables
#define R_S1 17
#define R_S2 18
#define R_S3 19
// S4-S7, T8 unused
#define R_T9 25 // Must contain routine address at call
// K0, K1, GP unused
#define R_SP 29 // Stack pointer
#define R_FP 30 // Frame pointer
#define R_RA 31 // Return address

struct _vmreg 
     reg_i0 = { "I0", R_T0 },
     reg_i1 = { "I1", R_T1 },
     reg_i2 = { "I2", R_T2 },
     reg_i3 = { "I3", R_T3 },
     reg_i4 = { "I4", R_T4 },
     reg_i5 = { "I5", R_T5 },
     reg_i6 = { "I6", R_T6 },
     reg_i7 = { "I7", R_T7 },
     reg_v0 = { "V0", R_S0 },
     reg_v1 = { "V1", R_S1 },
     reg_v2 = { "V2", R_S2 },
     reg_v3 = { "V3", R_S3 },
     reg_rr = { "RET", R_V0 },
     reg_fp = { "BASE", R_FP };

const int vm_nvreg = 4, vm_nireg = 9, vm_nfreg = 0;
const vmreg vm_ireg[] = {
     &reg_v0, &reg_v1, &reg_v2, &reg_v3,
     &reg_i0, &reg_i1, &reg_i2, &reg_i3,
     &reg_i4, &reg_i5, &reg_i6, &reg_i7
};
const vmreg vm_freg[] = { };
const vmreg vm_ret = &reg_rr, vm_base = &reg_fp;

#ifdef DEBUG

char *regname[] = {
     "$0", "$1", "$v0", "$v1", "$a0", "$a1", "$a2", "$a3",
     "$t0", "$t1", "$t2", "$t3", "$t4", "$t5", "$t6", "$t7", 
     "$s0", "$s1", "$s2", "$s3", "$s4", "$s5", "$s6", "$s7",
     "$t8", "$t9", "$k0", "$k1", "$gp", "$sp", "$fp", "$ra"
};

static char *fmt_addr(int rs, int imm) {
     static char buf[32];

     if (imm == 0)
          sprintf(buf, "(%s)", regname[rs]);
     else
          sprintf(buf, "%d(%s)", imm, regname[rs]);

     return buf;
}


// The usual macro madness

#define MNEM(mnem, op) mnem, op
#define OPDECL const char *mnem, int op
#define OPDECL2 const char *mnem2, int op2
#define OP mnem, op
#define OP2 mnem2, op2

#define _GETOP(mnem, op) op
#define GETOP(op) _GETOP(op)

#else

#define MNEM(mnem, op) op
#define OPDECL int op
#define OPDECL2 int op2
#define OP op
#define OP2 op2
#define GETOP(op) op

#endif


// OPCODES

// R-format: 0 rs rt rd op>>8 op
#define opSLLV	MNEM("sllv",   0x04)
#define opSRLV	MNEM("srlv",   0x06)
#define opROTRV	MNEM("rotrv", 0x106)
#define opSRAV	MNEM("srav",   0x07)
#define opJR	MNEM("jr",     0x08)
#define opJALR	MNEM("jalr",   0x09)
#define opMOVZ	MNEM("movz",   0x0a)
#define opMOVN	MNEM("movn",   0x0b)
#define opADDU	MNEM("addu",   0x21)
#define opSUBU	MNEM("subu",   0x23)
#define opAND	MNEM("and",    0x24)
#define opOR	MNEM("or",     0x25)
#define opXOR	MNEM("xor",    0x26)
#define opNOR	MNEM("nor",    0x27)
#define opSLT	MNEM("slt",    0x2a)
#define opSLTU	MNEM("sltu",   0x2b)

// Shift: 0 op>>8 rt rd sa op
#define opSLL	MNEM("sll",    0x00)
#define opSRL	MNEM("srl",    0x02)
#define opROTR	MNEM("rotr",  0x102)
#define opSRA	MNEM("sra",    0x03)

// RegImm: REGIMM rs op imm
#define REGIMM 0x01
#define opBLTZ	MNEM("bltz",   0x00)
#define opBGEZ	MNEM("bgez",   0x01)

// Jumps: op tgt
#define opJ     MNEM("j",      0x02)
#define opJAL   MNEM("jal",    0x03) // use JALR R_T9 instead for ABI

// Immediate: op rs rt imm
#define opB	MNEM("b",      0x04) // == BEQ
#define opBEQ	MNEM("beq",    0x04)
#define opBNE	MNEM("bne",    0x05)
#define opBLEZ	MNEM("blez",   0x06)
#define opBGTZ	MNEM("bgtz",   0x07)
#define opADDIU	MNEM("addiu",  0x09)
#define opSLTI	MNEM("slti",   0x0a)
#define opSLTIU	MNEM("sltiu",  0x0b)
#define opANDI	MNEM("andi",   0x0c)
#define opLI    MNEM("li",     0x0d) // == ORI
#define opORI	MNEM("ori",    0x0d)
#define opXORI	MNEM("xori",   0x0e)
#define opLUI	MNEM("lui",    0x0f)
#define opLB	MNEM("lb",     0x20)
#define opLH	MNEM("lh",     0x21)
#define opLW	MNEM("lw",     0x23)
#define opLBU	MNEM("lbu",    0x24)
#define opLHU	MNEM("lhu",    0x25)
#define opSB	MNEM("sb",     0x28)
#define opSH	MNEM("sh",     0x29)
#define opSW	MNEM("sw",     0x2b)

// Special2: SPECIAL2 rs rt rd 0 0x02
#define SPECIAL2 0x1c
#define opMUL	MNEM("mul",    0x02)

// BSHFL: SPECIAL3 0 rt rd op BSHFL
#define SPECIAL3 0x1f
#define BSHFL 0x20
#define opSEB	MNEM("seb",    0x10)
#define opSEH	MNEM("seh",    0x18)


// LOW-LEVEL ASSEMBLY

static void rformat(int op, int rs, int rt, int rd, int op2, int op3) {
     word(((op&0x3f)<<26) | ((rs&0x1f)<<21) | ((rt&0x1f)<<16)
          | ((rd&0x1f)<<11) | ((op2&0x1f)<<6) | (op3&0x3f));
}

static void iformat(int op, int rs, int rt, int imm) {
     word(((op&0x3f)<<26) | ((rs&0x1f)<<21) | ((rt&0x1f)<<16) | (imm&0xffff));
}

static unsigned jumpfmt(int op, unsigned tgt) {
#define TOP2 0xc0000000
     if ((tgt&TOP2) != ((int) pc&TOP2))
          vm_panic("J format error");

     return(((op&0x3f)<<26) | ((tgt>>2)&0x3ffffff));
}

#define jump(tgt) jumpfmt(GETOP(opJ), (unsigned) (tgt))
#define jformat(op, tgt) word(jumpfmt(op, tgt))

#define nop() word(0)

static void arith_r(OPDECL, int rd, int rs, int rt) {
     vm_debug2("%s %s, %s, %s", mnem, regname[rd], regname[rs], regname[rt]);
     rformat(0, rs, rt, rd, op>>8, op&0x3f);
     vm_done();
}

static void move_r(int rd, int rs) {
     vm_debug2("move %s, %s", regname[rd], regname[rs]);
     rformat(0, rs, R_0, rd, 0, GETOP(opOR));
     vm_done();
}

static void bshfl(OPDECL, int rd, int rt) {
     vm_debug2("%s %s, %s", mnem, regname[rd], regname[rt]);
     rformat(SPECIAL3, 0, rt, rd, op, BSHFL);
     vm_done();
}

static void special2(OPDECL, int rd, int rs, int rt) {
     vm_debug2("%s %s, %s, %s", mnem, regname[rd], regname[rs], regname[rt]);
     rformat(SPECIAL2, rs, rt, rd, 0, op);
     vm_done();
}

static void arith_i(OPDECL, int rt, int rs, int imm) {
     vm_debug2("%s %s, %s, %s", mnem, regname[rt], regname[rs], fmt_val(imm));
     iformat(op, rs, rt, imm);
     vm_done();
}

static void load_i(OPDECL, int rt, int imm) {
     vm_debug2("%s %s, %s", mnem, regname[rt], fmt_val(imm));
     iformat(op, 0, rt, imm);
     vm_done();
}

static void ldst_inst(OPDECL, int rt, int rs, int imm) {
     vm_debug2("%s %s, %s", mnem, regname[rt], fmt_addr(rs, imm));
     iformat(op, rs, rt, imm);
     vm_done();
}

static void shift_i(OPDECL, int rd, int rt, int sa) {
     vm_debug2("%s %s, %s, %d", mnem, regname[rd], regname[rt], sa);
     rformat(0, op>>8, rt, rd, sa, op&0x3f);
     vm_done();
}

static void jump_i(OPDECL, int imm) {
     vm_debug2("%s %#x; nop", mnem, imm);
     jformat(op, imm); nop();
     vm_done();
}

static void jump_r(OPDECL, int rd, int rs) {
     vm_debug2("%s %s; nop", mnem, regname[rs]); // Leave rd implicit
     rformat(0, rs, 0, rd, 0, op); nop();
     vm_done();
}

static void branch(OPDECL, vmlabel lab) {
     vm_debug2("%s %s; nop", mnem, fmt_lab(lab));
     code_addr loc = pc;
     iformat(op, 0, 0, 0); nop();
     vm_branch(BRANCH, loc, lab);
     vm_done();
}

static void branch_r(OPDECL, int rs, vmlabel lab) {
     vm_debug2("%s %s, %s; nop", mnem, regname[rs], fmt_lab(lab));
     code_addr loc = pc;
     iformat(op, rs, 0, 0); nop();
     vm_branch(BRANCH, loc, lab);
     vm_done();
}

static void branch_rr(OPDECL, int rs, int rt, vmlabel lab) {
     vm_debug2("%s %s, %s, %s; nop",
               mnem, regname[rs], regname[rt], fmt_lab(lab));
     code_addr loc = pc;
     iformat(op, rs, rt, 0); nop();
     vm_branch(BRANCH, loc, lab);
     vm_done();
}

static void br_regimm(OPDECL, int rs, vmlabel lab) {
     vm_debug2("%s %s, %s; nop", mnem, regname[rs], fmt_lab(lab));
     code_addr loc = pc;
     iformat(REGIMM, rs, op, 0); nop();
     vm_branch(BRANCH, loc, lab);
     vm_done();
}


// HIGH LEVEL INSTRUCTIONS

int unsigned16(int x) { return ((x) >= 0 && (x) < 65536); }
int signed16(int x) { return ((x) >= -32768 && (x) < 32768); }

static void move_immed(int rd, int imm) {
     if (imm == 0)
          move_r(rd, R_0);
     else if (unsigned16(imm))
          load_i(opLI, rd, imm);
     else if (signed16(imm))
          arith_i(opADDIU, rd, R_0, imm);
     else {
          load_i(opLUI, rd, (imm>>16)&0xffff);
          arith_i(opORI, rd, rd, imm&0xffff);
     }
}

static int const_reg(int imm) {
     if (imm == 0)
          return R_0;
     else {
          move_immed(R_1, imm);
          return R_1;
     }
}

static int shifted(int ra, int s) {
     if (s == 0)
          return ra;
     else {
          shift_i(opSLL, R_1, ra, s);
          return R_1;
     }
}

static int index_reg(int ra, int rb, int s) {
     arith_r(opADDU, R_1, ra, shifted(rb, s));
     return R_1;
}

static void arith_immed(OPDECL, OPDECL2, int (*fits)(int),
                        int rd, int rt, int imm) {
     if (fits(imm))
          arith_i(OP, rd, rt, imm);
     else
          arith_r(OP2, rd, rt, const_reg(imm));
}
                    
#define opADDIU_ opADDIU, opADDU, signed16
#define opSLTI_  opSLTI, opSLT, signed16
#define opSLTIU_ opSLTIU, opSLTU, signed16
#define opANDI_  opANDI, opAND, unsigned16
#define opORI_   opORI, opOR, unsigned16
#define opXORI_  opXORI, opXOR, unsigned16

static void addi(int rd, int rs, int imm) {
     arith_immed(opADDIU_, rd, rs, imm);
}

void load_store(OPDECL, int rt, int rs, int off) {
     if (signed16(off))
          ldst_inst(OP, rt, rs, off);
     else {
          short lo = (short) off;
          short hi = (off - lo) >> 16;
          assert((hi << 16) + lo == off);
          load_i(opLUI, R_1, hi);
          if (rs != R_0)
               arith_r(opADDU, R_1, R_1, rs);
          ldst_inst(OP, rt, R_1, lo);
     }
}

#define branch_lt_r(op, ra, rb, lab) \
     arith_r(op, R_1, ra, rb), branch_rr(opBNE, R_1, R_0, lab)
#define branch_geq_r(op, ra, rb, lab) \
     arith_r(op, R_1, ra, rb), branch_rr(opBEQ, R_1, R_0, lab)

#define branch_lt_i(op, ra, b, lab)        \
     arith_immed(op, R_1, ra, b), branch_rr(opBNE, R_1, R_0, lab)
#define branch_geq_i(op, ra, b, lab)        \
     arith_immed(op, R_1, ra, b), branch_rr(opBEQ, R_1, R_0, lab)



// INTERFACE ROUTINES

static int argp;

#define badop() vm_unknown(__FUNCTION__, op)

void vm_gen1r(operation op, vmreg rega) {
     int rx = rega->vr_reg;

     vm_debug1(op, 1, rega->vr_name);
     vm_space(0);

     switch (op) {
     case JUMP: 
	  jump_r(opJR, 0, rx); break;

     case CALL:
          assert(argp == 0);
          // PIC conventions require the subroutine address in T9
          move_r(R_T9, rx);
          jump_r(opJALR, R_RA, R_T9);
          break;
          
     case ARG:
	  argp--;
	  move_r(R_A0+argp, rx);
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
          assert(argp == 0);
          move_immed(R_T9, a);
	  jump_r(opJALR, R_RA, R_T9);
          break;

     case PREP:
	  assert(a <= 3);
	  argp = a;
	  break;

     case ARG:
          argp--;
          move_immed(R_A0+argp, a);
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
	  branch(opB, lab); break;

     default:
	  badop();
     }
}

static void vm_load_store(operation op, int rx, int ry, int c);
static void vm_load_storex(operation op, int ra, int rb, int rc, int s);

void vm_gen2rr(operation op, vmreg rega, vmreg regb) {
     int rx = rega->vr_reg, ry = regb->vr_reg;

     vm_debug1(op, 2, rega->vr_name, regb->vr_name);
     vm_space(0);

     switch (op) {
     case MOV:
          if (rx == ry) break;
          move_r(rx, ry);
          break;

     case NEG:    
	  arith_r(opSUBU, rx, R_0, ry); break;
     case NOT:    
	  arith_r(opNOR, rx, ry, R_0); break;
     case CONVis: 
	  bshfl(opSEH, rx, ry);
	  break;

     default:
          vm_load_store(op, rx, ry, 0);
     }
}

void vm_gen2ri(operation op, vmreg rega, int b) {
     int rx = rega->vr_reg;

     vm_debug1(op, 2, rega->vr_name, fmt_val(b));
     vm_space(0);

     switch (op) {
     case MOV: 
          move_immed(rx, b); break;
     case GETARG: 
	  move_r(rx, R_A0+b); break;
     default:
          vm_load_store(op, rx, R_0, b);
     }
}

void vm_gen2rj(operation op, vmreg rega, vmlabel b) {
     int rx = rega->vr_reg;

     vm_debug1(op, 2, rega->vr_name, fmt_lab(b));
     vm_space(0);

     switch (op) {
     case MOV: ;
          code_addr i1 = pc;
          load_i(opLUI, rx, 0);
          code_addr i2 = pc;
          arith_i(opORI, rx, rx, 0);
          vm_branch(HI16, i1, b);
          vm_branch(LO16, i2, b);
          break;
              
     default:
          badop();
     }
}

void vm_gen3rrr(operation op, vmreg rega, vmreg regb, vmreg regc) {
     int rx = rega->vr_reg, ry = regb->vr_reg, rz = regc->vr_reg;

     vm_debug1(op, 3, rega->vr_name, regb->vr_name, regc->vr_name);
     vm_space(0);

     switch (op) {
     case ADD: 
	  arith_r(opADDU, rx, ry, rz); break;
     case AND: 
	  arith_r(opAND, rx, ry, rz); break;
     case XOR: 
	  arith_r(opXOR, rx, ry, rz); break;
     case OR: 
	  arith_r(opOR, rx, ry, rz); break;
     case SUB: 
	  arith_r(opSUBU, rx, ry, rz); break;
     case MUL: 
	  special2(opMUL, rx, ry, rz); break;

     case LSH: 
	  arith_r(opSLLV, rx, rz, ry); break;
     case RSH: 
	  arith_r(opSRAV, rx, rz, ry); break;
     case RSHu: 
	  arith_r(opSRLV, rx, rz, ry); break;
     case ROR:
          arith_r(opROTRV, rx, rz, ry); break;

     case EQ: 
          arith_r(opSUBU, R_1, ry, rz);
          arith_i(opSLTIU, rx, R_1, 1); break;
     case NE:
          arith_r(opSUBU, R_1, ry, rz);
          arith_r(opSLTU, rx, R_0, R_1); break;
     case LT: 
          arith_r(opSLT, rx, ry, rz); break;
     case GE:
          arith_r(opSLT, R_1, ry, rz);
          arith_i(opXORI, rx, R_1, 1); break;
     case GT: 
          arith_r(opSLT, rx, rz, ry); break;
     case LE:
          arith_r(opSLT, R_1, rz, ry);
          arith_i(opXORI, rx, R_1, 1); break;

     default:
	  vm_load_storex(op, rx, ry, rz, 0);
     }
}

void vm_gen4rrrs(operation op, vmreg rega, vmreg regb, vmreg regc, int s) {
     int ra = rega->vr_reg, rb = regb->vr_reg, rc = regc->vr_reg;

     vm_debug1(op, 4, rega->vr_name, regb->vr_name, regc->vr_name, fmt_val(s));
     vm_space(0);
     
     switch (op) {
     case ADD:
          arith_r(opADDU, ra, rb, shifted(rc, s));
          break;

     default:
          vm_load_storex(op, ra, rb, rc, s);
     }
}

static void vm_load_storex(operation op, int ra, int rb, int rc, int s) {
     switch(op) {
     case LDW:
          load_store(opLW, ra, index_reg(rb, rc, s), 0);
	  break;
     case STW: 
          load_store(opSW, ra, index_reg(rb, rc, s), 0);
	  break;
     case LDS:
          load_store(opLH, ra, index_reg(rb, rc, s), 0);
          break;
     case LDSu: 
          load_store(opLHU, ra, index_reg(rb, rc, s), 0);
          break;
     case STS: 
          load_store(opSH, ra, index_reg(rb, rc, s), 0);
          break;
     case LDB:
          load_store(opLB, ra, index_reg(rb, rc, s), 0);
          break;
     case LDBu: 
          load_store(opLBU, ra, index_reg(rb, rc, s), 0);
          break;
     case STB: 
          load_store(opSB, ra, index_reg(rb, rc, s), 0);
          break;
     default:
	  badop();
     }
}

void vm_gen3rri(operation op, vmreg rega, vmreg regb, int c) {
     int rx = rega->vr_reg, ry = regb->vr_reg;

     vm_debug1(op, 3, rega->vr_name, regb->vr_name, fmt_val(c));
     vm_space(0);

     switch (op) {
     case ADD: 
          addi(rx, ry, c); break;
     case SUB: 
	  addi(rx, ry, -c); break;
     case AND: 
	  arith_immed(opANDI_, rx, ry, c); break;
     case OR: 
	  arith_immed(opORI_, rx, ry, c); break;
     case XOR: 
	  arith_immed(opXORI_, rx, ry, c); break;
     case MUL:
	  special2(opMUL, rx, ry, const_reg(c)); break;

     case LSH: 
	  shift_i(opSLL, rx, ry, c); break;
     case RSH: 
	  shift_i(opSRA, rx, ry, c); break;
     case RSHu: 
	  shift_i(opSRL, rx, ry, c); break;
     case ROR:
          shift_i(opROTR, rx, ry, c); break;

     case EQ:
          addi(R_1, ry, -c);
          arith_i(opSLTIU, rx, R_1, 1);
          break;
     case NE:
	  addi(R_1, ry, -c);
          arith_r(opSLTU, rx, R_0, R_1);
          break;
     case LT: 
          arith_immed(opSLTI_, rx, ry, c); break;
     case GE:
	  arith_immed(opSLTI_, R_1, ry, c);
          arith_i(opXORI, rx, R_1, 1); break;
     case LE:
          if (c == INT_MAX)
               move_immed(rx, 1);
          else
               arith_immed(opSLTI_, rx, ry, c+1);
          break;
     case GT: 
          if (c == INT_MAX)
               move_immed(rx, 0);
          else {
               arith_immed(opSLTI_, R_1, ry, c+1);
               arith_i(opXORI, rx, R_1, 1);
          }
          break;

     default:
          vm_load_store(op, rx, ry, c);
     }
}

static void vm_load_store(operation op, int rx, int ry, int c) {
     switch(op) {
     case LDW:
          load_store(opLW, rx, ry, c); break;
     case STW: 
          load_store(opSW, rx, ry, c); break;
     case LDS:
	  load_store(opLH, rx, ry, c); break;
     case LDSu: 
          load_store(opLHU, rx, ry, c); break;
     case STS: 
	  load_store(opSH, rx, ry, c); break;
     case LDB:
	  load_store(opLB, rx, ry, c); break;
     case LDBu: 
	  load_store(opLBU, rx, ry, c); break;
     case STB: 
	  load_store(opSB, rx, ry, c); break;

     default:
          badop();
     }
}

void branch_const(operation op, int rx, int b, vmlabel lab) {
     switch (op) {
     case BEQ: 
	  branch_rr(opBEQ, rx, const_reg(b), lab); break;
     case BNE: 
	  branch_rr(opBNE, rx, const_reg(b), lab); break;
     case BLT: 
          if (b == 0)
               br_regimm(opBLTZ, rx, lab);
          else
               branch_lt_i(opSLTI_, rx, b, lab);
          break;
     case BGE: 
	  if (b == 0)
               br_regimm(opBGEZ, rx, lab);
          else
               branch_geq_i(opSLTI_, rx, b, lab);
          break;
     case BGT: 
          if (b == 0)
               branch_r(opBGTZ, rx, lab);
          else if (b != INT_MAX)
               branch_geq_i(opSLTI_, rx, b+1, lab);
	  break;
     case BLE: 
          if (b == 0)
               branch_r(opBLEZ, rx, lab);
          else if (b == INT_MAX)
               branch(opB, lab);
          else
               branch_lt_i(opSLTI_, rx, b+1, lab);
	  break;

     default:
          badop();
     }
}

void vm_gen3rrj(operation op, vmreg rega, vmreg regb, vmlabel lab) {
     int rx = rega->vr_reg, ry = regb->vr_reg;

     vm_debug1(op, 3, rega->vr_name, regb->vr_name, fmt_lab(lab));
     vm_space(0);

     switch (op) {
     case BEQ: 
	  branch_rr(opBEQ, rx, ry, lab); break;
     case BNE: 
	  branch_rr(opBNE, rx, ry, lab); break;
     case BLT: 
          branch_lt_r(opSLT, rx, ry, lab); break;
     case BGE: 
          branch_geq_r(opSLT, rx, ry, lab); break;
     case BGT: 
          branch_lt_r(opSLT, ry, rx, lab); break;
     case BLE: 
          branch_geq_r(opSLT, ry, rx, lab); break;
     case BLTu: 
          branch_lt_r(opSLTU, rx, ry, lab); break;
     case BGEu:
          branch_geq_r(opSLTU, rx, ry, lab); break;
     case BGTu:
          branch_lt_r(opSLTU, ry, rx, lab); break;
     case BLEu:
          branch_geq_r(opSLTU, ry, rx, lab); break;

     default:
	  badop();
     }
}

void vm_gen3rij(operation op, vmreg rega, int b, vmlabel lab) {
     int rx = rega->vr_reg;

     vm_debug1(op, 3, rega->vr_name, fmt_val(b), fmt_lab(lab));
     vm_space(0);

     switch (op) {
     case BLTu: 
          branch_lt_i(opSLTIU_, rx, b, lab); break;
     case BGEu:
          branch_geq_i(opSLTIU_, rx, b, lab); break;
     case BLEu:
          if (b == UINT_MAX)
               branch(opB, lab);
          else
               branch_lt_i(opSLTIU_, rx, b+1, lab);
          break;
     case BGTu:
          if (b != UINT_MAX)
               branch_geq_i(opSLTIU_, rx, b+1, lab);
          break;

     default:
          branch_const(op, rx, b, lab);
     }
}

void vm_chain(code_addr p) {
#ifdef DEBUG
     if (vm_debug > 1) printf("Chain!\n");
#endif
     jump_i(opJ, (int) p);
}

void vm_patch(code_addr loc, code_addr lab) {
     unsigned *p = (unsigned *) loc;
     int off = lab - loc - 4;
     assert((off & 0x3) == 0);
     off >>= 2;
     if (off >= -0x10000 && off < 0x10000) {
          *p = (*p & ~0xffff) | (off & 0xffff);
          return;
     }

     if (*p >> 16 == GETOP(opB) << 10) {
          // An uncondx branch: make it a jump
          *p = jump((unsigned) lab);
          return;
     }

     // You're not going to like this
     unsigned *fix = (unsigned *) vm_literal(24);
     fix[0] = (*p & ~0xffff) | 0x3; //     b??? L1
     fix[1] = 0;                    //     nop
     fix[2] = jump(p+2);            //     j p+2
     fix[3] = 0;                    //     nop
     fix[4] = jump(lab);            // L1: j lab
     fix[5] = 0;                    //     nop
     __clear_cache(fix, fix+6);
     
     *p = jump(fix);
}

/*
MIPS stack frame:

	Incoming 3      Incoming space not used
        Incoming 2
        Incoming 1
old sp: Incoming 0
--------------------
        Local n-1       Round locals up to mult of 8
        ...
fp:     Local 0         FP not needed if no locals, or FP = SP+40
 +36    Saved RA
 +32    Saved FP
 +28    Saved R_S3
 +24    Saved R_S2
 +20    Saved R_S1
 +16    Saved R_S0
        Outgoing 3
        Outgoing 2
        Outgoing 1
sp:     Outgoing 0      SP = old SP - locals - 40
--------------------
*/        

static int locals;

#define FRAME 40
#define RBASE 16

int vm_prelude(int nargs, int locs) {
     code_addr entry = pc;
     locals = (locs+7)&~7;

     if (vm_debug > 0) printf("at %#x\n", (unsigned) entry);

     addi(R_SP, R_SP, -(locals+FRAME));
     ldst_inst(opSW, R_RA, R_SP, RBASE+20);
     ldst_inst(opSW, R_FP, R_SP, RBASE+16);
     ldst_inst(opSW, R_S3, R_SP, RBASE+12);
     ldst_inst(opSW, R_S2, R_SP, RBASE+8);
     ldst_inst(opSW, R_S1, R_SP, RBASE+4);
     ldst_inst(opSW, R_S0, R_SP, RBASE);
     addi(R_FP, R_SP, FRAME);

     return (int) entry;
}

void vm_postlude(void) {
     vm_space(128);
     ldst_inst(opLW, R_S0, R_SP, RBASE);
     ldst_inst(opLW, R_S1, R_SP, RBASE+4);
     ldst_inst(opLW, R_S2, R_SP, RBASE+8);
     ldst_inst(opLW, R_S3, R_SP, RBASE+12);
     ldst_inst(opLW, R_FP, R_SP, RBASE+16);
     ldst_inst(opLW, R_RA, R_SP, RBASE+20);
     addi(R_SP, R_SP, locals+FRAME);
     jump_r(opJR, R_0, R_RA);
}

#ifdef DEBUG
int vm_print(code_addr p) {
     unsigned w = * (unsigned *) p;

     switch (w >> 26) {
     case 0:
     case SPECIAL2:
     case SPECIAL3:
          printf("R%02x.%02x.%02x.%02x.%02x.%02x",
                 (w >> 26) & 0x3f, (w >> 21) & 0x1f,
                 (w >> 16) & 0x1f, (w >> 11) & 0x1f,
                 (w >> 6) & 0x1f, w & 0x3f);
          break;

     case GETOP(opJ):
     case GETOP(opJAL):
          printf("J%02x.%#x", (w >> 26) & 0x3f, (w & 0x3ffffff) << 2);
          break;

     default:
          // Assume I-format
          printf("I%02x.%02x.%02x.%#x",
                 (w >> 26) & 0x3f, (w >> 21) & 0x1f,
                 (w >> 16) & 0x1f, w & 0xffff);
     }

     return 4;
}
#endif
