/*
 * jit.c
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

#include "obx.h"
#include "keiko.h"
#include "jit.h"
#include <stdarg.h>
#include <assert.h>

/* Debug levels:
    1: VM instructions
    2: VM instructions + native instructions
    3: ... + stack values
    4: ... + register state
    5: All the above + dump code into files */


/* Decoding table */

#define __i2__(sym, ...) { #sym, { __VA_ARGS__ } },
struct _inst instrs[] = { __INSTRS__(__i2__) };

#define __o2__(op, inst, patt, arg, len) { I_##inst, patt, arg, len },
struct _decode decode[] = { __OPCODES__(__o2__) };


/* Translator */

static value *context;		/* CP for the current procedure */
static uchar *pcbase, *pclimit;	/* Code addresses */

#define konst(i) context[CP_CONST+i]

static vmlabel stack_oflo, retlab;

#define push_con(k) push(I_CON, INT, rZERO, k, 1)
#define push_reg(r) push(I_REG, INT, r, 0, 1)

/* prolog -- generate code for procedure prologue */
static code_addr prolog(const char *name, int frame, int map) {
     vmlabel lab = vm_newlab();
     code_addr entry = vm_begin(name, 1);
     vm_gen(GETARG, rBP->r_reg, 0);

     /* Check for stack overflow */
     vm_gen(BGEu, rBP->r_reg, address(stack + SLIMIT + frame), lab);
     vm_label(stack_oflo);
     push_reg(rBP);
     gcall(STKOFLO, 1);
     vm_label(lab);

     if (map != 0 && (map & 0x1) == 0) {
          // A complex map -- just clear the frame
	  vm_gen(SUB, rI0->r_reg, rBP->r_reg, frame);
          push_con(frame);
          push_con(0);
          push_reg(rI0);
	  gcall(MEMSET, 3);
     } else if (map != 0) {
          // A bitmap -- clear specified words
          map >>= 1;
	  vm_gen(MOV, rI0->r_reg, 0);
	  for (int i = -FRAME_SHIFT; i < 0; i++) {
               if ((map & 0x1) != 0)
                    vm_gen(STW, rI0->r_reg, rBP->r_reg, 4*i);
               map >>= 1;
          }
     }

     return entry;
}

/* stack_map -- find pointer map for eval stack at procedure call */
static int stack_map(uchar *pc) {
     value *r = valptr(context[CP_STKMAP]);
     if (r == NULL) return 0;
     while (pointer(r[0]) != NULL) {
	  if (pointer(r[0]) == pc) return r[1].i;
	  r += 2;
     }
     return 0;
}

/* gmonop -- generic unary operation */
static void gmonop(operation op, int rclass1, int rclass2, int s) {
     reg r1, r2;

     /* Try to reallocate the input register for the result of
	unary operations, so as to save a move on some architectures. */

     r1 = move_to_reg(1, rclass1); pop(1); 
     r2 = ralloc_suggest(rclass2, r1); unlock(1);	
     vm_gen(op, r2->r_reg, r1->r_reg);
     push(I_REG, rclass2, r2, 0, s);
}

#define imonop(op) gmonop(op, INT, INT, 1)
#define qmonop(op) gmonop(op, INT, INT, 2)
#define fmonop(op) gmonop(op, FLO, FLO, 1)
#define dmonop(op) gmonop(op, FLO, FLO, 2)

/* binop -- binary integer operation */
static void binop(operation op, int size) {
     reg r1, r2;
     ctvalue v;

     /* For binary operators, we try to reallocate the first operand
	register for the result, because this saves a move if the target
	has two-address instructions. */

     r1 = move_to_reg(2, INT); 
     v = move_to_rc(1); 
     pop(2);				
     r2 = ralloc_suggest(INT, r1); 
     unlock(2);				
     if (v->v_op == I_CON)						
	  vm_gen(op, r2->r_reg, r1->r_reg, v->v_val);
     else								
	  vm_gen(op, r2->r_reg, r1->r_reg, v->v_reg->r_reg);
     push(I_REG, INT, r2, 0, size);
}

#define ibinop(op)  binop(op, 1)
#define qbinop(op)  binop(op, 2)

/* gbinop -- general binary operation */
static void gbinop(operation op, int ty, int s) {
     reg r1, r2, r3;

     r1 = move_to_reg(2, ty); r2 = move_to_reg(1, ty); pop(2);
     r3 = ralloc_suggest(ty, r1); unlock(2);				
     vm_gen(op, r3->r_reg, r1->r_reg, r2->r_reg);				
     push(I_REG, ty, r3, 0, s);
}

#define fbinop(op) gbinop(op, FLO, 1)
#define dbinop(op) gbinop(op, FLO, 2)

static mybool is_zero(ctvalue v) {
     return (v->v_op == I_CON && v->v_val == 0);
}

/* fcomp -- float or double comparison */
static void fcomp(operation op, int ty) {
     reg r1, r2, r3;

     r1 = move_to_reg(2, ty); 
     r2 = move_to_reg(1, ty); pop(2);				
     r3 = ralloc(INT); unlock(2);				
     vm_gen(op, r3->r_reg, r1->r_reg, r2->r_reg);	
     push(I_REG, INT, r3, 0, 1);						
}

/* compare -- integer comparison */
#define compare(op) compare1(op, op##f, op##d, op##q)

static void compare1(operation op, operation opf, operation opd,
                     operation op64) {
     if (is_zero(peek(1))) {
          switch (peek(2)->v_op) {
          case I_FCMPL:
          case I_FCMPG:
               pop(2); fcomp(opf, FLO); return;
          case I_DCMPL:
          case I_DCMPG:
               pop(2); fcomp(opd, FLO); return;
          case I_QCMP:
               pop(2); ibinop(op64); return;
          }
     }

     ibinop(op);
}

static void icondj(operation op, int lab) {
     flush(2); 
     ctvalue v = move_to_rc(1);
     reg r1 = move_to_reg(2, INT); 
     pop(2); unlock(2);	
     if (v->v_op == I_CON)					
          vm_gen(op, r1->r_reg, v->v_val, target(lab));
     else						
          vm_gen(op, r1->r_reg, v->v_reg->r_reg, target(lab));
}

/* fcondj -- float or double conditional jump */
static void fcondj(operation op, int lab) {
     flush(2); 
     reg r1 = move_to_reg(2, FLO);
     reg r2 = move_to_reg(1, FLO); 
     pop(2); unlock(2);		
     vm_gen(op, r1->r_reg, r2->r_reg, target(lab));
}

typedef struct jmptab {
     operation op;              /* Integer */
     operation oplf, opgf;      /* Float */
     operation opld, opgd;      /* Double */
     operation opq;             /* Longint */
} *jtable;

#define jtab(op, opl, opg) \
     { op, opl##f, opg##f, opl##d, opg##d, op##q }

struct jmptab tab_jeq = jtab(BEQ, BEQ, BEQ);
struct jmptab tab_jlt = jtab(BLT, BNGE, BLT);
struct jmptab tab_jgt = jtab(BGT, BGT, BNLT);
struct jmptab tab_jle = jtab(BLE, BNGT, BLE);
struct jmptab tab_jge = jtab(BGE, BGE, BNLT);
struct jmptab tab_jne = jtab(BNE, BNE, BNE);

static void condj(jtable t, int lab) {
     if (is_zero(peek(1))) {
          switch (peek(2)->v_op) {
          case I_FCMPL:
               pop(2); fcondj(t->oplf, lab); return;
          case I_FCMPG:
               pop(2); fcondj(t->opgf, lab); return;
          case I_DCMPL:
               pop(2); fcondj(t->opld, lab); return;
          case I_DCMPG:
               pop(2); fcondj(t->opgd, lab); return;
          case I_QCMP:
               pop(2); icondj(t->opq, lab); return;
          }
     }

     icondj(t->op, lab);
}

static void callout(func op, int nargs) {
     reg r;
     flush_stack(0, nargs);
     killregs();
     r = ralloc(INT);
     get_sp(r);
     push_reg(r);
     gcall(op, 1);
}

/* proc_call -- procedure call */
static void proc_call(uchar *pc, int arg) {
     /* arg is the argument size in words, but we want the
	count of stack items */
     int nargs = count_args(arg);
     reg r1, r2;

     r1 = move_to_reg(1, INT); 
     pop(1); unlock(1); 
     rfreeze(r1);
     push(I_REG, INT, r1, 0, 1);	       /* CP */
     push(I_CON, INT, rZERO, stack_map(pc), 1); /* PC = stack map */
     push(I_REG, INT, rBP, 0, 1);	       /* BP */
     flush_stack(0, nargs+3);
     killregs();
     r2 = ralloc(INT); rthaw(r1);
     vm_gen(LDW, r1->r_reg, r1->r_reg, 4*CP_PRIM);
     get_sp(r2);
     push_reg(r2);
     gcallr(r1, 1);
     pop(nargs+3);
}


/* instr -- translate one bytecode instruction */
static void instr(uchar *pc, int i, int arg1, int arg2) {
     reg r1, r2, r3;
     ctvalue v, v2;
     vmlabel lab;
     code_addr a;

     switch (i) {
     case I_PUSH:
          push(I_CON, INT, rZERO, arg1, 1);
          break;

     case I_LOCAL:
          if (arg1 == 0)
               push(I_REG, INT, rBP, 0, 1);
          else
               push(I_ADDR, INT, rBP, arg1, 1); 
          break;

     case I_LDKW:	
	  push(I_LDKW, INT, rZERO, address(&konst(arg1)), 1); 
	  break;
     case I_LDKQ:
	  push(I_LDKQ, INT, rZERO, address(&konst(arg1)), 2); 
	  break;
     case I_LDKF:
	  push(I_LDKF, FLO, rZERO, address(&konst(arg1)), 1); 
	  break;
     case I_LDKD:	
	  push(I_LDKD, FLO, rZERO, address(&konst(arg1)), 2); 
	  break;

     case I_LOADS:	deref(I_LOADS, INT, 1); break;
     case I_LOADC:	deref(I_LOADC, INT, 1); break;
     case I_LOADW: 	deref(I_LOADW, INT, 1); break;
     case I_LOADF:	deref(I_LOADF, FLO, 1); break;
     case I_LOADD:	deref(I_LOADD, FLO, 2); break;

     case I_LOADQ:
	  /* LOADQ is dangerous, because it can't be flushed without
	     allocating at least one register.  So we're cautious about
	     letting it remain unevaluated on the stack if it uses an 
	     index register. */
	  deref(I_LOADQ, INT, 2); 
#ifndef M64X32
	  v = peek(1);
	  if (member(v->v_reg, INT) && n_reserved() > 3) 
	       move_to_frame(1); 
#endif
	  break;

     case I_STOREW:	store(I_LOADW, INT, 1); break;
     case I_STOREF:	store(I_LOADF, FLO, 1); break;
     case I_STORES:	store(I_LOADS, INT, 1); break;
     case I_STOREC:	store(I_LOADC, INT, 1); break;
     case I_STORED:	store(I_LOADD, FLO, 2); break;
     case I_STOREQ:	store(I_LOADQ, INT, 2); break;

     case I_DUP:
	  v = move_from_frame(arg1+1);
	  push(v->v_op, v->v_type, v->v_reg, v->v_val, v->v_size);
	  break;
	  
     case I_SWAP:
	  v = move_from_frame(2); v2 = move_from_frame(1);
	  { struct _ctvalue tmp = *v; *v = *v2; *v2 = tmp; }
	  break;

     case I_POP:
	  pop(arg1); break;

     case I_PLUS:	ibinop(ADD); break;
     case I_MINUS: 	ibinop(SUB); break;
     case I_TIMES:	ibinop(MUL); break;
     case I_DIV:	callout(INT_DIV, 2); pop(1); break;
     case I_MOD:	callout(INT_MOD, 2); pop(1); break; 
     case I_AND: case I_BITAND:
			ibinop(AND); break;
     case I_OR: case I_BITOR:	
	  		ibinop(OR); break;
     case I_BITXOR:	ibinop(XOR); break;
     case I_ASR:	ibinop(RSH); break;
     case I_LSR:	ibinop(RSHu); break;
     case I_ROR:	ibinop(ROR); break;
     case I_PLUSA:	plusa(); break;

     case I_LSL:        
	  v = peek(2); v2 = peek(1);
	  if (v->v_op == I_CON && v2->v_op == I_CON) {
	       pop(2); push(I_CON, INT, rZERO, v->v_val << v2->v_val, 1);
	  } else {
	       ibinop(LSH); 
	  }
	  break;

     case I_UMINUS:	imonop(NEG); break;
     case I_BITNOT:	imonop(NOT); break;

     case I_CONVNF:	gmonop(CONVif, INT, FLO, 1); break;
     case I_CONVND:	gmonop(CONVid, INT, FLO, 2); break;
     case I_CONVFN:	gmonop(CONVfi, FLO, INT, 1); break;
     case I_CONVDN:	gmonop(CONVdi, FLO, INT, 1); break;
     case I_CONVDF:	gmonop(CONVdf, FLO, FLO, 1); break;
     case I_CONVFD:	gmonop(CONVfd, FLO, FLO, 2); break;
     case I_CONVNS:	gmonop(CONVis, INT, INT, 1); break;

     case I_CONVNC:
          push(I_CON, INT, rZERO, 0xff, 1);
          ibinop(AND);
          break;
  
     case I_NOT:	
	  push(I_CON, INT, rZERO, 1, 1); 
	  ibinop(XOR);
	  break;

     case I_EQ:		compare(EQ); break;
     case I_LT:		compare(LT); break;
     case I_GT:		compare(GT); break;
     case I_LE:		compare(LE); break;
     case I_GE:		compare(GE); break;
     case I_NE:		compare(NE); break;

     case I_JEQ:	condj(&tab_jeq, arg1); break;
     case I_JLT:	condj(&tab_jlt, arg1); break;
     case I_JGT:	condj(&tab_jgt, arg1); break;
     case I_JLE:	condj(&tab_jle, arg1); break;
     case I_JGE:	condj(&tab_jge, arg1); break;
     case I_JNE:	condj(&tab_jne, arg1); break;
	  
     case I_JUMP:	
	  flush(0); 
	  vm_gen(JUMP, target(arg1)); 
	  break;

     case I_JCASE:
	  lab = vm_newlab();
	  a = vm_jumptable(arg1);

	  flush(1);
	  r1 = move_to_reg(1, INT); pop(1);
          r2 = ralloc_suggest(INT, r1); unlock(1);
	  vm_gen(BGEu, r1->r_reg, arg1, lab);
          vm_gen(LSH, r2->r_reg, r1->r_reg, 2);
          vm_gen(LDW, r2->r_reg, r2->r_reg, address(a));
	  vm_gen(JUMP, r2->r_reg);
	  vm_label(lab);

	  pc += 2;
	  for (int j = 0; j < arg1; j++) {
	       case_label(get2(pc)+(pc-pcbase));
	       pc += 2;
	  }
	  break;

     case I_JRANGE:
	  lab = vm_newlab();
	  r1 = move_to_reg(3, INT); 
	  v = fix_const(2, FALSE); 
	  v2 = fix_const(1, FALSE); 
	  pop(3); unlock(3); killregs();
	  vm_gen(BLT, r1->r_reg, v->v_val, lab);
	  vm_gen(BLE, r1->r_reg, v2->v_val, target(arg1));
	  vm_label(lab);
	  break;

     case I_TESTGE:
	  flush(1); 
	  r1 = move_to_reg(2, INT);
	  v = fix_const(1, FALSE);
	  pop(2); unlock(2); killregs();
	  push(I_STACKW, INT, rZERO, 0, 1);
	  vm_gen(BGE, r1->r_reg, v->v_val, target(arg1));
	  break;

     case I_FPLUS:	fbinop(ADDf); break;
     case I_FMINUS:	fbinop(SUBf); break;
     case I_FTIMES:	fbinop(MULf); break;
     case I_FDIV:	fbinop(DIVf); break;
     case I_FUMINUS:    fmonop(NEGf); break;
	  
     case I_DPLUS:	dbinop(ADDd); break;
     case I_DMINUS:	dbinop(SUBd); break;
     case I_DTIMES:	dbinop(MULd); break;
     case I_DDIV:	dbinop(DIVd); break;
     case I_DUMINUS:	dmonop(NEGd); break;

	  /* [FD]CMP[LG] must be followed by an integer 
	     comparison, so we just set a flag and generate 
	     the appropriate comparison instruction later */

     case I_FCMPL:
     case I_FCMPG:
     case I_DCMPL:	
     case I_DCMPG:	
          push(i, INT, rZERO, 0, 0); break;

     case I_BOUND:
          v = peek(2);
          if (v->v_op == I_CON && peek(1)->v_op != I_CON) {
               // Handle a[2] when a has dynamic bound
               r1 = move_to_reg(1, INT);
               pop(1); unlock(1);
               vm_gen(BLEu, r1->r_reg, v->v_val,
                      handler(E_BOUND, arg1));
               break;
          }

	  r1 = move_to_reg(2, INT); 
	  v = move_to_rc(1); 
	  pop(2); unlock(2);
	  if (v->v_op == I_CON)
	       vm_gen(BGEu, r1->r_reg, v->v_val,
                      handler(E_BOUND, arg1));
	  else
	       vm_gen(BGEu, r1->r_reg, v->v_reg->r_reg,
                      handler(E_BOUND, arg1));
	  push(I_REG, INT, r1, 0, 1);
	  break;
     
     case I_NCHECK:
	  r1 = move_to_reg(1, INT); pop(1); unlock(1);
	  vm_gen(BEQ, r1->r_reg, 0, handler(E_NULL, arg1));
	  push(I_REG, INT, r1, 0, 1);
	  break;

     case I_ZCHECK:
	  r1 = move_to_reg(1, INT); pop(1); unlock(1);
	  vm_gen(BEQ, r1->r_reg, 0, handler(E_DIV, arg1));
	  push(I_REG, INT, r1, 0, 1);
	  break;

     case I_FZCHECK:
	  r1 = move_to_reg(1, FLO); r2 = ralloc(FLO); pop(1); unlock(1);
	  vm_gen(ZEROf, r2->r_reg);
	  vm_gen(BEQf, r1->r_reg, r2->r_reg, handler(E_FDIV, arg1));
	  push(I_REG, FLO, r1, 0, 1);
	  break;

     case I_DZCHECK:
	  r1 = move_to_reg(1, FLO); r2 = ralloc(FLO); pop(1); unlock(1);
	  vm_gen(ZEROd, r2->r_reg);
	  vm_gen(BEQd, r1->r_reg, r2->r_reg, handler(E_FDIV, arg1));
	  push(I_REG, FLO, r1, 0, 2);
	  break;

     case I_GCHECK:
	  r1 = move_to_reg(1, INT); pop(1); unlock(1);
	  vm_gen(BNE, r1->r_reg, 0, handler(E_GLOB, arg1));
	  break;

     case I_ERROR:
	  vm_gen(JUMP, handler(arg1, arg2));
	  break;

     case I_ALIGNC:
     case I_ALIGNS:
	  break;

     case I_FIXCOPY:
	  flush_stack(3, 3); 
	  r1 = move_to_reg(3, INT); 
	  r2 = move_to_reg(2, INT); 
	  move_to_rc(1); 
          push_reg(r2);
          push_reg(r1);
          gcall(MEMCPY, 3);
	  pop(2); killregs();
	  break;
	  
     case I_FLEXCOPY:
	  flush_stack(2, 2); 
	  r1 = move_to_reg(2, INT); 
	  r2 = move_to_reg(1, INT); 
	  r3 = ralloc(INT); 
	  pop(2); unlock(2); 
	  flex_space(r2);
	  vm_gen(BLTu, rSP->r_reg, address(stack + SLIMIT), stack_oflo);
          vm_gen(LDW, r3->r_reg, r1->r_reg, 0);
          vm_gen(STW, rSP->r_reg, r1->r_reg, 0);
          push_reg(r2);
          push_reg(r3);
          push_reg(rSP);
	  gcall(MEMCPY, 3);
          killregs();
          break;

     case I_LINK:
	  push(I_CON, INT, rZERO, address(&statlink), 1);
	  store(I_LOADW, INT, 1);
	  break;

     case I_SAVELINK:
	  push(I_LOADW, INT, rZERO, address(&statlink), 1);
	  push(I_ADDR, INT, rBP, 4*SL, 1);
	  store(I_LOADW, INT, 1);
	  break;

     case I_JPROC:
	  /* Let the SLIDEs instruction do the work */
	  break;

     case I_SLIDE:
	  proc_call(pc, arg1);
	  break;

     case I_SLIDEW:
	  proc_call(pc, arg1);
	  push(I_LOADW, INT, rZERO, address(&ob_res), 1);
	  break;

     case I_SLIDEQ:
	  proc_call(pc, arg1);
	  push(I_LOADQ, INT, rZERO, address(&ob_res), 2);
	  break;

     case I_SLIDEF:
	  proc_call(pc, arg1);
	  push(I_LOADF, FLO, rZERO, address(&ob_res), 1);
	  break;

     case I_SLIDED:
	  proc_call(pc, arg1);
	  push(I_LOADD, FLO, rZERO, address(&ob_res), 2);
	  break;

     case I_RESULTF:
	  push(I_CON, INT, rZERO, address(&ob_res), 1);
	  store(I_LOADF, FLO, 1);
	  break;

     case I_RESULTD:
	  push(I_CON, INT, rZERO, address(&ob_res), 1);
	  store(I_LOADD, FLO, 2);
	  break;

     case I_RESULTW:
	  push(I_CON, INT, rZERO, address(&ob_res), 1);
	  store(I_LOADW, INT, 1);
	  break;

     case I_RETURN:
          /* Elide the jump at end of procedure */
          if (pc+1 < pclimit)
               vm_gen(JUMP, retlab);
	  break;

     case I_LNUM:
	  break;

     case I_RESULTQ:
#ifndef M64X32
	  move_longval(peek(1), rZERO, address(&ob_res));
	  pop(1);
#else
          push(I_CON, INT, rZERO, address(&ob_res), 1);
          store(I_LOADQ, INT, 2);
#endif
	  break;

#ifndef M64X32
     case I_QPLUS:      callout(LONG_ADD, 2); pop(1); break;
     case I_QMINUS:	callout(LONG_SUB, 2); pop(1); break;
     case I_QTIMES:	callout(LONG_MUL, 2); pop(1); break;
     case I_QUMINUS:    callout(LONG_NEG, 1); break;
#else
     case I_QPLUS:      qbinop(ADDq); break;
     case I_QMINUS:	qbinop(SUBq); break;
     case I_QTIMES:	qbinop(MULq); break;
     case I_QUMINUS:	qmonop(NEGq); break;
#endif
          
     case I_QDIV:	callout(LONG_DIV, 2); pop(1); break;
     case I_QMOD:	callout(LONG_MOD, 2); pop(1); break;

     case I_QCMP:
#ifndef M64X32
	  callout(LONG_CMP, 2); pop(2);
	  push(I_STACKW, INT, rZERO, 0, 1);
#else
          push(I_QCMP, INT, rZERO, 0, 0);
#endif          
	  break;

     case I_CONVNQ:
	  v = peek(1);
	  if (v->v_op == I_CON) {
	       pop(1);
	       push(I_CON, INT, rZERO, v->v_val, 2);
	  } else {
#ifndef M64X32
	       callout(LONG_EXT, 1); pop(1);
	       push(I_STACKQ, INT, rZERO, 0, 2);
#else
               r1 = move_to_reg(1, INT); pop(1);
               vm_gen(SXTq, r1->r_reg, r1->r_reg);
               push(I_REG, INT, r1, 0, 2);
#endif
	  }
	  break;

     case I_CONVQN:
#ifndef M64X32
          move_to_frame(1);
          r1 = ralloc(INT);
          ldst_item(LDW, r1, 1);
          pop(1);
          push(I_REG, INT, r1, 0, 1);
#else
          r1 = move_to_reg(1, INT); pop(1);
          vm_gen(MOV, r1->r_reg, r1->r_reg);
          push(I_REG, INT, r1, 0, 1);
#endif
          break;

     case I_CONVQD:
	  callout(LONG_FLO, 1); pop(1);
	  push(I_STACKQ, FLO, rZERO, 0, 2);
	  break;

     case I_QZCHECK:
#ifndef M64X32
          push_reg(rBP);
          push_con(arg1);
          callout(LONG_ZCHECK, 3);
          pop(2);
#else
          r1 = move_to_reg(1, INT); pop(1); unlock(1);
          vm_gen(BEQq, r1->r_reg, 0, handler(E_DIV, arg1));
          push(I_REG, INT, r1, 0, 2);
#endif
	  break;

     default:
	  panic("*instruction %s is not implemented", instrs[i].i_name);
     }

#ifdef DEBUG
     if (dflag >= 4) dumpregs();
#endif
}

/* tran_instr -- expand and translate a bytecode instruction */
static void tran_instr(uchar *pc, int inst, int arg1, int arg2, int lev) {
     int *equiv = instrs[inst].i_equiv;

     if (equiv[0] == 0)
	  instr(pc, inst, arg1, arg2);
     else {
	  for (int i = 0; equiv[i] != 0; i++) {
	       int e = equiv[i];
	       int arg = 0;

	       if (e&IARG)
		    arg = arg1;
	       else if (e&ICON)
		    arg = equiv[++i];

#ifdef DEBUG
	       if (dflag >= 1) {
		    printf("%.*s %s", lev, "++++++++++", 
                           instrs[e&IMASK].i_name);
                    if (e&(IARG|ICON)) printf(" %d", arg);
                    printf("\n"); 
	       }
#endif
	       
	       tran_instr(pc, e&IMASK, arg, 0, lev+1);
	  }
     }
}

/* map_labels -- determine branch targets in a bytecode routine */
static void map_labels(void) {
     /* We need to identify branch targets before translating,
	because cached values must be flushed at each target,
	even the target of a backwards branch */

     for (uchar *pc = pcbase; pc < pclimit; ) {
	  int op = *pc, n;
	  uchar *pc1 = pc+1;
	  struct _decode *d = &decode[op];

	  for (const char *s = d->d_patt; *s != '\0'; s++) {
	       switch (*s) {
	       case '1': 
	       case 'K':
		    pc1++; break;
	       case '2':
	       case 'L': 
		    pc1 += 2; break;
	       case 'R':
		    mark_label(get2(pc1)+(pc-pcbase)); pc1 += 2; break;
	       case 'S':
		    mark_label(get1(pc1)+(pc-pcbase)); pc1 += 1; break;
	       case 'N':
		    break;
	       default:
		    panic("*bad pattern char %c for %s", *s, 
			  instrs[d->d_inst].i_name);
	       }
	  }

	  switch (op) {
	  case K_JCASE_1:
	       n = pc[1]; pc += 2;
	       for (int i = 0; i < n; i++) {
		    mark_label(get2(pc)+(pc-pcbase));
		    pc += 2;
	       }
	       break;
	  default:	       
	       pc += d->d_len;
	  }
     }
}

/* translate -- decode a bytecode routine and translate the instructions */
static void translate(void) {
     for (uchar *pc = pcbase; pc < pclimit; ) {
	  int op = *pc;
	  uchar *pc1 = pc+1;
	  struct _decode *d = &decode[op];
	  int args[2];
	  int nargs = 0;
	  args[0] = 0; args[1] = 0;

	  for (const char *s = d->d_patt; *s != '\0'; s++) {
	       switch (*s) {
	       case '1': 
	       case 'K':
		    args[nargs++] = get1(pc1); pc1++; break;
	       case '2':
	       case 'L':
		    args[nargs++] = get2(pc1); pc1 += 2; break;
	       case 'R':
		    args[nargs++] = get2(pc1)+(pc-pcbase); pc1 += 2; break;
	       case 'S':
		    args[nargs++] = get1(pc1)+(pc-pcbase); pc1 += 1; break;
	       case 'N':
		    args[nargs++] = d->d_arg; break;
	       default:
		    panic("*bad pattern char %c", *s);
	       }
	  }

#ifdef DEBUG
	  if (dflag >= 1) {
	       printf("%ld: %s", (long) (pc-pcbase), instrs[d->d_inst].i_name);
	       for (int i = 0; i < nargs; i++) printf(" %d", args[i]);
	       printf("\n");
	  }
#endif

          codepoint lab = find_label(pc-pcbase);
	  if (lab != NULL) {
	       killregs();

	       if (lab->l_depth >= 0) {
		    /* Forward branch: restore stack state */
		    flush(0);
		    restore_stack(lab);
	       }

	       label(lab);
	  }

	  tran_instr(pc, d->d_inst, args[0], args[1], 1);

	  switch (op) {
	  case K_JCASE_1:
	       pc += 2*args[0] + 2;
	       break;
	  default:
	       pc += d->d_len;
	  }
     }

     vm_label(retlab);
     vm_gen(RET);
}

static void make_error(vmlabel lab, int code, int line) {
     vm_label(lab);
     push_reg(rBP);
     push_con(line);
     push_con(code);
     gcall(RTERROR, 3);
}

static int serial;              /* Serial number for anonymous procedures */

/* jit_compile -- replace a bytecode routine with native code */
void jit_compile(value *cp) {
     proc p = find_proc(cp);
     const char *pname;
     static char name[16];

     if (p != NULL)
          pname = p->p_name;
     else {
          sprintf(name, "G_%d", ++serial);
          pname = name;
     }

#ifdef DEBUG
     if (dflag >= 1)
	  printf("JIT: %s\n", (p != NULL ? p->p_name : "???"));
#endif

     context = cp; 
     int frame = context[CP_FRAME].i;
     pcbase = pointer(context[CP_CODE]);
     pclimit = pcbase + context[CP_SIZE].i;
     int map = context[CP_MAP].i;

     init_regs();
     init_labels();
     init_stack(frame);
     stack_oflo = vm_newlab();
     retlab = vm_newlab();

     map_labels();
     code_addr entry = prolog(pname, frame, map);
     translate();
     do_errors(make_error);
     vm_end();
     cp[CP_PRIM].a = wrap_prim((primitive *) entry);

#ifdef DEBUG
     if (dflag > 0) fflush(stdout);
#endif
}

/* jit_trap -- translate procedure on first call */
void jit_trap(value *bp) {
     value *cp = valptr(bp[CP]);
     jit_compile(cp);
     primcall(cp, bp);
}

/* jit_proc -- translate a specified procedure */
void jit_proc(value *bp) {
     value *p = valptr(bp[HEAD]);
     jit_compile(p);
}

/* jit_debug -- enable vm debugging */
void jit_debug(value *bp) {
     vm_debug = dflag = bp[HEAD].i;
}

/* vm_alloc -- upcall from vm to allocate code buffer */
void *vm_alloc(int size) {
     return scratch_alloc(size);
}
