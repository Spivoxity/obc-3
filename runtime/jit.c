/*
 * jit.c
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
#include "keiko.h"
#include "jit.h"
#include "decode.h"
#include <stdarg.h>
#include <assert.h>

/* Translator */

static value *context;		/* CP for the current procedure */
static uchar *pcbase, *pclimit;	/* Code addresses */
static int frame;		/* Size of local variable frame */
static int cmpflag = 0;         /* Flag for FCMP or DCMP */

#define konst(i) context[CP_CONST+i]

static vmlabel stack_oflo, retlab;

/* prolog -- generate code for procedure prologue */
static code_addr prolog(const char *name) {
     int i;
     code_addr entry;
     vmlabel lab = vm_newlab();

     entry = vm_begin(name, 1);
     vm_gen2ri(GETARG, rBP->r_reg, 0);

     /* Check for stack overflow */
     vm_gen3rij(BGEQU, rBP->r_reg, (unsigned) stack + SLIMIT + frame, lab);
     vm_label(stack_oflo);
     gcall(stkoflo, 1, rBP);
     vm_label(lab);

     if (frame > 24) {
	  vm_gen3rri(SUB, rI0->r_reg, rBP->r_reg, frame);
	  vm_gen2ri(MOV, rI1->r_reg, 0);
	  vm_gen2ri(MOV, rI2->r_reg, frame);
	  gcall(memset, 3, rI0, rI1, rI2);
     } else if (frame > 0) {
	  vm_gen2ri(MOV, rI0->r_reg, 0);
	  for (i = 4; i <= frame; i += 4)
	       vm_gen3rri(STW, rI0->r_reg, rBP->r_reg, -i);
     }

     return entry;
}

/* stack_map -- find pointer map for eval stack at procedure call */
static int stack_map(uchar *pc) {
     value *r = context[CP_STKMAP].p;
     if (r == NULL) return 0;
     while (r[0].x != NULL) {
	  if (r[0].x == pc) return r[1].i;
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
     vm_gen2rr(op, r2->r_reg, r1->r_reg);
     push(I_REG, rclass2, r2, 0, s);
}

#define imonop(op) gmonop(op, INT, INT, 1)
#define fmonop(op) gmonop(op, FLO, FLO, 1)
#define dmonop(op) gmonop(op, FLO, FLO, 2)

/* ibinop -- binary integer operation */
static void ibinop(operation op) {
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
	  vm_gen3rri(op, r2->r_reg, r1->r_reg, v->v_val);
     else								
	  vm_gen3rrr(op, r2->r_reg, r1->r_reg, v->v_reg->r_reg);
     push(I_REG, INT, r2, 0, 1);
}

/* fdbinop -- float or double binary operation */
static void fdbinop(operation op, int s) {
     reg r1, r2, r3;

     r1 = move_to_reg(2, FLO); r2 = move_to_reg(1, FLO); pop(2);
     r3 = ralloc_suggest(FLO, r1); unlock(2);				
     vm_gen3rrr(op, r3->r_reg, r1->r_reg, r2->r_reg);				
     push(I_REG, FLO, r3, 0, s);
}

#define fbinop(op) fdbinop(op, 1)
#define dbinop(op) fdbinop(op, 2)

/* fcomp -- float or double comparison */
static void fcomp(operation op) {
     reg r1, r2, r3;
     ctvalue v;

     v = peek(1);
     assert(v->v_op == I_CON && v->v_val == 0);	
     r1 = move_to_reg(3, FLO); 
     r2 = move_to_reg(2, FLO); pop(3);				
     r3 = ralloc(INT); unlock(3);				
     vm_gen3rrr(op, r3->r_reg, r1->r_reg, r2->r_reg);	
     push(I_REG, INT, r3, 0, 1);						
}

/* icomp -- integer comparison */
#define icomp(op) icomp1(op, op##F, op##D)

static void icomp1(operation op, operation opf, operation opd) {
     switch (cmpflag) {
     case I_FCMP:
	  fcomp(opf); break;
     case I_DCMP:
          fcomp(opd); break;
     default:
	  ibinop(op); break;
     }

     cmpflag = 0;
}

/* fcondj -- float or double conditional jump */
static void fcondj(operation op, int lab) {
     reg r1, r2;
     ctvalue v;

     v = peek(1);
     assert(v->v_op == I_CON && v->v_val == 0);	
     flush(3); 
     r1 = move_to_reg(3, FLO); r2 = move_to_reg(2, FLO); 
     pop(3); unlock(3);		
     vm_gen3rrj(op, r1->r_reg, r2->r_reg, target(lab));
}

/* icondj -- integer conditional jump */
#define icondj(op, lab) icondj1(op, op##F, op##D, lab)

static void icondj1(operation op, operation opf, operation opd, int lab) {
     reg r1; ctvalue v;

     switch (cmpflag) {
     case I_FCMP:
	  fcondj(opf, lab); break;
     case I_DCMP:
          fcondj(opd, lab); break;
     default:
	  flush(2); 
	  v = move_to_rc(1);
          r1 = move_to_reg(2, INT); 
          pop(2); unlock(2);	
          if (v->v_op == I_CON)					
               vm_gen3rij(op, r1->r_reg, v->v_val, target(lab));
          else						
               vm_gen3rrj(op, r1->r_reg, v->v_reg->r_reg, target(lab));
     }

     cmpflag = 0;
}

static void callout(void (*op)(value *sp), int nargs) {
     reg r;
     flush_stack(0, nargs);
     killregs();
     r = ralloc(INT);
     get_sp(r);
     gcall(op, 1, r);
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
     vm_gen3rri(LDW, r1->r_reg, r1->r_reg, 0);
     get_sp(r2);
     gcallr(r1, 1, r2);
     pop(nargs+3);
}


/* instr -- translate one bytecode instruction */
static void instr(uchar *pc, int i, int arg1, int arg2) {
     reg r1, r2, r3;
     ctvalue v, v2;
     vmlabel lab;
     code_addr a;
     int j;

     switch (i) {
     case I_PUSH:	push(I_CON, INT, rZERO, arg1, 1); break;

     case I_LOCAL:
          if (arg1 == 0)
               push(I_REG, INT, rBP, 0, 1);
          else
               push(I_ADDR, INT, rBP, arg1, 1); 
          break;

     case I_LDKW:	
	  push(I_LDKW, INT, rZERO, (unsigned) &konst(arg1), 1); 
	  break;
     case I_LDKQ:
	  push(I_LDKQ, INT, rZERO, (unsigned) &konst(arg1), 2); 
	  break;
     case I_LDKF:
	  push(I_LDKF, FLO, rZERO, (unsigned) &konst(arg1), 1); 
	  break;
     case I_LDKD:	
	  push(I_LDKD, FLO, rZERO, (unsigned) &konst(arg1), 2); 
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
	  v = peek(1);
	  if (member(v->v_reg, INT) && n_reserved() > 3) 
	       move_to_frame(1); 
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
     case I_DIV:	callout(int_div, 2); pop(1); break;
     case I_MOD:	callout(int_mod, 2); pop(1); break; 
     case I_AND: case I_BITAND:
			ibinop(AND); break;
     case I_OR: case I_BITOR:	
	  		ibinop(OR); break;
     case I_BITXOR:	ibinop(XOR); break;
     case I_ASR:	ibinop(RSH); break;
     case I_LSR:	ibinop(RSHU); break;
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

     case I_CONVNF:	gmonop(CONVIF, INT, FLO, 1); break;
     case I_CONVND:	gmonop(CONVID, INT, FLO, 2); break;
     case I_CONVDF:	gmonop(CONVDF, FLO, FLO, 1); break;
     case I_CONVFD:	gmonop(CONVFD, FLO, FLO, 2); break;
     case I_CONVNC:	gmonop(CONVIC, INT, INT, 1); break;
     case I_CONVNS:	gmonop(CONVIS, INT, INT, 1); break;
  
     case I_NOT:	
	  push(I_CON, INT, rZERO, 1, 1); 
	  ibinop(XOR);
	  break;

     case I_EQ:		icomp(EQ); break;
     case I_LT:		icomp(LT); break;
     case I_GT:		icomp(GT); break;
     case I_LEQ:	icomp(LEQ); break;
     case I_GEQ:	icomp(GEQ); break;
     case I_NEQ:	icomp(NEQ); break;

     case I_JEQ:	icondj(BEQ, arg1); break;
     case I_JLT:	icondj(BLT, arg1); break;
     case I_JGT:	icondj(BGT, arg1); break;
     case I_JLEQ:	icondj(BLEQ, arg1); break;
     case I_JGEQ:	icondj(BGEQ, arg1); break;
     case I_JNEQ:	icondj(BNEQ, arg1); break;
	  
     case I_JUMP:	
	  flush(0); 
	  vm_gen1j(JUMP, target(arg1)); 
	  break;

     case I_JCASE:
	  lab = vm_newlab();
	  a = vm_jumptable(arg1);

	  flush(1);
	  r1 = move_to_reg(1, INT); pop(1); 
	  r2 = ralloc_suggest(INT, r1); unlock(1);
	  vm_gen3rij(BGEQU, r1->r_reg, arg1, lab);
          vm_gen3rri(LSH, r2->r_reg, r1->r_reg, 2);
	  vm_gen3rri(LDW, r2->r_reg, r2->r_reg, (unsigned) a);
	  vm_gen1r(JUMP, r2->r_reg);
	  vm_label(lab);

	  pc += 2;
	  for (j = 0; j < arg1; j++) {
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
	  vm_gen3rij(BLT, r1->r_reg, v->v_val, lab);
	  vm_gen3rij(BLEQ, r1->r_reg, v2->v_val, target(arg1));
	  vm_label(lab);
	  break;

     case I_TESTGEQ:
	  flush(1); 
	  r1 = move_to_reg(2, INT);
	  v = fix_const(1, FALSE);
	  pop(2); unlock(2); killregs();
	  push(I_STACKW, INT, rZERO, 0, 1);
	  vm_gen3rij(BGEQ, r1->r_reg, v->v_val, target(arg1));
	  break;

     case I_FPLUS:	fbinop(ADDF); break;
     case I_FMINUS:	fbinop(SUBF); break;
     case I_FTIMES:	fbinop(MULF); break;
     case I_FDIV:	fbinop(DIVF); break;
     case I_FUMINUS:    fmonop(NEGF); break;
	  
     case I_DPLUS:	dbinop(ADDD); break;
     case I_DMINUS:	dbinop(SUBD); break;
     case I_DTIMES:	dbinop(MULD); break;
     case I_DDIV:	dbinop(DIVD); break;
     case I_DUMINUS:	dmonop(NEGD); break;

	  /* FCMP or DCMP must be followed by an integer 
	     comparison, so we just set a flag and generate 
	     the appropriate comparison instruction later */

     case I_FCMP: case I_DCMP:	
          cmpflag = i; break;

     case I_BOUND:
	  r1 = move_to_reg(2, INT); 
	  v = move_to_rc(1); 
	  pop(2); unlock(2);
	  if (v->v_op == I_CON)
	       vm_gen3rij(BGEQU, r1->r_reg, v->v_val, handler(E_BOUND, arg1));
	  else
	       vm_gen3rrj(BGEQU, r1->r_reg, v->v_reg->r_reg, 
                          handler(E_BOUND, arg1));
	  push(I_REG, INT, r1, 0, 1);
	  break;
     
     case I_NCHECK:
	  r1 = move_to_reg(1, INT); pop(1); unlock(1);
	  vm_gen3rij(BEQ, r1->r_reg, 0, handler(E_NULL, arg1));
	  push(I_REG, INT, r1, 0, 1);
	  break;

     case I_ZCHECK:
	  r1 = move_to_reg(1, INT); pop(1); unlock(1);
	  vm_gen3rij(BEQ, r1->r_reg, 0, handler(E_DIV, arg1));
	  push(I_REG, INT, r1, 0, 1);
	  break;

     case I_FZCHECK:
	  r1 = move_to_reg(1, FLO); r2 = ralloc(FLO); pop(1); unlock(1);
	  vm_gen1r(ZEROF, r2->r_reg);
	  vm_gen3rrj(BEQF, r1->r_reg, r2->r_reg, handler(E_FDIV, arg1));
	  push(I_REG, FLO, r1, 0, 1);
	  break;

     case I_DZCHECK:
	  r1 = move_to_reg(1, FLO); r2 = ralloc(FLO); pop(1); unlock(1);
	  vm_gen1r(ZEROD, r2->r_reg);
	  vm_gen3rrj(BEQD, r1->r_reg, r2->r_reg, handler(E_FDIV, arg1));
	  push(I_REG, FLO, r1, 0, 2);
	  break;

     case I_GCHECK:
	  r1 = move_to_reg(1, INT); pop(1); unlock(1);
	  vm_gen3rij(BNEQ, r1->r_reg, 0, handler(E_GLOB, arg1));
	  break;

     case I_ERROR:
	  vm_gen1j(JUMP, handler(arg1, arg2));
	  break;

     case I_ALIGNC:
     case I_ALIGNS:
	  break;

     case I_FIXCOPY:
	  flush_stack(3, 3); 
	  r1 = move_to_reg(3, INT); 
	  r2 = move_to_reg(2, INT); 
	  r3 = move_to_reg(1, INT); 
	  pop(3); killregs();
	  gcall(memcpy, 3, r1, r2, r3);
	  break;
	  
     case I_FLEXCOPY:
	  flush_stack(2, 2); 
	  r1 = move_to_reg(2, INT); 
	  r2 = move_to_reg(1, INT); 
	  r3 = ralloc(INT); 
	  pop(2); unlock(2); killregs();
	  flex_space(r2);
	  vm_gen3rij(BLTU, rSP->r_reg, (unsigned) stack + SLIMIT, stack_oflo);
          vm_gen3rri(LDW, r3->r_reg, r1->r_reg, 0);
          vm_gen3rri(STW, rSP->r_reg, r1->r_reg, 0);
	  gcall(memcpy, 3, rSP, r3, r2);
	  break;

     case I_LINK:
	  push(I_CON, INT, rZERO, (unsigned) &statlink, 1);
	  store(I_LOADW, INT, 1);
	  break;

     case I_SAVELINK:
	  push(I_LOADW, INT, rZERO, (unsigned) &statlink, 1);
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
	  push(I_LOADW, INT, rZERO, (unsigned) &ob_res, 1);
	  break;

     case I_SLIDEQ:
	  proc_call(pc, arg1);
	  push(I_LOADQ, INT, rZERO, (unsigned) &ob_dres, 2);
	  break;

     case I_SLIDEF:
	  proc_call(pc, arg1);
	  push(I_LOADF, FLO, rZERO, (unsigned) &ob_res, 1);
	  break;

     case I_SLIDED:
	  proc_call(pc, arg1);
	  push(I_LOADD, FLO, rZERO, (unsigned) &ob_dres, 2);
	  break;

     case I_RESULTF:
	  push(I_CON, INT, rZERO, (unsigned) &ob_res, 1);
	  store(I_LOADF, FLO, 1);
	  break;

     case I_RESULTD:
	  push(I_CON, INT, rZERO, (unsigned) &ob_dres, 1);
	  store(I_LOADD, FLO, 2);
	  break;

     case I_RESULTW:
	  push(I_CON, INT, rZERO, (unsigned) &ob_res, 1);
	  store(I_LOADW, INT, 1);
	  break;

     case I_RESULTQ:
	  move_longval(peek(1), rZERO, (unsigned) &ob_dres);
	  pop(1);
	  break;

     case I_RETURN:
          if (pc+1 < pclimit)
               vm_gen1j(JUMP, retlab);
	  break;

     case I_LNUM:
	  break;

#ifdef INT64
     case I_QPLUS:	callout(long_add, 2); pop(1); break;
     case I_QMINUS:	callout(long_sub, 2); pop(1); break;
     case I_QTIMES:	callout(long_mul, 2); pop(1); break;
     case I_QDIV:	callout(long_div, 2); pop(1); break;
     case I_QMOD:	callout(long_mod, 2); pop(1); break;
     case I_QUMINUS:    callout(long_neg, 1); break;

     case I_QCMP:
	  callout(long_cmp, 2); pop(2);
	  push(I_STACKW, INT, rZERO, 0, 1);
	  break;

     case I_CONVNQ:
	  v = peek(1);
	  if (v->v_op == I_CON) {
	       pop(1);
	       push(I_CON, INT, rZERO, v->v_val, 2);
	  } else {
	       callout(long_ext, 1); pop(1);
	       push(I_STACKD, INT, rZERO, 0, 2);
	  }
	  break;

     case I_CONVQN:
	  v = peek(1);
	  r1 = ralloc(INT);
	  get_halflong(v, 0, r1);
	  pop(1);
	  push(I_REG, INT, r1, 0, 1);
	  break;

     case I_CONVQD:
	  callout(long_flo, 1); pop(1);
	  push(I_STACKD, FLO, rZERO, 0, 2);
	  break;

     case I_QZCHECK:
	  lab = vm_newlab();
	  v = peek(1);
	  r1 = ralloc(INT);
	  get_halflong(v, 1, r1);
	  vm_gen3rij(BNEQ, r1->r_reg, 0, lab);
	  get_halflong(v, 0, r1);
	  vm_gen3rij(BEQ, r1->r_reg, 0, handler(E_DIV, arg1));
	  vm_label(lab);
	  break;
#endif

#ifdef SPECIALS
     /* Specials for Compilers course */

     case I_CASEJUMP:
	  flush(1);
	  r1 = move_to_reg(1, INT); pop(1); unlock(1);
	  pc += 2;
	  for (j = 0; j < arg1; j++) {
	       g3rib(BEQ, r1, get2(pc), to_label(get2(pc+2)+(pc-pcbase)));
	       pc += 4;
	  }
	  break;

     case I_PACK:
	  callout(pack_closure, 2); 
	  pop(1); 
	  break;

     case I_UNPACK:
	  callout(unpack_closure, 1); 
	  push(I_STACKW, INT, rZERO, 0, 1); 
	  break;
#endif

     default:
	  panic("instruction %s is not implemented", instrs[i].i_name);
     }

#ifdef DEBUG
     if (dflag > 2) dumpregs();
#endif
}

/* tran_instr -- expand and translate a bytecode instruction */
static void tran_instr(uchar *pc, int inst, int arg1, int arg2, int lev) {
     int *equiv = instrs[inst].i_equiv;
     int i;

     if (equiv[0] == 0)
	  instr(pc, inst, arg1, arg2);
     else {
	  for (i = 0; equiv[i] != 0; i++) {
	       int e = equiv[i];
	       int arg = 0;

	       if (e&IARG)
		    arg = arg1;
	       else if (e&ICON)
		    arg = equiv[++i];

#ifdef DEBUG
	       if (dflag > 0) {
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
     uchar *pc; 
     int i, n; 
     const char *s;

     /* We need to identify branch targets before translating,
	because cached values must be flushed at each target,
	even the target of a backwards branch */

     for (pc = pcbase; pc < pclimit; ) {
	  int op = *pc;
	  uchar *pc1 = pc+1;
	  struct _decode *d = &decode[op];

	  for (s = d->d_patt; *s != '\0'; s++) {
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
	       for (i = 0; i < n; i++) {
		    mark_label(get2(pc)+(pc-pcbase));
		    pc += 2;
	       }
	       break;
#ifdef SPECIALS
	  case K_CASEJUMP_1:
	       n = pc[1]; pc += 2;
	       for (i = 0; i < n; i++) {
		    mark_label(get2(pc+2)+(pc-pcbase));
		    pc += 4;
	       }
	       break;
#endif	  
	  default:	       
	       pc += d->d_len;
	  }
     }
}

/* translate -- decode a bytecode routine and translate the instructions */
static void translate(void) {
     uchar *pc; 
     const char *s;
     codepoint lab;

     for (pc = pcbase; pc < pclimit; ) {
	  int op = *pc;
	  uchar *pc1 = pc+1;
	  struct _decode *d = &decode[op];
	  int args[2];
	  int nargs = 0;
	  args[0] = 0; args[1] = 0;

	  for (s = d->d_patt; *s != '\0'; s++) {
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
	  if (dflag > 0) {
	       int i;
	       printf("%d: %s", pc-pcbase, instrs[d->d_inst].i_name);
	       for (i = 0; i < nargs; i++) printf(" %d", args[i]);
	       printf("\n");
	  }
#endif

	  lab = find_label(pc-pcbase);

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
#ifdef SPECIALS
	  case K_CASEJUMP_1:
	       pc += 4*args[0] + 2;
	       break;
#endif
	  default:
	       pc += d->d_len;
	  }
     }

     vm_label(retlab);
     vm_gen0(RET);
}

static void make_error(vmlabel lab, int code, int line) {
     vm_label(lab);
     vm_gen2ri(MOV, rI0->r_reg, code);
     vm_gen2ri(MOV, rI1->r_reg, line);
     gcall(rterror, 3, rI0, rI1, rBP);
}

static int serial;              /* Serial number for anonymous procedures */

/* jit_compile -- replace a bytecode routine with native code */
void jit_compile(value *cp) {
     proc p = find_proc(cp);
     code_addr entry;
     const char *pname;
     static char name[16];

     if (p != NULL)
          pname = p->p_name;
     else {
          sprintf(name, "G_%d", ++serial);
          pname = name;
     }

#ifdef DEBUG
     if (dflag > 0)
	  printf("JIT: %s\n", (p != NULL ? p->p_name : "???"));
#endif

     context = cp; 
     frame = context[CP_FRAME].i;
     pcbase = context[CP_CODE].x;
     pclimit = pcbase + context[CP_SIZE].i;

     init_regs();
     init_labels();
     init_stack(frame);
     cmpflag = 0;
     stack_oflo = vm_newlab();
     retlab = vm_newlab();

     map_labels();
     entry = prolog(pname);
     translate();
     do_errors(make_error);
     vm_end();
     cp[CP_PRIM].z = (primitive *) entry;

#ifdef DEBUG
     if (dflag > 0) fflush(stdout);
#endif
}

/* jit_trap -- translate procedure on first call */
void jit_trap(value *bp) {
     value *cp = bp[CP].p;
     jit_compile(cp);
     cp[CP_PRIM].z(bp);
}

/* jit_proc -- translate a specified procedure */
void jit_proc(value *bp) {
     value *p = bp[HEAD].p;
     jit_compile(p);
}

/* jit_debug -- enable vm debugging */
void jit_debug(value *bp) {
     vm_debug = dflag = bp[HEAD].i;
}

/* vm_alloc -- upcall from vm to allocate code buffer */
void *vm_alloc(int size) {
     return scratch_alloc(size, TRUE);
}
