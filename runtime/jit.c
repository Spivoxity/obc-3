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

/* Code generation interface */

/* gargs -- generate function arguments */
static void gargs(int n, va_list va) {
     int i;
     reg arg[6];

     for (i = 0; i < n; i++)
	  arg[i] = (reg) va_arg(va, int);

     vm_gen1i(PREP, n);
     for (i = n-1; i >= 0; i--)
	  vm_gen1r(ARG, regs[arg[i]].r_reg);
}

static void gcall1(const char *fname, int f, int n, ...) {
     va_list va;

#ifdef DEBUG
     if (dflag > 1) printf("\t\t\t\t\tCall %s\n", fname);
#endif

     va_start(va, n);
     gargs(n, va);
     va_end(va);
     vm_gen1i(CALL, f);
}

/* gcallr -- indirect function call */
static void gcallr(reg f, int n, ...) {
     va_list va;

     va_start(va, n);
     gargs(n, va);
     va_end(va);
     vm_gen1r(CALL, regs[f].r_reg);
}

void use_label(code_addr loc, codepoint lab) {
     if (lab == NULL) panic("undefined label");

     patch_label(loc, lab);
     if (lab->l_depth < 0)
	  save_stack(lab);
}
     

/* Translator */

static value *context;		/* CP for the current procedure */
static uchar *pcbase, *pclimit;	/* Code addresses */
static int frame;		/* Size of local variable frame */
static mybool cmpflag = FALSE;	/* Flag for FCMP or DCMP */

#define konst(i) context[CP_CONST+i]

static code_addr stack_oflo = NULL;

/* prolog -- generate code for procedure prologue */
static code_addr prolog(void) {
     int in, i;
     code_addr entry;

     if (stack_oflo == NULL) {
	  stack_oflo = vm_label();
	  g2ri(MOV, rI0, E_STACK);
	  g2ri(MOV, rI1, 0);
	  gcall(rterror, 3, rI0, rI1, rBP);
     }

     entry = vm_label();
     vm_prolog(1);
     in = vm_arg();
     g2ri(GETARG, rBP, in);

     /* Check for stack overflow */
     g3rib(BLTU, rBP, (unsigned) stack + SLIMIT + frame, 
	   to_addr(stack_oflo));

     if (frame > 24) {
	  g2ri(MOV, rI0, 0);
	  g2ri(MOV, rI1, frame);
	  g3rri(SUB, rI2, rBP, frame);
	  gcall(memset, 3, rI2, rI0, rI1);
     } else if (frame > 0) {
	  g2ri(MOV, rI0, 0);
	  for (i = 4; i <= frame; i += 4)
	       g3rri(STW, rI0, rBP, -i);
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
     g2rr(op, r2, r1);						
     push(I_REG, rclass2, r2, 0, s);
}

/* coerce -- widen or narrow float */
static void coerce(int s) {
     reg r;

     r = move_to_reg(1, FLO); pop(1); 
     push(I_REG, FLO, r, 0, s);
}

#define imonop(op) gmonop(op, INT, INT, 1)
#define fmonop(op) gmonop(op, FLO, FLO, 2)
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
	  g3rri(op, r2, r1, v->v_val);				
     else								
	  g3rrr(op, r2, r1, v->v_reg);				
     push(I_REG, INT, r2, 0, 1);
}

/* fdbinop -- float or double binary operation */
static void fdbinop(operation op, int s) {
     reg r1, r2, r3;

     r1 = move_to_reg(2, FLO); r2 = move_to_reg(1, FLO); pop(2);
     r3 = ralloc_suggest(FLO, r1); unlock(2);				
     g3rrr(op, r3, r1, r2);					
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
     g3rrr(op, r3, r1, r2);					
     push(I_REG, INT, r3, 0, 1);						
}

/* icomp -- integer comparison */
#define icomp(op) icomp1(op, op##F)

static void icomp1(operation op, operation opf) {
     if (cmpflag)
	  fcomp(opf);
     else
	  ibinop(op);

     cmpflag = FALSE;
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
     g3rrb(op, r1, r2, to_label(lab));
}

/* typejump -- jump on type test */
static void typejump(int op, ctvalue v, int lab) {
     int d; reg r1, r2; codepoint lab2;

     d = v->v_op >> 16;
     r1 = v->v_reg;
     r2 = ralloc_avoid(INT, r1);
     pop(1); 

     switch (op) {
     case BEQ:
	  /* if r.depth < d or r.ancestor[n] != v->v_val goto lab */
	  g3rri(LDW, r2, r1, 4*DESC_DEPTH);
	  g3rib(BLT, r2, d, to_label(lab));
	  g3rri(LDW, r2, r1, 4*DESC_ANCES);
	  g3rri(LDW, r2, r2, 4*d);
	  g3rib(BNEQ, r2, v->v_val, to_label(lab));
	  break;

     case BNEQ:
	  /* if r.depth >= d and r.ancestor[n] == v->v_val goto lab */
	  lab2 = new_label();
	  g3rri(LDW, r2, r1, 4*DESC_DEPTH);
	  g3rib(BLT, r2, d, lab2);
	  g3rri(LDW, r2, r1, 4*DESC_ANCES);
	  g3rri(LDW, r2, r2, 4*d);
	  g3rib(BEQ, r2, v->v_val, to_label(lab));
	  label(lab2);
	  break;

     default:
	  panic("?typejump");
     }
}

/* icondj -- integer conditional jump */
#define icondj(op, lab) icondj1(op, op##F, lab)

static void icondj1(operation op, operation opf, int lab) {
     reg r1; ctvalue v, v2;

     if (cmpflag)
	  fcondj(opf, lab);						
     else {
	  flush(2); 
	  v = move_to_rc(1);
	  v2 = peek(2);
	  if ((v2->v_op & 0xff) == I_TYPETEST
	      && v->v_op == I_CON && v->v_val == 0
	      && (op == BEQ || op == BNEQ)) {
	       pop(1);
	       typejump(op, v2, lab);
	  } else {							
	       r1 = move_to_reg(2, INT); 
	       pop(2); unlock(2);	
	       if (v->v_op == I_CON)					
		    g3rib(op, r1, v->v_val, to_label(lab));
	       else						
		    g3rrb(op, r1, v->v_reg, to_label(lab));
	  }							
     }

     cmpflag = FALSE;
}

/* callout -- call out-of-line operation */
#define callout(op, nargs) callout1(#op, op, nargs)

static void callout1(const char *name, void (*op)(value *sp), int nargs) {
     reg r;
     flush_stack(0, nargs);
     killregs();
     r = ralloc(INT);
     get_sp(r);
     gcall1(name, (int) op, 1, r);
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
     push(I_CON, INT, ZERO, stack_map(pc), 1); /* PC = stack map */
     push(I_REG, INT, rBP, 0, 1);	       /* BP */
     flush_stack(0, nargs+3);
     killregs();
     r2 = ralloc(INT); rthaw(r1);
     g3rri(LDW, r1, r1, 0);
     get_sp(r2);
     gcallr(r1, 1, r2);
     pop(nargs+3);
}


/* instr -- translate one bytecode instruction */
static void instr(uchar *pc, int i, int arg1, int arg2) {
     reg r1, r2, r3;
     ctvalue v, v2;
     codepoint lab;
     int j;

     switch (i) {
     case I_PUSH:	push(I_CON, INT, ZERO, arg1, 1); break;
     case I_LOCAL:	push(I_ADDR, INT, rBP, arg1, 1); break;

     case I_LDKW:	
	  push(I_LDKW, INT, ZERO, (unsigned) &konst(arg1), 1); 
	  break;
     case I_LDKQ:
	  push(I_LDKQ, INT, ZERO, (unsigned) &konst(arg1), 2); 
	  break;
     case I_LDKF:
	  push(I_LDKF, FLO, ZERO, (unsigned) &konst(arg1), 1); 
	  break;
     case I_LDKD:	
	  push(I_LDKD, FLO, ZERO, (unsigned) &konst(arg1), 2); 
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
	  if (member(v->v_reg, INT) && n_reserved() > 3) move_to_frame(1); 
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
     case I_PLUSA:	plusa(); break;

     case I_LSL:        
	  v = peek(2); v2 = peek(1);
	  if (v->v_op == I_CON && v2->v_op == I_CON) {
	       pop(2); push(I_CON, INT, ZERO, v->v_val << v2->v_val, 1);
	  } else {
	       ibinop(LSH); 
	  }
	  break;

     case I_UMINUS:	imonop(NEG); break;
     case I_BITNOT:	imonop(NOT); break;

     case I_CONVNF:	gmonop(CONVIF, INT, FLO, 1); break;
     case I_CONVND:	gmonop(CONVIF, INT, FLO, 2); break;
     case I_CONVDF:	coerce(1); break;
     case I_CONVFD:	coerce(2); break;
     case I_CONVNC:	gmonop(CONVIC, INT, INT, 1); break;
     case I_CONVNS:	gmonop(CONVIS, INT, INT, 1); break;
  
     case I_NOT:	
	  push(I_CON, INT, ZERO, 0, 1); 
	  ibinop(EQ);
	  break;

     case I_BIT:
	  push(I_CON, INT, ZERO, 2, 1);
	  ibinop(LSH); 
	  r1 = move_to_reg(1, INT);
	  pop(1); unlock(1);
	  push(I_LOADW, INT, r1, (unsigned) bit, 1);
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
	  g1b(JUMP, to_label(arg1)); 
	  break;

     case I_JCASE:
	  lab = new_label();
	  caseptr = vm_jumptable(arg1);

	  flush(1);
	  r1 = move_to_reg(1, INT); pop(1); 
	  r2 = ralloc_suggest(INT, r1); unlock(1);
	  g3rib(BGEQU, r1, arg1, lab);
          g3rri(LSH, r2, r1, 2);
	  g3rri(LDW, r2, r2, (unsigned) caseptr);
	  g1r(JUMP, r2);
	  label(lab);

	  pc += 2;
	  for (j = 0; j < arg1; j++) {
	       case_label(get2(pc)+(pc-pcbase));
	       pc += 2;
	  }
	  break;

     case I_JRANGE:
	  lab = new_label();
	  r1 = move_to_reg(3, INT); 
	  v = fix_const(2, FALSE); 
	  v2 = fix_const(1, FALSE); 
	  pop(3); unlock(3); killregs();
	  g3rib(BLT, r1, v->v_val, lab);
	  g3rib(BLEQ, r1, v2->v_val, to_label(arg1));
	  label(lab);
	  break;

     case I_TESTGEQ:
	  flush(1); 
	  r1 = move_to_reg(2, INT);
	  v = fix_const(1, FALSE);
	  pop(2); unlock(2); killregs();
	  push(I_STACK, INT, ZERO, 0, 1);
	  g3rib(BGEQ, r1, v->v_val, to_label(arg1));
	  break;

     case I_FPLUS:	fbinop(ADDF); break;
     case I_FMINUS:	fbinop(SUBF); break;
     case I_FTIMES:	fbinop(MULF); break;
     case I_FDIV:	fbinop(DIVF); break;
     case I_FUMINUS:    fmonop(NEGF); break;
	  
     case I_DPLUS:	dbinop(ADDF); break;
     case I_DMINUS:	dbinop(SUBF); break;
     case I_DTIMES:	dbinop(MULF); break;
     case I_DDIV:	dbinop(DIVF); break;
     case I_DUMINUS:	dmonop(NEGF); break;

	  /* FCMP or DCMP must be followed by an integer 
	     comparison, so we just set a flag and generate 
	     the appropriate comparison instruction later */

     case I_FCMP: case I_DCMP:	
	  		cmpflag = TRUE; break;

     case I_BOUND:
	  r1 = move_to_reg(2, INT); 
	  v = move_to_rc(1); 
	  pop(2); unlock(2);
	  if (v->v_op == I_CON)
	       g3rib(BGEQU, r1, v->v_val, to_error(E_BOUND, arg1));
	  else
	       g3rrb(BGEQU, r1, v->v_reg, to_error(E_BOUND, arg1));
	  push(I_REG, INT, r1, 0, 1);
	  break;
     
     case I_NCHECK:
	  r1 = move_to_reg(1, INT); pop(1); unlock(1);
	  g3rib(BEQ, r1, 0, to_error(E_NULL, arg1));
	  push(I_REG, INT, r1, 0, 1);
	  break;

     case I_ZCHECK:
	  r1 = move_to_reg(1, INT); pop(1); unlock(1);
	  g3rib(BEQ, r1, 0, to_error(E_DIV, arg1));
	  push(I_REG, INT, r1, 0, 1);
	  break;

     case I_FZCHECK:
	  r1 = move_to_reg(1, FLO); r2 = ralloc(FLO); pop(1); unlock(1);
	  g1r(ZEROF, r2);
	  g3rrb(BEQF, r1, r2, to_error(E_FDIV, arg1));
	  push(I_REG, FLO, r1, 0, 1);
	  break;

     case I_DZCHECK:
	  r1 = move_to_reg(1, FLO); r2 = ralloc(FLO); pop(1); unlock(1);
	  g1r(ZEROF, r2);
	  g3rrb(BEQF, r1, r2, to_error(E_FDIV, arg1));
	  push(I_REG, FLO, r1, 0, 2);
	  break;

     case I_GCHECK:
	  r1 = move_to_reg(1, INT); pop(1); unlock(1);
	  g3rib(BNEQ, r1, 0, to_error(E_GLOB, arg1));
	  break;

     case I_ERROR:
	  g1b(JUMP, to_error(arg1, arg2));
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
	  g3rib(BLTU, rSP, (unsigned) stack + SLIMIT, 
		to_addr(stack_oflo)); /* Check for stack overflow */
          g3rri(LDW, r3, r1, 0);
          g3rri(STW, rSP, r1, 0);
	  gcall(memcpy, 3, rSP, r3, r2);
	  break;

     case I_TYPETEST:
	  r1 = move_to_reg(2, INT); 
	  v = fix_const(1, FALSE); 
	  pop(2); unlock(2);
	  push(I_TYPETEST|(arg1<<16), INT, r1, v->v_val, 1);
	  break;

     case I_LINK:
	  push(I_CON, INT, ZERO, (unsigned) &statlink, 1);
	  store(I_LOADW, INT, 1);
	  break;

     case I_SAVELINK:
	  push(I_LOADW, INT, ZERO, (unsigned) &statlink, 1);
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
	  push(I_LOADW, INT, ZERO, (unsigned) &ob_res, 1);
	  break;

     case I_SLIDEQ:
	  proc_call(pc, arg1);
	  push(I_LOADQ, INT, ZERO, (unsigned) &ob_dres, 2);
	  break;

     case I_SLIDEF:
	  proc_call(pc, arg1);
	  push(I_LOADF, FLO, ZERO, (unsigned) &ob_res, 1);
	  break;

     case I_SLIDED:
	  proc_call(pc, arg1);
	  push(I_LOADD, FLO, ZERO, (unsigned) &ob_dres, 2);
	  break;

     case I_RESULTF:
	  push(I_CON, INT, ZERO, (unsigned) &ob_res, 1);
	  store(I_LOADF, FLO, 1);
	  break;

     case I_RESULTD:
	  push(I_CON, INT, ZERO, (unsigned) &ob_dres, 1);
	  store(I_LOADD, FLO, 2);
	  break;

     case I_RESULTW:
	  push(I_CON, INT, ZERO, (unsigned) &ob_res, 1);
	  store(I_LOADW, INT, 1);
	  break;

     case I_RESULTQ:
	  move_longval(peek(1), ZERO, (unsigned) &ob_dres);
	  pop(1);
	  break;

     case I_RETURN:
	  g0(RET);
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
	  push(I_STACK, INT, ZERO, 0, 1);
	  break;

     case I_CONVNQ:
	  v = peek(1);
	  if (v->v_op == I_CON) {
	       pop(1);
	       push(I_CON, INT, ZERO, v->v_val, 2);
	  } else {
	       callout(long_ext, 1); pop(1);
	       push(I_STACK, INT, ZERO, 0, 2);
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
	  push(I_STACK, FLO, ZERO, 0, 2);
	  break;

     case I_QZCHECK:
	  lab = new_label();
	  v = peek(1);
	  r1 = ralloc(INT);
	  get_halflong(v, 1, r1);
	  g3rib(BNEQ, r1, 0, lab);
	  get_halflong(v, 0, r1);
	  g3rib(BEQ, r1, 0, to_error(E_DIV, arg1));
	  label(lab);
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
	  callout(pack_closure, 2); pop(1); break;

     case I_UNPACK:
	  callout(unpack_closure, 1); push(I_STACK, INT, ZERO, 0, 1); break;
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
	       if (dflag > 1) {
		    printf("\t%.*s%s %d\n", 
			   lev, "++++++++++",
			   instrs[e&IMASK].i_name, arg);
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
	  if (dflag > 1) {
	       int i;
	       printf("%6d: %s", pc-pcbase, instrs[d->d_inst].i_name);
	       for (i = 0; i < nargs; i++) printf(" %d", args[i]);
	       printf("\n");
	  }
#endif

	  lab = to_label(pc-pcbase);

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
}

static void make_error(codepoint lab, int code, int line) {
     label(lab);
     g2ri(MOV, rI0, code);
     g2ri(MOV, rI1, line);
     gcall(rterror, 3, rI0, rI1, rBP);
}

/* jit_compile -- replace a bytecode routine with native code */
void jit_compile(value *cp) {
     code_addr start, entry, finish;

#ifdef DEBUG
     if (dflag > 0) {
	  proc p = find_proc(cp);
	  printf("JIT: %s\n", (p != NULL ? p->p_name : "???"));
     }
#endif

     context = cp; 
     frame = context[CP_FRAME].i;
     pcbase = context[CP_CODE].x;
     pclimit = pcbase + context[CP_SIZE].i;

     init_regs();
     init_patch();
     init_stack(frame);
     cmpflag = FALSE;

     map_labels();
     start = vm_label();
     entry = prolog();
     translate();
     do_errors(make_error);
     finish = vm_label();
     vm_flush(start, finish);
     cp[CP_PRIM].z = (primitive *) entry;

#ifdef DEBUG
     if (dflag > 0) fflush(stdout);
#endif
}

/* interp -- translate procedure on first call */
void interp(value *bp) {
     value *cp = bp[CP].p;
     jit_compile(cp);
     cp[CP_PRIM].z(bp);
}

/* vm_alloc -- upcall from vm to allocate code buffer */
void *vm_alloc(int size) {
     return scratch_alloc(size, TRUE);
}
