/*
 * jitval.c
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
#include <assert.h>

/* Possible compile-time values

     OP            FIELDS      MEANING
                                
     REG           reg         reg
     CON               val     val
     STACKW            val     mem_4[BP + val]
     STACKQ            val     mem_8[BP + val]
     ADDR          reg val     reg + val
     LDKW, LDKF        val     konst_4[val]
     LDKD, LDKQ        val     konst_8[val]
     LOADs         reg val     mem_s[reg + val] for s = C, S, W, D, F, Q
     [FD]CMP[LG], QCMP         comparison of two previous values

LDKW and LDKD refer to the pool of constants for the procedure
being compiled.  */

#ifdef DEBUG
/* show -- print a value for debugging */
static void show(ctvalue v) {
     switch (v->v_op) {
     case I_REG: 
	  printf("reg %s", (v->v_reg == NULL ? "*null*"
                            : vm_regname(v->v_reg->r_reg)));
          break;

     case I_CON:
	  printf("const %d", v->v_val); break;

     case I_STACKW: 
	  printf("stackw %d", v->v_val); break;

     case I_STACKQ: 
	  printf("stackq %d", v->v_val); break;

     case I_FCMPL:
     case I_FCMPG:
     case I_DCMPL:
     case I_DCMPG:
     case I_QCMP:
          printf("compare %s", instrs[v->v_op].i_name);
          break;

     default:
	  printf("[%s %d", instrs[v->v_op].i_name, v->v_val);
	  if (v->v_reg != NULL) printf("(%s)", vm_regname(v->v_reg->r_reg));
	  printf("]");
     }
}

/* dumpregs -- print values cached in all registers */
void dumpregs(void) {
     mybool blank = TRUE;
     reg r;

     for_regs (r) {
	  if (r->r_class == 0 || (r->r_refct == 0 && ! cached(r))) continue;
          if (blank) {
               printf("Regs:");
               blank = FALSE;
          }
	  printf("  %s(%d)", vm_regname(r->r_reg), r->r_refct);
	  if (cached(r)) {
	       printf(" = "); show(&r->r_value);
	  }
     }
     if (! blank) printf("\n");
}
#endif

static mybool same(ctvalue v, ctvalue w) {
     return (v->v_op == w->v_op && v->v_val == w->v_val 
	     && v->v_reg == w->v_reg && v->v_size == w->v_size);
}

void set_cache(reg r, ctvalue v) {
#ifdef DEBUG
     if (dflag >= 4) {
	  printf("\tCache %s = ", vm_regname(r->r_reg));
	  show(v);
	  printf("\n");
     }
#endif

     r->r_value = *v;
}

/* alias -- conservatively test if two values may be aliases */
static mybool alias(ctvalue v, ctvalue w) {
     /* Assume v is a LOAD */

     switch (w->v_op) {
     case I_LOADC:
     case I_LOADS:
     case I_LOADW:
     case I_LOADD:
     case I_LOADF:
     case I_LOADQ:
	  return (v->v_reg != NULL || w->v_reg != NULL || same(v, w));

     default:
	  return FALSE;
     }
}

/* kill_alias -- forget all cached values that might alias v */
static void kill_alias(ctvalue v) {
     reg r;

#ifdef DEBUG
     if (dflag >= 4) {
	  printf("Unalias(");
	  show(v);
	  printf(")\n");
     }
#endif

     for_regs (r) {
	  if (alias(v, &(r->r_value))) 
	       kill(r);
     }
}


/* Compile-time evaluation stack */

#define STACK 32

static struct _ctvalue vstack[STACK];  /* Stack of value descriptions */
static int sp = 0;		/* Number of stack items */

static int offset[STACK];       /* Offset of each item from stack base */
static int pdepth = 0;		/* Total size of runtime stack */
static reg breg;		/* Base register for runtime stack */
static int sbase;		/* Offset of stack base from breg */

/* In normal procedures, breg = rBP and sbase = frame size.  But some
   procedures require a variable-sized area on the stack for copying
   open array parameters passed by value, and they set breg = rSP and
   sbase = 0, after generating code to set rSP to the *base* of the
   working stack area.  There is no top-of-stack register, because
   the working stack size is statically known. */

void init_stack(int frame) {
     sp = 0; 
     pdepth = 0;
     sbase = -frame;
     breg = rBP;
}

int count_args(int size) {
     int nargs = 0;

     while (nargs < sp-1 
	    && offset[sp-nargs-2] < -pdepth + 4 + 4*size) 
	  nargs++;

     return nargs;
}

/* flex_space -- allocate space to copy an open array parameter */
void flex_space(reg nreg) {
     reg r0 = rSP;

     if (breg == rBP) {
	  /* Disable rSP and use it as stack pointer */
	  assert(rSP->r_refct == 0);
	  rSP->r_refct = OMEGA+1;

	  if (sbase == 0) 
	       r0 = rBP;
	  else
	       vm_gen(SUB, rSP->r_reg, rBP->r_reg, -sbase);

	  breg = rSP; sbase = 0;
     }

     vm_gen(SUB, rSP->r_reg, r0->r_reg, nreg->r_reg);
     vm_gen(AND, rSP->r_reg, rSP->r_reg, ~0x3);
}

/* get_sp -- compute value of Oberon stack pointer */
void get_sp(reg r) {
     vm_gen(SUB, r->r_reg, breg->r_reg, pdepth-sbase);
}

/* set -- assign to a stack slot */
static void set(int i, int op, int type, int val, reg r, int s) {
     ctvalue v = &vstack[i];
     reserve(r);

     if (op == I_STACKW || op == I_STACKQ) val = offset[i];

     v->v_op = op; v->v_type = type; v->v_val = val; 
     v->v_reg = r; v->v_size = s;

#ifdef DEBUG
     if (dflag >= 3) {
	  printf("<%d> = ", i);
	  show(v);
	  printf(" (%d/%d)\n", offset[i], s);
     }
#endif
}

/* push -- push a value onto the eval stack */
void push(int op, int type, reg r, int val, int size) {
     pdepth += 4*size;
     offset[sp] = -pdepth;
     set(sp++, op, type, val, r, size);
}

/* pop -- pop one or more values from the eval stack */
void pop(int n) {
     for (int i = sp - n; i < sp; i++) 
	  rfree(vstack[i].v_reg);

     sp -= n;
     pdepth = (sp == 0 ? 0 : -offset[sp-1]);
}

/* peek -- access value near top of stack */
ctvalue peek(int n) {
     return &vstack[sp-n];
}

/* unlock -- unlock registers used near the top of the stack */
void unlock(int n) {
     for (int i = sp; i < sp + n; i++)
	  runlock(vstack[i].v_reg);
}

/* save_stack -- record stack contents at a forward branch */
void save_stack(codepoint lab) {
     int map = 0;

     if (sp > 32) panic("too many items to save");

     /* Make a bitmap showing the size of each item */
     for (int i = 0; i < sp; i++) {
	  if (vstack[i].v_size == 2)
	       map |= (1 << i);
     }

     lab->l_depth = sp;
     lab->l_stack = map;
}

/* restore_stack -- restore stack state at target of a forward branch */
void restore_stack(codepoint lab) {
     int n = lab->l_depth, map = lab->l_stack;

#ifdef DEBUG
     if (dflag >= 2 && n > 0) printf("[Restore %d]\n", n);
#endif

     sp = 0; pdepth = 0;
     for (int i = 0; i < n; i++) {
	  if (map & (1 << i))
	       push(I_STACKQ, INT, NULL, 0, 2);
	  else
	       push(I_STACKW, INT, NULL, 0, 1);	       
     }
}

vmlabel target(int addr) {
     codepoint lab = find_label(addr);
     if (lab->l_depth < 0)
          save_stack(lab);
     return lab->l_vmlab;
}


/* Value motion */

/* move_from_frame -- move vstack[i] so that it is not in the runtime stack */
ctvalue move_from_frame(int i) {
     ctvalue v = &vstack[sp-i];

     if (v->v_op == I_STACKW) {
	  reg r = move_to_reg(i, INT);
	  runlock(r);
     }

     return v;
}

#define choose(size, opw, opd) \
     ((size) == 1 ? opw : (size) == 2 ? opd : (panic("choose"), 999))

void ldst_item(int op, reg r, int i) {
     vm_gen(op, r->r_reg, breg->r_reg, sbase+offset[sp-i]);
}

/* move_to_frame -- force vstack[sp-i] into the runtime stack */
void move_to_frame(int i) {
     ctvalue v = &vstack[sp-i];
     reg r;

#ifdef DEBUG
     if (dflag >= 3) {
          printf("move_to_frame(%d: ", sp-i);
          show(v);
          printf(")\n");
     }
#endif

     if (v->v_op != I_STACKW && v->v_op != I_STACKQ) {
#ifndef M64X32
	  if (v->v_type == INT && v->v_size == 2) {
               move_longval(v, breg, sbase+offset[sp-i]);
               rfree(v->v_reg);
          } else
#endif
          {
               r = move_to_reg(i, v->v_type); runlock(r); rfree(r);
               ldst_item(choose(v->v_size, STW, STQ), r, i);
               if (v->v_op != I_REG && v->v_reg != r) 
                    set_cache(r, v);
          }

	  set(sp-i, choose(v->v_size, I_STACKW, I_STACKQ), 
	      v->v_type, 0, NULL, v->v_size);
     }
}

/* transient -- check if a value is not preserved across a procedure call */
static mybool transient(ctvalue v) {
     if (v->v_reg != NULL && v->v_reg->r_class != 0)
	  return TRUE;

     switch (v->v_op) {
     case I_LOADW:
     case I_LOADF:
     case I_LOADQ:
     case I_LOADD:
	  return (v->v_val == address(&ob_res));
     default:
	  return FALSE;
     }
}

/* flush_stack -- flush values into the runtime stack */
void flush_stack(int a, int b) {
     /* Values vstack[0..sp-b) are flushed if they use an
	     allocable register or the result location.
	Values vstack[sp-b..sp-a) are all flushed (perhaps to
	     become arguments in a procedure call).
	Values vstack[sp-a..sp) are left alone */

     for (int j = sp; j > a; j--)
	  if (j <= b || transient(&vstack[sp-j]))
	       move_to_frame(j);
}

/* spill -- scan stack and spill values that use a given reg */
void spill(reg r) {
     int *rc = &r->r_refct;
     mybool saved = FALSE;

     static unsigned tmp;
     if (tmp == 0) tmp = address(scratch_alloc(sizeof(double)));

     for (int i = sp; i > 0 && *rc > 0; i--) {
          ctvalue v = &vstack[sp-i];
	  if (vstack[sp-i].v_reg == r) {
               if (*rc == 1 || v->v_op == I_REG)
                    move_to_frame(i);
               else {
                    /* If r is a least-used register and has multiple
                       references, then we are going to have to help 
                       out the register allocator a bit. */
                    int c = *rc;

                    if (!saved) {
                         vm_gen(choose(v->v_size, STW, STQ), r->r_reg, tmp);
                         saved = TRUE;
                    }

                    *rc = 1;
                    move_to_frame(i);
                    vm_gen(choose(v->v_size, LDW, LDQ), r->r_reg, tmp);
                    *rc = c-1;
               }
          }
     }
}

/* ldst -- load or store, with optional index register */
static void ldst(int op, reg rs, reg rd, int off) {
     if (rd == NULL)
          vm_gen(op, rs->r_reg, off);
     else
          vm_gen(op, rs->r_reg, rd->r_reg, off);
}

/* load -- load from memory into register */
static reg load(operation op, int cl, reg r, int val) {
     reg r1;
     rfree(r); rlock(r);
     r1 = ralloc(cl);
     runlock(r);
     ldst(op, r1, r, val);
     return r1;
}

/* move_to_reg -- move stack item to a register */
reg move_to_reg(int i, int ty) {
     ctvalue v = &vstack[sp-i];
     reg r = NULL, r2;

#ifdef DEBUG
     if (dflag >= 3) {
          printf("move_to_reg(%d: ", sp-i);
          show(v);
          printf(")\n");
     }
#endif

     if (v->v_op != I_REG) {
	  for_regs (r) {
	       if (cached(r) && same(&r->r_value, v) && member(r, ty)) {
#ifdef DEBUG
		    if (dflag >= 4) printf("Hit %s\n", vm_regname(r->r_reg));
#endif
		    rfree(v->v_reg);
		    set(sp-i, I_REG, ty, 0, r, v->v_size);
		    return rlock(r);
	       }
	  }
     }

     switch (v->v_op) {
     case I_REG:
	  r = rfree(v->v_reg);
	  break;

     case I_CON:
	  r = ralloc(INT);
	  vm_gen(MOV, r->r_reg, v->v_val);
#ifdef M64X32
          if (v->v_size == 2 && v->v_val < 0)
               vm_gen(SXTq, r->r_reg, r->r_reg);
#endif
	  break;

     case I_LDKW:
     case I_LDKF:
	  r = ralloc(ty);
          vm_gen(LDKW, r->r_reg, v->v_val);
	  break;
	  
     case I_LDKQ:
     case I_LDKD:
	  r = ralloc(ty);
	  vm_gen(LDQ, r->r_reg, v->v_val);
	  break;

     case I_ADDR:
	  rfree(v->v_reg); rlock(v->v_reg);
          r = ralloc_suggest(INT, v->v_reg);
          runlock(v->v_reg);
	  vm_gen(ADD, r->r_reg, v->v_reg->r_reg, v->v_val);
	  break;

     case I_LOADW:	
     case I_LOADF:
	  r = load(LDW, ty, v->v_reg, v->v_val);
	  break;

     case I_LOADS:	
	  r = load(LDS, INT, v->v_reg, v->v_val); 
	  break;

     case I_LOADC:	
	  r = load(LDBu, INT, v->v_reg, v->v_val); 
	  break;

     case I_LOADD: 
     case I_LOADQ:
	  r = load(LDQ, ty, v->v_reg, v->v_val); 
	  break;

     case I_STACKW:
	  r = load(LDW, ty, reserve(breg), sbase + v->v_val);
	  break;

     case I_STACKQ:
	  r = load(LDQ, ty, reserve(breg), sbase + v->v_val);
	  break;

     default:
	  panic("move_to_reg %s\n", instrs[v->v_op].i_name);
     }

     /* Unusually, e.g. in SYSTEM.VAL(REAL, n+1), a floating point
	value can appear in an integer register, or vice versa. 
        See test tValReal.m */
     if (rkind(r) != ty) {
	  r2 = ralloc(ty);
	  vm_gen(choose(v->v_size, MOV, MOVq), r2->r_reg, r->r_reg);
	  r = r2;
     }

     if (v->v_op != I_STACKW && v->v_op != I_STACKQ && v->v_reg != r)
	  set_cache(r, v);

     set(sp-i, I_REG, ty, 0, r, v->v_size);
     return rlock(r);
}

/* fix_const -- check a stack item is a constant or move it to a register */
ctvalue fix_const(int i, mybool rflag) {
     ctvalue v = &vstack[sp-i];

     switch (v->v_op) {
     case I_CON:
	  break;

     case I_LDKW:
	  set(sp-i, I_CON, INT, *ptrcast(int, v->v_val), NULL, 1);
	  break;

     default:
	  if (!rflag)
	       panic("fix_const %s", instrs[v->v_op].i_name);
	  move_to_reg(i, INT);
     }

     return v;
}

/* deref -- perform load operation on top of stack */
void deref(int op, int ty, int size) {
     ctvalue v = &vstack[sp-1];
     reg r1;

     switch (v->v_op) {
     case I_ADDR:
	  pop(1);
	  push(op, ty, v->v_reg, v->v_val, size);
	  break;

     case I_CON:
     case I_LDKW:
	  fix_const(1, FALSE); pop(1); unlock(1);
	  push(op, ty, NULL, v->v_val, size);
	  break;

     default:
	  r1 = move_to_reg(1, INT); pop(1); unlock(1); 
	  push(op, ty, r1, 0, size); 
	  break;
     }
}

/* unalias -- execute load operations that might alias v */
static void unalias(int a, ctvalue v) {
     for (int i = sp; i > a; i--) {
	  ctvalue w = &vstack[sp-i];
	  if (alias(v, w)) {
#ifndef M64X32
	       if (w->v_op == I_LOADQ)
		    move_to_frame(i);
               else
#endif
	       {
		    reg r = move_to_reg(i, w->v_type);
                    runlock(r);
               }
	  }
     }
}

/* store -- perform store operation on top of stack */
void store(int ldop, int s) {
     reg r1;
     ctvalue v;
     int op = -1;
     mybool cache = TRUE;

     deref(ldop, vstack[sp-2].v_type, s);							
     v = &vstack[sp-1];
     if (same(v, &vstack[sp-2])) {
	  /* Store into same location as load: mostly for
	     SLIDEW / RESULTW */
	  pop(2);
	  return;
     }

     unalias(2, v); 

     int ty = v->v_type;

#ifndef M64X32
     if (ldop == I_LOADQ) {
	  move_longval(&vstack[sp-2], vstack[sp-1].v_reg, vstack[sp-1].v_val);
	  pop(2);
	  return;
     }

     if (ldop == I_LOADD) ty = FLO;
#endif
     
     rlock(v->v_reg);
     r1 = move_to_reg(2, ty); 
     pop(2); unlock(2);						

     switch (ldop) {
     case I_LOADW:
     case I_LOADF:
          op = STW; break;
     case I_LOADC:
          op = STB; cache = FALSE; break;
     case I_LOADS:
          op = STS; cache = FALSE; break;
     case I_LOADD:
     case I_LOADQ:
          op = STQ; break;

     default:
	  panic("put %s", instrs[ldop].i_name);
     }

     ldst(op, r1, v->v_reg, v->v_val);
     kill_alias(v);
     if (cache && v->v_reg != r1) set_cache(r1, v);
}

/* plusa -- add address and offset */
void plusa() {
     ctvalue v1, v2;
     reg r1, r2;

     switch (vstack[sp-2].v_op) {
     case I_CON:
     case I_LDKW:
	  v1 = fix_const(2, FALSE); 
	  v2 = move_to_rc(1); 
	  pop(2); unlock(2);
	  if (v2->v_op == I_CON)
	       push(I_CON, INT, NULL, v1->v_val + v2->v_val, 1);
	  else {
               r2 = v2->v_reg;
	       push(I_ADDR, INT, r2, v1->v_val, 1);
          }
	  break;

     case I_ADDR:
	  v1 = &vstack[sp-2];
	  v2 = move_to_rc(1); 
	  pop(2);
	  if (v2->v_op == I_CON)
	       push(I_ADDR, INT, v1->v_reg, v1->v_val + v2->v_val, 1);
	  else {
	       r1 = ralloc_suggest(INT, v1->v_reg); 
               r2 = v2->v_reg;
	       runlock(v2->v_reg);
	       vm_gen(ADD, r1->r_reg, v1->v_reg->r_reg, r2->r_reg);
	       push(I_ADDR, INT, r1, v1->v_val, 1);
	  }
	  break;

     default:
	  r1 = move_to_reg(2, INT); 
	  v2 = move_to_rc(1); 
	  pop(2);
	  if (v2->v_op == I_CON) {
               // No need to sign extend as r1 is an address
	       unlock(2);
	       push(I_ADDR, INT, r1, v2->v_val, 1);
	  } else {
	       r2 = ralloc_suggest(INT, r1); 
	       unlock(2);
	       vm_gen(ADD, r2->r_reg, r1->r_reg, v2->v_reg->r_reg);
	       push(I_REG, INT, r2, 0, 1);
	  }
     }
}


/* Procedure calls */

/* gen_args -- generate ARG instructions from right to left */
static void gen_args(int n) {
     vm_gen(PREP, n);
     for (int i = n; i > 0; i--) {
          ctvalue arg = &vstack[sp-i];
          switch (arg->v_op) {
          case I_REG:
               vm_gen(ARG, arg->v_reg->r_reg);
               break;
          case I_CON:
               vm_gen(ARG, arg->v_val);
               break;
          default:
               panic("Bad arg code %d", arg->v_op);
          }
     }
     pop(n);
}

#define __func2__(name, fun) fun,
static void *func_table[] = {
     __FUNC__(__func2__)
};

static word func_wrapper[NFUNC];

/* gcall -- call with arguments on stack */
void gcall(func f, int n) {
     gen_args(n);
     if (func_wrapper[f] == 0)
          func_wrapper[f] = vm_wrap(func_table[f]);
     vm_gen(CALL, func_wrapper[f]);
}

/* gcallr -- indirect function call */
void gcallr(reg r, int n) {
     gen_args(n);
     vm_gen(CALL, r->r_reg);
}


/* 64-bit operations */

#ifndef M64X32
/* move_long -- move a 64-bit value */
static void move_long(reg rs, int offs, reg rd, int offd) {
     reg r1, r2;
     
     if (rs == rd && offs == offd) return;

     rlock(rs); rlock(rd);
     r1 = ralloc(INT); r2 = ralloc_avoid(INT, r1);
     runlock(rs); runlock(rd);
     ldst(LDW, r1, rs, offs);
     ldst(LDW, r2, rs, offs+4);
     ldst(STW, r1, rd, offd);
     ldst(STW, r2, rd, offd+4);
}
     
/* half_const -- fetch one or other half of a 64-bit constant */
static int half_const(ctvalue v, int off) {
     switch (v->v_op) {
     case I_CON:
	  if (off == 0)
	       return v->v_val;
	  else
	       return (v->v_val < 0 ? -1 : 0);

     case I_LDKQ:
	  return ptrcast(int, v->v_val)[off];

     default:
	  panic("half_const %s", instrs[v->v_op].i_name);
	  return 0;
     }
}     

/* move_longconst -- move 64-bit constant */
static void move_longconst(ctvalue src, reg rd, int offd) {
     reg r1, r2;

     rlock(rd);
     r1 = ralloc(INT); r2 = ralloc_avoid(INT, r1);
     runlock(rd);

     vm_gen(MOV, r1->r_reg, half_const(src, 0));
     vm_gen(MOV, r2->r_reg, half_const(src, 1));
     ldst(STW, r1, rd, offd);
     ldst(STW, r2, rd, offd+4);
}

/* move_longval -- move a long value into memory */
void move_longval(ctvalue src, reg rd, int offd) {
     /* Move from src to offd(rd) */

     switch (src->v_op) {
     case I_LOADQ:
     case I_LOADD:
          /* Could be LOADD if the result of SYSTEM.VAL(LONGINT, ...) */
	  move_long(src->v_reg, src->v_val, rd, offd);
	  break;
     case I_STACKQ:
	  move_long(breg, sbase + src->v_val, rd, offd);
	  break;
     case I_LDKQ:
     case I_CON:
          move_longconst(src, rd, offd);
	  break;
     case I_REG:
          /* Must be result of SYSTEM.VAL(LONGINT, x) */
          assert(src->v_type == FLO);
          ldst(STQ, src->v_reg, rd, offd);
          break;
     default:
	  panic("move_longval %s", instrs[src->v_op].i_name);
     }
}
#endif
