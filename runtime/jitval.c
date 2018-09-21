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

     OP    FIELDS      MEANING
                                
     CON       val      val
     REG   reg          reg
     ADDR  reg val r2 k if r2 = NULL: reg + val else: reg + r2<<k
     KONW      val      konst_4[val]
     KONQ      val      konst_8[val]
     MEMs  reg val r2 k mem_s[reg + val] or mem_s[reg + r2<<k]
     STKW      val      mem_4[BP + val]
     STKQ      val      mem_8[BP + val]
     *CMP*              comparison of two previous values */

#define __v2__(sym) #sym,
static char *vkind_name[] = { __VALKINDS__(__v2__) };

#ifdef DEBUG
/* show -- print a value for debugging */
static void show(ctvalue v) {
     switch (v->v_op) {
     case V_REG: 
	  printf("reg %s", (v->v_reg == NULL ? "*null*"
                            : vm_regname(v->v_reg->r_reg)));
          break;

     case V_CON:
	  printf("const %d", v->v_val); break;

     case V_STKW: 
	  printf("stackw %d", v->v_val); break;

     case V_STKQ: 
	  printf("stackq %d", v->v_val); break;

     case V_FCMPL:
     case V_FCMPG:
     case V_DCMPL:
     case V_DCMPG:
     case V_QCMP:
          printf("compare %s", vkind_name[v->v_op]);
          break;

#define regname(r) vm_regname(r->r_reg)

     default:
          if (v->v_reg2 != NULL && v->v_scale != 0)
               printf("[%s (%s+%s<<%d)]", vkind_name[v->v_op],
                      regname(v->v_reg), regname(v->v_reg2), v->v_scale);
          else if (v->v_reg2 != NULL)
               printf("[%s (%s+%s)]", vkind_name[v->v_op],
                      regname(v->v_reg), regname(v->v_reg2));
          else if (v->v_reg != NULL)
               printf("[%s %d(%s)]", vkind_name[v->v_op],
                      v->v_val, regname(v->v_reg));
          else
               printf("[%s %d]", vkind_name[v->v_op], v->v_val);
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
	       printf("="); show(&r->r_value);
	  }
     }
     if (! blank) printf("\n");
}
#endif

static mybool same(ctvalue v, ctvalue w) {
     return (v->v_op == w->v_op && v->v_val == w->v_val 
	     && v->v_reg == w->v_reg && v->v_reg2 == w->v_reg2
             && v->v_scale == w->v_scale && v->v_size == w->v_size);
}

void set_cache(reg r, ctvalue v) {
     if (v->v_reg == r || v->v_reg2 == r) return;

#ifdef DEBUG
     if (dflag >= 4) {
	  printf("Cache %s = ", vm_regname(r->r_reg));
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
     case V_MEMC:
     case V_MEMS:
     case V_MEMW:
     case V_MEMQ:
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
static void set(int i, valkind vkind, int type,
                int val, reg r, reg r2, int scale, int s) {
     ctvalue v = &vstack[i];
     reserve(r); reserve(r2);

     if (vkind == V_STKW || vkind == V_STKQ) val = offset[i];

     v->v_op = vkind; v->v_type = type; v->v_val = val; 
     v->v_reg = r; v->v_reg2 = r2; v->v_scale = scale; v->v_size = s;

#ifdef DEBUG
     if (dflag >= 3) {
	  printf("<%d> = ", i);
	  show(v);
	  printf(" (%d/%d)\n", offset[i], s);
     }
#endif
}

/* pushx -- push a value onto the eval stack */
void pushx(valkind vkind, int type, int val, reg r, reg r2,
           int scale, int size) {
#ifndef FLOATOPS
     type = INT;
#endif
     pdepth += 4*size;
     offset[sp] = -pdepth;
     set(sp++, vkind, type, val, r, r2, scale, size);
}

/* pop -- pop one or more values from the eval stack */
void pop(int n) {
     for (int i = sp - n; i < sp; i++) {
	  rfree(vstack[i].v_reg);
          rfree(vstack[i].v_reg2);
     }

     sp -= n;
     pdepth = (sp == 0 ? 0 : -offset[sp-1]);
}

/* peek -- access value near top of stack */
ctvalue peek(int n) {
     return &vstack[sp-n];
}

/* unlock -- unlock registers used near the top of the stack */
void unlock(int n) {
     for (int i = sp; i < sp + n; i++) {
	  runlock(vstack[i].v_reg);
          runlock(vstack[i].v_reg2);
     }
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
	       push(V_STKQ, INT, 0, NULL, 2);
	  else
	       push(V_STKW, INT, 0, NULL, 1);	       
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

     if (v->v_op == V_STKW) {
	  reg r = move_to_reg(i, v->v_type);
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

     if (v->v_op != V_STKW && v->v_op != V_STKQ) {
#ifndef M64X32
	  if (v->v_type == INT && v->v_size == 2) {
               move_longval(v, breg, sbase+offset[sp-i]);
               rfree(v->v_reg);
          } else
#endif
          {
               r = move_to_reg(i, v->v_type); runlock(r); rfree(r);
	       ldst_item(choose(v->v_size, STW, STQ), r, i);
               if (v->v_op != V_REG) set_cache(r, v);
          }

	  set(sp-i, choose(v->v_size, V_STKW, V_STKQ), 
	      v->v_type, 0, NULL, NULL, 0, v->v_size);
     }
}

/* transient -- check if a value is not preserved across a procedure call */
static mybool transient(ctvalue v) {
     if (v->v_reg != NULL && v->v_reg->r_class != 0) return TRUE;
     if (v->v_reg2 != NULL && v->v_reg2->r_class != 0) return TRUE;

     switch (v->v_op) {
     case V_MEMW:
     case V_MEMQ:
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
	  if (vstack[sp-i].v_reg == r || vstack[sp-i].v_reg2 == r) {
               if (*rc == 1 || v->v_op == V_REG)
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

static reg loadv(operation op, int cl, ctvalue v) {
     rfree(v->v_reg); rlock(v->v_reg);
     rfree(v->v_reg2); rlock(v->v_reg2);
     reg r1 = ralloc(cl);
     runlock(v->v_reg); runlock(v->v_reg2);

     if (v->v_reg2 != NULL)
          vm_gen(op, r1->r_reg, v->v_reg->r_reg, v->v_reg2->r_reg, v->v_scale);
     else if (v->v_reg != NULL)
          vm_gen(op, r1->r_reg, v->v_reg->r_reg, v->v_val);
     else
          vm_gen(op, r1->r_reg, v->v_val);

     return r1;
}


static void storev(operation op, reg r, ctvalue v) {
     if (v->v_reg2 != NULL)
          vm_gen(op, r->r_reg, v->v_reg->r_reg, v->v_reg2->r_reg, v->v_scale);
     else if (v->v_reg != NULL)
          vm_gen(op, r->r_reg, v->v_reg->r_reg, v->v_val);
     else
          vm_gen(op, r->r_reg, v->v_val);
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

     if (v->v_op != V_REG) {
	  for_regs (r) {
	       if (cached(r) && same(&r->r_value, v) && member(r, ty)) {
#ifdef DEBUG
		    if (dflag >= 4) printf("Hit %s\n", vm_regname(r->r_reg));
#endif
		    rfree(v->v_reg); rfree(v->v_reg2);
		    set(sp-i, V_REG, ty, 0, r, NULL, 0, v->v_size);
		    return rlock(r);
	       }
	  }
     }

     switch (v->v_op) {
     case V_REG:
	  r = rfree(v->v_reg);
	  break;

     case V_CON:
	  r = ralloc(INT);
	  vm_gen(MOV, r->r_reg, v->v_val);
#ifdef M64X32
          if (v->v_size == 2 && v->v_val < 0)
               vm_gen(SXTq, r->r_reg, r->r_reg);
#endif
	  break;

     case V_ADDR:
          if (v->v_reg2 != NULL) {
               rfree(v->v_reg); rlock(v->v_reg);
               rfree(v->v_reg2); rlock(v->v_reg2);
               r = ralloc_suggest(INT, v->v_reg);
               runlock(v->v_reg); runlock(v->v_reg2);
               vm_gen(ADD, r->r_reg, v->v_reg->r_reg,
                      v->v_reg2->r_reg, v->v_scale);
          } else if (v->v_val == 0) {
               r = rfree(v->v_reg);
          } else {
               rfree(v->v_reg); rlock(v->v_reg);
               r = ralloc_suggest(INT, v->v_reg);
               runlock(v->v_reg);
               vm_gen(ADD, r->r_reg, v->v_reg->r_reg, v->v_val);
          }
	  break;

     case V_MEMW:	
	  r = loadv(LDW, ty, v);
 	  break;
 
     case V_MEMS:	
	  r = loadv(LDS, INT, v); 
 	  break;
 
     case V_MEMC:	
          r = loadv(LDBu, INT, v); 
 	  break;
 
     case V_MEMQ:
	  r = loadv(LDQ, ty, v); 
 	  break;
 
     case V_KONW:
          r = load(LDW, ty, rCP, v->v_val);
          break;

     case V_KONQ:
          r = load(LDQ, ty, rCP, v->v_val);
          break;

     case V_STKW:
	  r = load(LDW, ty, reserve(breg), sbase + v->v_val);
	  break;

     case V_STKQ:
	  r = load(LDQ, ty, reserve(breg), sbase + v->v_val);
	  break;

     default:
	  panic("move_to_reg %s\n", vkind_name[v->v_op]);
     }

     /* Unusually, e.g. in SYSTEM.VAL(REAL, n+1), a floating point
	value can appear in an integer register, or vice versa. 
        See test tValReal.m */
     if (rkind(r) != ty) {
	  r2 = ralloc(ty);
	  vm_gen(choose(v->v_size, MOV, MOVq), r2->r_reg, r->r_reg);
	  r = r2;
     }

     if (v->v_op != V_STKW && v->v_op != V_STKQ) set_cache(r, v);

     set(sp-i, V_REG, ty, 0, r, NULL, 0, v->v_size);
     return rlock(r);
}

/* fix_const -- check a stack item is a constant, or maybe move to a reg */
ctvalue fix_const(int i, mybool rflag) {
     ctvalue v = &vstack[sp-i];
     extern value *jit_cxt;

     switch (v->v_op) {
     case V_CON:
          break;

     case V_KONW:
          v->v_op = V_CON;
          v->v_val = * (word *) ((uchar *) jit_cxt + v->v_val);
          break;

     default:
          if (!rflag)
               panic("fix_const %s", vkind_name[v->v_op]);
          move_to_reg(i, INT);
     }

     return v;
}

/* deref -- perform load operation on top of stack */
void deref(valkind vkind, int ty, int size) {
     ctvalue v = peek(1);
     reg r1;

     switch (v->v_op) {
     case V_ADDR:
#ifndef M64X32
          if (vkind == V_MEMQ && ty == INT && v->v_reg2 != NULL)
               goto catchall;
#endif

	  pop(1);
	  pushx(vkind, ty, v->v_val, v->v_reg, v->v_reg2, v->v_scale, size);
          break;

     case V_CON:
	  pop(1); unlock(1);
 	  push(vkind, ty, v->v_val, NULL, size);
 	  break;

     default:
#ifndef M64X32
     catchall:
#endif
	  r1 = move_to_reg(1, INT); pop(1); unlock(1); 
	  push(vkind, ty, 0, r1, size); 
	  break;
     }
 }

/* unalias -- execute load operations that might alias v */
static void unalias(int a, ctvalue v) {
     for (int i = sp; i > a; i--) {
	  ctvalue w = &vstack[sp-i];
	  if (alias(v, w)) {
#ifndef M64X32
	       if (w->v_op == V_MEMQ)
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
void store(valkind vkind, int s) {
     reg r1;
     ctvalue v = &vstack[sp-1];
     int ty = v->v_type;

     deref(vkind, vstack[sp-2].v_type, s);
     if (same(v, &vstack[sp-2])) {
	  /* Store into same location as load: mostly for
	     SLIDEW / RESULTW */
	  pop(2);
	  return;
     }

     unalias(2, v); 

#ifndef M64X32
     if (ty == INT && vkind == V_MEMQ) {
          if (v->v_reg2 != NULL) panic("Oops!");
          move_longval(&vstack[sp-2], v->v_reg, v->v_val);
          pop(2);
	  return;
     }
#endif
     
     rlock(v->v_reg); rlock(v->v_reg2);
     r1 = move_to_reg(2, ty); 
     pop(2); unlock(2);						

     switch (vkind) {
     case V_MEMW:
          storev(STW, r1, v); break;
     case V_MEMC:
          storev(STB, r1, v); break;
     case V_MEMS:
          storev(STS, r1, v); break;
     case V_MEMQ:
          storev(STQ, r1, v); break;
     default:
	  panic("store %s", vkind_name[vkind]);
     }

     kill_alias(v);
     if (vkind == V_MEMW || vkind == V_MEMQ) set_cache(r1, v);
}

/* add_offset -- add address and offset */
void add_offset(int scale) {
     ctvalue v1 = peek(2), v2;
     reg r1;

     switch (v1->v_op) {
     case V_ADDR:
          if (v1->v_reg2 != NULL) goto catchall;
 	  v2 = move_to_rc(1); 
	  if (v2->v_op == V_CON) {
               pop(2);
	       push(V_ADDR, INT, v1->v_val + (v2->v_val<<scale), v1->v_reg, 1);
          } else if (v1->v_val == 0) {
               pop(2);
               push2(V_ADDR, INT, v1->v_reg, v2->v_reg, scale, 1);
          } else {
               pop(2); 
               reserve(v2->v_reg);
               r1 = ralloc_suggest(INT, v1->v_reg);
               rthaw(v2->v_reg);
	       vm_gen(ADD, r1->r_reg, v1->v_reg->r_reg, v1->v_val);
	       push2(V_ADDR, INT, r1, v2->v_reg, scale, 1);
 	  }
 	  break;

     default:
     catchall:
	  r1 = move_to_reg(2, INT); 
	  v2 = move_to_rc(1); 
	  pop(2); unlock(2);
	  if (v2->v_op == V_CON)
 	       push(V_ADDR, INT, v2->v_val<<scale, r1, 1);
          else
	       push2(V_ADDR, INT, r1, v2->v_reg, scale, 1);
     }
}


/* Procedure calls */

/* gen_args -- generate ARG instructions from right to left */
static void gen_args(int n) {
     vm_gen(PREP, n);
     for (int i = n; i > 0; i--) {
          ctvalue arg = &vstack[sp-i];
          switch (arg->v_op) {
          case V_REG:
               vm_gen(ARG, arg->v_reg->r_reg);
               break;
          case V_CON:
               vm_gen(ARG, arg->v_val);
               break;
          default:
               panic("Bad arg code %d", arg->v_op);
          }
     }
     pop(n);
}

#ifndef M64X32
#define func_wrapper(f) ((word) (f))
#else
// Only a few wrappers are generally needed on amd64, but we
// allow extra room to allow experiments with soft float

#define NWRAP 128

static word wrapper[NWRAP];
static void *wrfunc[NWRAP];
int nwrap = 0;

word func_wrapper(void *f) {
     for (int i = 0; i < nwrap; i++)
          if (wrfunc[i] == f) return wrapper[i];

     assert(nwrap < NWRAP);
     int j = nwrap++;
     word w = vm_wrap(f);
     wrapper[j] = w; wrfunc[j] = f;
     return w;
}
#endif

/* gcall -- call with arguments on stack */
void gcall(void *f, int n) {
     gen_args(n);
     vm_gen(CALL, func_wrapper(f));
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
     
/* move_longconst -- move 64-bit constant */
static void move_longconst(ctvalue src, reg rd, int offd) {
     reg r1;

     rlock(rd);
     r1 = ralloc(INT);
     runlock(rd);

     vm_gen(MOV, r1->r_reg, src->v_val);
     ldst(STW, r1, rd, offd);
     vm_gen(MOV, r1->r_reg, (src->v_val < 0 ? -1 : 0));
     ldst(STW, r1, rd, offd+4);
}

/* move_longval -- move a long value into memory */
void move_longval(ctvalue src, reg rd, int offd) {
     /* Move from src to offd(rd) */

     switch (src->v_op) {
     case V_MEMQ:
	  move_long(src->v_reg, src->v_val, rd, offd);
	  break;
     case V_KONQ:
          move_long(rCP, src->v_val, rd, offd);
          break;
     case V_STKQ:
	  move_long(breg, sbase + src->v_val, rd, offd);
	  break;
     case V_CON:
          move_longconst(src, rd, offd);
	  break;
     case V_REG:
          /* Must be result of SYSTEM.VAL(LONGINT, x) */
          assert(src->v_type == FLO);
          ldst(STQ, src->v_reg, rd, offd);
          break;
     default:
	  panic("move_longval %s", vkind_name[src->v_op]);
     }
}
#endif
