/*
 * jitregs.c
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
#include "jit.h"
#include <assert.h>

/* We use registers rI0 etc. in the JIT translator, and they are mapped onto
   registers with names like xR0 that are part of the RISC-ish
   portability interface.  Behind that interface, xR0 might be mapped
   to the EAX register on a PC.  What might be confusing is that the
   x86 has its own registers called BP and SP, and they may or may not
   be the same as the resisters xV3 and xV2 that we rename as rBP and
   rSP here. */

#define novalue { 0, 0, 0, ZERO, 0 }
#define __r2__(sym, phys, class) { #sym, phys, class, 0, novalue },
struct _reg regs[] = { __REGS__(__r2__) };

/* Each register has a reference count, and may also be locked -- this
   is signified by increasing the reference count by OMEGA.  Normally, a
   register is unlocked and has a reference count equal to the number of
   times it appears in the stack.
   
   A typical allocation phase for an operation looks like this:
   
   move_to_reg(sp-2); move_to_reg(sp-1);
   // Now the two top items on the stack are in registers that are locked.
   r1 = ralloc(IREG);
   // r1 is guaranteed to be different from the input registers holding the 
   // two stack items
   pop(2);
   // The two registers are still locked, but their reference counts
   // have been decremented
   r2 = ralloc_avoid(IREG, r1);
   // r2 is different from r1, but one of the two input registers may be reused.
   // However, the input registers will not be spilled.
   unlock(2);
   // The input registers are now unlocked.
   
   The ralloc function does not increment the reference count of the
   register that it allocates; that is done later when a value is pushed
   onto the stack. */

/* n_reserved -- count reserved integer registers */
int n_reserved(void) {
     int r, res = 0;

     for (r = 0; r < NREGS; r++) {
	  if (member(r, INT) && regs[r].r_refct > 0)
	       res++;
     }

     return res;
}

/* incref -- increment or decrement reference count */
reg incref(reg r, int inc) {
     if (regs[r].r_class != 0) regs[r].r_refct += inc;
     return r;
}

/* ralloc_avoid -- allocate register, avoiding one previously allocated */
reg ralloc_avoid(int s, reg r2) {
     reg best = ZERO;
     unsigned ir;
     int min = 2, cost;

     static mybool spilling = FALSE;

     /* See if there is an unused register */
     for (ir = 0; ir < NREGS; ir++) {
	  reg r = (reg) ir;

	  /* Locked registers are OK if their refcount is otherwise 0 */
	  if (member(r, s) && regs[r].r_refct % OMEGA == 0 && r != r2) {
	       cost = (cached(r));
	       if (cost < min) {
		    best = r; min = cost;
		    if (min == 0) break;
	       }
	  }
     }

     if (best != ZERO)
	  return kill(best);

     /* Now try spilling: ignore locked registers */
     if (! spilling) {
	  min = OMEGA; 
	  spilling = TRUE;

	  for (ir = 0; ir < NREGS; ir++) {
	       reg r = (reg) ir;

	       if (member(r, s) && regs[r].r_refct < min && r != r2) {
		    best = r; min = regs[r].r_refct;
	       }
	  }

	  if (best != ZERO) {
#ifdef DEBUG
	       if (dflag > 1) 
		    printf("Spilling %s (refct=%d)\n", 
			   regs[best].r_name, regs[best].r_refct);
#endif
	       spill(best);
#ifdef DEBUG
	       if (dflag > 1)
		    printf("Refct now %d\n", regs[best].r_refct);
#endif

	       spilling = FALSE;
	       return kill(best);
	  }

     }

     panic("out of registers");
     return ZERO;
}

/* ralloc_suggest -- allocate a preferred register, or choose another */
reg ralloc_suggest(int s, reg r) {
     if (r != ZERO && member(r, s) && regs[r].r_refct % OMEGA == 0)
	  return kill(r);
     else
	  return ralloc(s);
}

/* killregs -- forget all cached values */
void killregs(void) {
     int ir;

#ifdef DEBUG
     if (dflag > 2) printf("\tKillregs\n");
#endif

     for (ir = 0; ir < NREGS; ir++) {
	  reg r = (reg) ir;
	  uncache(r);
     }
}

/* kill -- forget a register and all others related to it */
reg kill(reg r) {
     int ir;

#ifdef DEBUG
     if (dflag > 2) printf("\tKill %s\n", regs[r].r_name);
#endif

     uncache(r);

     for (ir = 0; ir < NREGS; ir++) {
	  reg r1 = (reg) ir;
	  if (cached(r1) && regs[r1].r_value.v_reg == r)
	       uncache(r1);
     }

     return r;
}

/* init_regs -- reset registers for new procedure */
void init_regs(void) {
     int ir;

     for (ir = 0; ir < NREGS; ir++) {
	  reg r = (reg) ir;
	  regs[r].r_refct = 0;
	  uncache(r);
     }
}
