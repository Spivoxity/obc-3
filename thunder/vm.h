/*
 * vm.h
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

typedef unsigned char *code_addr;

/* Expand a macro once for each VM opcode */
#define __OP__(p) \
     p(ADD) p(ADDF) p(AND) p(BEQ) p(BEQF) p(BGEQ) p(BGEQF)	    \
     p(BGEQU) p(BGT) p(BGTF) p(BLEQ) p(BLEQF) p(BLT) p(BLTF)	    \
     p(BLTU) p(BNEQ) p(BNEQF) p(CONVIC) p(CONVIF) p(CONVIS) p(DIVF) \
     p(EQ) p(EQF) p(GEQ) p(GEQF) p(GETARG) p(GT) p(GTF) p(JUMP)	    \
     p(LDW) p(LDCU) p(LDD) p(LDSU) p(LDC) p(LDS) p(LEQ)		    \
     p(LEQF) p(LSH) p(LT) p(LTF) p(MOV)	p(MUL) p(MULF)		    \
     p(NEG) p(NEGF) p(NEQ) p(NEQF) p(NOT) p(OR) p(RET)              \
     p(RSH) p(RSHU) p(STW) p(STC) p(STD) p(STS) p(SUB)		    \
     p(SUBF) p(XOR) p(PREP) p(ARG) p(CALL) p(ZEROF)		    \
     p(BGTU) p(BLEQU) p(LDKW)                                       \
     p(ADDD) p(SUBD) p(MULD) p(DIVD) p(NEGD) p(ZEROD)               \
     p(BEQD) p(BGEQD) p(BLEQD) p(BLTD) p(BNEQD) p(BGTD)             \
     p(EQD) p(GEQD) p(LEQD) p(LTD) p(NEQD) p(GTD)                   \
     p(CONVFD) p(CONVDF) p(CONVID) p(ROR)

#define __op1__(op) op,

typedef enum { 
     __OP__(__op1__)
} operation;

/*
ADD/SUB/MUL/DIV ra, rb, rc/imm                
  -- integer arithmetic
NEG ra, rb
  -- unary minus
NOT ra rb
  -- bitwise negation
NEGF/NEGD fa, fb
  -- float or double unary minus
ADDF/SUBF/MULF/DIVF fa, fb, fc                
  -- floating point arithmetic
ADDD/SUBD/MULD/DIVD fa, fb, fc                
  -- double precision arithmetic
AND/OR/XOR ra, rb, rc/imm                     
  -- bitwise logical operations
LSH/RSH/RSHU ra, rb, rc/imm
  -- left shift, arithmetic right shift, logical right shift
BEQ/BNEQ/BLT/BLEQ/BGT/BGEQ ra, rb/imm, lab    
  -- conditional branches
BEQF/BNEQF/BLTF/BLEQF/BGTF/BGEQF fa, fb, lab  
  -- floating point conditional branches
BEQD/BNEQD/BLTD/BLEQD/BGTD/BGEQD fa, fb, lab  
  -- double precision conditional branches
BLTU/BGEQU ra, rb/imm, lab                    
  -- unsigned conditional branches
JUMP lab
  -- unconditionl branch
EQ/NEQ/LT/LEQ/GT/GEQ ra, rb, rb/imm                         
  -- comparisons with boolean result
EQF/NEQF/LTF/LEQF/GTF/GEQF ra, fb, fc
  -- float comparisons with boolean result
EQD/NEQD/LTD/LEQD/GTD/GEQD ra, fb, fc                 
  -- double comparisons with boolean result
CONVIC ra, rb
  -- convert integer to character (AND with 0xff)
CONVIS ra, rb
  -- convert integer to short (AND with 0xffff and sign extend)    
CONVIF/CONVID fa, rb
  -- convert integer to float or double
CONVFD/CONVDF fa, fb
  -- convert float/double to double/float
MOV ra/fa, rb/fb
  -- move between registers, including integer to float and vice versa
LDW/STW ra/fa, rb, imm
  -- load/store word: ra/fa := mem4[rb+imm] or mem4[rb+imm] := ra
LDC/LDS ra, rb, imm
  -- load character or short with sign extension
LDCU/LDSU ra, rb, imm
  -- load unsigned character or short
STC/STS ra, rb, imm
  -- store character or short
LDD/STD fa, rb, imm
  -- load/store double
ZEROF/ZEROD fa
  -- set float/double register to zero
LDKW ra/fa, imm
  -- load register with constant from specified address

The remaining instructions are associated with subroutine calls, and are
used only in special patterns.

GETARG ra, imm
  -- fetch argument at offset imm (must be used first in the routine)
RET
  -- return from subroutine
PREP imm
  -- prepare subroutine call with specified number of arguments
ARG ra/imm
  -- send subroutine argument from register or constant
CALL ra/imm
  -- perform subroutine call

Specifically: a procedure call is compiled by first computing the
arguments (and, for an indirect call, the procedure address) into
registers.  The call itself becomes the sequence

	PREP n
	ARG rz
	ARG ry
	ARG rx
	CALL p

where n <= 3 is the number of arguments.  The ARG instructions
(exactly n of them) specify the arguments in right-to-left order.

A subroutine is compiled by first calling vm_begin(name, n), where n
is the number of arguments.  The procedure code begins with exactly n
GETARG instructions:

	GETARG rx, 0
	GETARG ry, 1
	GETARG rz, 2

(any other instruction may destroy the arguments, so it is necessary
to fetch them into registers immediately.)  The rest of the procedure
body follows, and ends with a single RET instruction.

To return a result, the procedure code moves the result into a special
register ret, then immediately branches to the RET instruction.  Since
the ret register may be identical with one of the working registers,
any other intervening code may destroy the return value.

Note that Keiko procedures are compiled into subroutines that accept
one argument (the value of the Keiko stack pointer) and return no
result.  The parameters to a Keiko procedure are passed in a separate
memory area from the host's subroutine stack, and any result is
assigned to a global variable ob_res.


IMPLEMENTATION HINTS

It's necessary to obey the calling conventions of the host, so that
dynamically translated routines can both be called from compiled code
and call compiled subroutines.

On machines where arguments are passed in registers, it is simplest to
decide that the registers (three of them) used for outgoing arguments
are not also used as working registers.  The PREP instruction can
simply initialise an argument count, and the ARG instruction can move
an argument value into the correct register, working from right to
left.  The GETARG instructions are also implemented as moves
from register to register.

It's possible to use the argument registers for working registers too,
but then a pretty dance may be needed to move the right values into
the right registers at a call.

On machines where arguments are passed on the stack, PREP can set up
the right amount of (properly aligned) stack space and ARG can become
a push or store instruction.  GETARG is implemented as a load
instruction with a computed offset in the stack frame.
*/

/* 
Integer resisters vreg[0..nvreg) are callee-save.  
Integer registers ireg[0..nireg) are caller-save.
Floating point registers freg[0..nfreg) are caller save.
The floating-point registers are each big enough to hold a double. 

The Keiko JIT does not actually assume that the callee-save registers
are preserved across calls, though other applications might do so.
Keiko assumes nvreg+nireg >= 5.
*/

typedef struct _vmreg *vmreg;

extern const int nvreg, nireg, nfreg;
extern const vmreg vreg[], ireg[], freg[], ret, zero;

const char *vm_regname(vmreg r);

typedef struct _vmlabel *vmlabel;

vmlabel vm_newlab(void);
void vm_label(vmlabel lab);

void vm_gen0(operation op);
void vm_gen1r(operation op, vmreg a);
void vm_gen1i(operation op, int a);
void vm_gen1j(operation op, vmlabel lab);
void vm_gen2rr(operation op, vmreg a, vmreg b);
void vm_gen2ri(operation op, vmreg a, int b);
void vm_gen3rrr(operation op, vmreg a, vmreg b, vmreg c);
void vm_gen3rri(operation op, vmreg a, vmreg b, int c);
void vm_gen3rrj(operation op, vmreg a, vmreg b, vmlabel lab);
void vm_gen3rij(operation op, vmreg a, int b, vmlabel lab);

code_addr vm_begin(const char *name, int n);
void vm_end(void);

code_addr vm_jumptable(int n);
void vm_caselab(vmlabel lab);

void *vm_scratch(int size);

/* Callback to allocate pages of memory */
void *vm_alloc(int size);

extern int vm_debug;
