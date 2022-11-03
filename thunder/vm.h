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

/* Expand a macro once for each VM opcode */
#define __OP__(p) \
     /* Integer arithmetic */                                       \
     p(MOV) p(ADD) p(SUB) p(MUL) p(NEG)                             \
     p(AND) p(OR) p(XOR) p(NOT)                                     \
     p(LSH) p(RSH) p(RSHu) p(ROR)                                   \
     /* Floating point arithmetic */                                \
     p(ADDf) p(SUBf) p(MULf) p(DIVf) p(NEGf) p(ZEROf)               \
     p(ADDd) p(SUBd) p(MULd) p(DIVd) p(NEGd) p(ZEROd)               \
     /* Integer comparisons */                                      \
     p(LT) p(LE) p(EQ) p(GE) p(GT) p(NE)                            \
     p(BLT) p(BLE) p(BEQ) p(BGE) p(BGT) p(BNE)                      \
     p(BLTu) p(BLEu) p(BGEu) p(BGTu)                                \
     /* Floating point comparisons */                               \
     p(LTf) p(LEf) p(EQf) p(GEf) p(GTf) p(NEf)                      \
     p(BLTf) p(BLEf) p(BEQf) p(BGEf) p(BGTf)                        \
     p(BNLTf) p(BNLEf) p(BNEf) p(BNGEf) p(BNGTf)                    \
     p(LTd) p(LEd) p(EQd) p(GEd) p(GTd) p(NEd)                      \
     p(BLTd) p(BLEd) p(BEQd) p(BGEd) p(BGTd)                        \
     p(BNLTd) p(BNLEd) p(BNEd) p(BNGEd) p(BNGTd)                    \
     /* Conversions */                                              \
     p(CONVif) p(CONVfi) p(CONVdi) p(CONVfd) p(CONVdf)              \
     p(CONVid) p(CONVis)                                            \
     /* Load and store */                                           \
     p(LDB) p(LDBu) p(LDS) p(LDSu) p(LDW) p(LDQ)                    \
     p(STW) p(STB) p(STQ) p(STS)                                    \
     /* Call and jump */                                            \
     p(PREP) p(ARG) p(CALL) p(GETARG) p(JUMP)                       \
     /* 64-bit arithmetic (M64X32 only) */                          \
     p(ADDq) p(SUBq) p(MULq) p(NEGq) p(MOVq) p(SXTq)                \
     p(LTq) p(LEq) p(EQq) p(GEq) p(GTq) p(NEq)                      \
     p(BLTq) p(BLEq) p(BEQq) p(BGEq) p(BGTq) p(BNEq)

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
NEGf/NEGd fa, fb
  -- float or double unary minus
ADDf/SUBf/MULf/DIVf fa, fb, fc                
  -- floating point arithmetic
ADDd/SUBd/MULd/DIVd fa, fb, fc                
  -- double precision arithmetic
AND/OR/XOR ra, rb, rc/imm                     
  -- bitwise logical operations
LSH/RSH/RSHu ra, rb, rc/imm
  -- left shift, arithmetic right shift, logical right shift
BEQ/BNE/BLT/BLE/BGT/BGE ra, rb/imm, lab    
  -- conditional branches
BEQf/BNEf/BLTf/BLEf/BGTf/BGEf fa, fb, lab  
  -- floating point conditional branches
BEQd/BNEd/BLTd/BLEd/BGTd/BGEd fa, fb, lab  
  -- double precision conditional branches
BLTu/BGEu ra, rb/imm, lab                    
  -- unsigned conditional branches
JUMP lab
  -- unconditional branch
EQ/NE/LT/LE/GT/GE ra, rb, rb/imm                         
  -- comparisons with boolean result
EQf/NEf/LTf/LEf/GTf/GEf ra, fb, fc
  -- float comparisons with boolean result
EQd/NEd/LTd/LEd/GTd/GEd ra, fb, fc                 
  -- double comparisons with boolean result
CONVif/CONVid fa, rb
  -- convert integer to float or double
CONVfd/CONVdf fa, fb
  -- convert float/double to double/float
MOV ra/fa, rb/fb
  -- move between registers, including integer to float and vice versa
LDW/STW ra/fa, rb, imm
  -- load/store word: ra/fa := mem4[rb+imm] or mem4[rb+imm] := ra
LDB/LDS ra, rb, imm
  -- load byte or short with sign extension
LDBu/LDSu ra, rb, imm
  -- load unsigned byte or short
STC/STS ra, rb, imm
  -- store character or short
LDQ/STQ ra/fa, rb, imm
  -- load/store double
ZEROf/ZEROd fa
  -- set float/double register to zero

The remaining instructions are associated with subroutine calls, and are
used only in special patterns.

GETARG ra, imm
  -- fetch argument at offset imm (must be used first in the routine)
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
to fetch them into registers immediately.)

To return a result, the procedure code moves the result into a special
register ret, then immediately branches to the end of the procedure.
Since the ret register may be identical with one of the working
registers, any other intervening code may destroy the return value.

Note that Keiko procedures are compiled into subroutines that accept
one argument (the value of the Keiko stack pointer) and return no
result.  The parameters to a Keiko procedure are passed in a separate
memory area from the host's subroutine stack, and any result is
also returned in the same area.


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
Integer resisters ireg[0..nvreg) are callee-save.  
Integer registers ireg[nvreg..nireg) are caller-save.
Floating point registers freg[0..nfreg) are caller save.
The floating-point registers are each big enough to hold a double. 

The Keiko JIT does not actually assume that the callee-save registers
are preserved across calls, though other applications might do so.
Keiko assumes nvreg+nireg >= 5.
*/

typedef struct _vmreg *vmreg;

extern const int vm_nvreg, vm_nireg, vm_nfreg;
extern const vmreg vm_ireg[], vm_freg[], vm_ret, vm_base;

const char *vm_regname(vmreg r);

typedef struct _vmlabel *vmlabel;

vmlabel vm_newlab(void);
void vm_label(vmlabel lab);

void vm_gen0(operation op);
void vm_gen1r(operation op, vmreg a);
void vm_gen1i(operation op, int a);
void vm_gen1a(operation op, void *a);
void vm_gen1j(operation op, vmlabel lab);
void vm_gen2rr(operation op, vmreg a, vmreg b);
void vm_gen2ri(operation op, vmreg a, int b);
void vm_gen2rj(operation op, vmreg a, vmlabel b);
void vm_gen3rrr(operation op, vmreg a, vmreg b, vmreg c);
void vm_gen3rri(operation op, vmreg a, vmreg b, int c);
void vm_gen3rrj(operation op, vmreg a, vmreg b, vmlabel lab);
void vm_gen3rij(operation op, vmreg a, int b, vmlabel lab);
void vm_gen4rrrs(operation op, vmreg a, vmreg b, vmreg c, int s);

int vm_addr(void *x);

unsigned vm_begin_locals(const char *name, int n, int locs);
#define vm_begin(name, n) vm_begin_locals(name, n, 0);
void vm_end(void);

int vm_jumptable(int n);
void vm_caselab(vmlabel lab);

void *vm_scratch(int size);

typedef void (*funptr)(void);

int vm_wrap(funptr fun);

void *vm_literal_align(int n, int almt);
#define vm_literal(n) vm_literal_align(n, 4);

funptr vm_func(int fun);

/* vm_procsize -- size of last procedure */
int vm_procsize(void);

/* Callback to allocate pages of memory */
void *vm_alloc(int size);

/* Level of debug printing, if compiled for debugging */
extern int vm_debug;

/* Whether to suppress addresses in output */
extern int vm_aflag;


/* Fancy _Generic stuff to provide overloading of vm_gen */

#define _SELECT(p, q, r, s, t, u, ...) u

#define vm_gen(...) \
     _SELECT(__VA_ARGS__, vm_gen4, vm_gen3, vm_gen2, vm_gen1)(__VA_ARGS__)

#define intcases(r) int: r, unsigned: r, char: r

#define vm_gen1(op, a)                                                  \
     _Generic(a, default: vm_gen1r, intcases(vm_gen1i),                 \
              vmlabel: vm_gen1j, void *: vm_gen1a)(op, a)

#define vm_gen2(op, a, b)                                               \
     _Generic(b, default: vm_gen2rr, intcases(vm_gen2ri),               \
              vmlabel: vm_gen2rj)(op, a, b)

#define vm_gen3(op, a, b, c)                                            \
     _Generic(c, default: vm_gen3rrr, intcases(vm_gen3rri),             \
              vmlabel: _Generic(b, default: vm_gen3rrj,                 \
                                intcases(vm_gen3rij)))(op, a, b, c)

#define vm_gen4(op, a, b, c, d)                 			\
     vm_gen4rrrs(op, a, b, c, d)
