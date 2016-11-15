/*
 * decode.h
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

#define MAXEQ 6

/* Instructions: this table is indexed by instructions codes like I_LDLW.
   Each complex instruction can be defined as equal to a sequence of simpler
   ones: for example, LDLW n = LOCAL n / LOAD.  This is encoded by having
   the i_equiv array contain I_LOCAL|IARG, I_LOAD.  Similarly INC = 
   CONST 1 / PLUS, and this is encoded as I_CONST|ICON, 1, I_PLUS in the
   i_equiv array for I_INC.  The i_equiv array is terminated by a zero,
   and if the first element is zero then there is no expansion for the
   instruction.  Expansions may be recursive. */

struct _inst {
     const char *i_name;	/* Name of the instruction */
     int i_equiv[MAXEQ];	/* Expansion into simpler instructions */
};

#define IMASK 0xffff
#define IARG 0x10000
#define ICON 0x20000

extern struct _inst instrs[];

/* Opcodes: this table is indexed by opcodes such as K_LDLW_1 (meaning
   the LDLW instruction with a 1-byte offset). The entry for this opcode
   will have d_inst = I_LDLW, d_patt = "1" and d_len = 2.  For opcodes
   that use a pattern of "N" (opcode contains argument), the d_arg field
   contains the integer value of the argument. */

struct _decode {
     int d_inst;
     const char *d_patt;
     int d_arg;
     int d_len;
};

extern struct _decode decode[];
