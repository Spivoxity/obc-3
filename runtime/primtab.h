/*
 * primtab.h
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

/* Types for arguments */
typedef int type_C, type_S, type_I;
typedef longint type_L;
typedef float type_F;
typedef double type_D;
typedef void *type_P, *type_Q, *type_X;
typedef void type_V;

/* How to fetch each kind of argument */
#define arg_C(j)  align_byte(bp[HEAD+j].i)
#define arg_I(j)  bp[HEAD+j].i
#define arg_F(j)  bp[HEAD+j].f
#define arg_P(j)  pointer(bp[HEAD+j])
#define arg_L(j)  get_long(&bp[HEAD+j])
#define arg_D(j)  get_double(&bp[HEAD+j])
#define arg_X(j)  pointer(bp[HEAD+j])
#define arg_Q(j)  ptrcast(void, get_long(&bp[HEAD+j]))

/* How to return each kind of result via ob_res */
#define res_C(v)  ob_res.i = v
#define res_I(v)  ob_res.i = v
#define res_F(v)  ob_res.f = v
#define res_P(v)  ob_res.a = address(v)
#define res_L(v)  put_long(&ob_res, v)
#define res_D(v)  put_double(&ob_res, v)
#define res_Q(v)  put_long(&ob_res, (ptrtype) v)
#define res_V(v)  v
