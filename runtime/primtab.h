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
#include <math.h>
#include <ctype.h>

/* Types for each kind of argument */
typedef int type_C, type_I;
typedef longint type_L;
typedef float type_F;
typedef double type_D;
typedef void *type_P, *type_Q, *type_X;
typedef void type_V;

/* Size of each kind in argument words */
#define size_C 1
#define size_I 1
#define size_F 1
#define size_P 1
#define size_L 2
#define size_D 2
#define size_X 2
#define size_Q 2

/* How to fetch each kind of argument */
#define arg_I(j)  bp[HEAD+j].i
#define arg_C(j)  align_byte(bp[HEAD+j].i)
#define arg_L(j)  get_long(&bp[HEAD+j])
#define arg_F(j)  bp[HEAD+j].f
#define arg_D(j)  get_double(&bp[HEAD+j])
#define arg_P(j)  pointer(bp[HEAD+j])
#define arg_X(j)  pointer(bp[HEAD+j])
#define arg_Q(j)  ptrcast(void, get_long(&bp[HEAD+j]))

/* How to return each kind of result via ob_res */
#define res_I(v)  ob_res.i = v
#define res_C(v)  ob_res.i = v
#define res_F(v)  ob_res.f = v
#define res_P(v)  ob_res.a = address(v)
#define res_L(v)  put_long(&ob_res, v)
#define res_D(v)  put_double(&ob_res, v)
#define res_Q(v)  put_long(&ob_res, (ptrtype) v)
#define res_V(v)  v

/* Three kinds of primitive: 
   DIRECT   -- defined by a function "void prim(value *bp)"
   WRAPPER  -- defined by an foreign function prim that declared in one of 
               the included header files.  We generate a wrapper P_prim.
   INDIRECT -- defined by an internal function in some library module, with
               a natural type.  We generate a wrapper that includes a
               declaration of the function. 

   Call WRAPPER(name, res, a1, a2, ..., an) where res and a1, ..., an are
   type letters for the result and arguments. */

#define DIRECT(name)  void name(value *bp);
#define WRAPPER(...)  WRAP(_WRAP, 0, __VA_ARGS__)
#define INDIRECT(...) WRAP(_INDIR, 0, __VA_ARGS__)

/* WRAP(mac, base, name, res, a1, ..., an) is

   mac(name, res, 
       (type_a1, ..., type_an), 
       (arg_a1(base), arg_a2(base+s1), arg_a3(base+s1+s2), ..., 
           arg_an(base+s1+s2+...s(n-1))))
   
   where si = size_ai. */

#define WRAP(mac, base, ...)                          \
     SELECT(__VA_ARGS__, WRAP6, WRAP5, WRAP4, WRAP3, \
                 WRAP2, WRAP1, WRAP0)(mac, base, __VA_ARGS__)

#define SELECT(n, r, a1, a2, a3, a4, a5, a6, t, ...) t

#define WRAP0(mac, base, name, res)              \
     mac(name, res, (void), ())
#define WRAP1(mac, base, name, res, a1)          \
     mac(name, res, (type_##a1), (arg_##a1(base)))
#define WRAP2(mac, base, name, res, a1, a2)                      \
     mac(name, res, (type_##a1, type_##a2), (args2(base, a1, a2)))
#define WRAP3(mac, base, name, res, a1, a2, a3)  \
     mac(name, res, \
         (type_##a1, type_##a2, type_##a3), \
         (args3(base, a1, a2, a3)))
#define WRAP4(mac, base, name, res, a1, a2, a3, a4)      \
     mac(name, res, \
         (type_##a1, type_##a2, type_##a3, type_##a4), \
         (args4(base, a1, a2, a3, a4)))
#define WRAP5(mac, base, name, res, a1, a2, a3, a4, a5)  \
     mac(name, res, \
         (type_##a1, type_##a2, type_##a3, type_##a4, type_##a5), \
         (args5(base, a1, a2, a3, a4, a5)))
#define WRAP6(mac, base, name, res, a1, a2, a3, a4, a5, a6)      \
     mac(name, res, \
         (type_##a1, type_##a2, type_##a3, type_##a4, type_##a5, type_##a6), \
         (args6(base, a1, a2, a3, a4, a5, a6)))

#define args2(j, a1, a2) \
     arg_##a1(j), arg_##a2(j+size_##a1)
#define args3(j, a1, a2, a3) \
     arg_##a1(j), args2(j+size_##a1, a2, a3)
#define args4(j, a1, a2, a3, a4) \
     arg_##a1(j), args3(j+size_##a1, a2, a3, a4)
#define args5(j, a1, a2, a3, a4, a5) \
     arg_##a1(j), args4(j+size_##a1, a2, a3, a4, a5)
#define args6(j, a1, a2, a3, a4, a5, a6) \
     arg_##a1(j), args5(j+size_##a1, a2, a3, a4, a5, a6)

/* How to generate a wrapper function with (_INDIR) or without (_WRAP)
   a declaration of the function being wrapped. */
#define _WRAP(name, res, atypes, args) \
     __WRAP(, name, res, args)
#define _INDIR(name, res, atypes, args) \
     __WRAP(type_##res name atypes;, name, res, args)
#define __WRAP(decl, name, res, args) \
     void P_##name(value *bp) { decl FPINIT; res_##res(name args); }

#define WRAPPERS(prims) prims(DIRECT, INDIRECT, WRAPPER)

/* How to generate entries in the primitive table */
#define DPRIM(name, ...)  { #name, name },
#define IPRIM(name, ...)  DPRIM(P_##name)

#define TABLE(prims)              \
     struct primdef primtab[] = { \
          prims(DPRIM, IPRIM, IPRIM) \
          { NULL, NULL }          \
     };

/* If dynamic linking is enabled, we don't need a static table of
   primitives; if not, then we make a table and dltrap(dynlink.c)
   will search it. Note that we can have statically generated wrappers
   for speed even if FFI is available. */

#ifdef DYNLINK
#define PRIMTAB(prims) WRAPPERS(prims)
#else
#define PRIMTAB(prims) WRAPPERS(prims) TABLE(prims)
#endif

/* Variation for the compilers course with offset to compensate for
   dummy static link */

#define PWRAPPER(...)  WRAP(_WRAP, 1, __VA_ARGS__)
#define PINDIRECT(...) WRAP(_INDIR, 1, __VA_ARGS__)
#define PWRAPPERS(prims) prims(DIRECT, PINDIRECT, PWRAPPER)

#ifdef DYNLINK
#define PPRIMTAB(prims) PWRAPPERS(prims)
#else
#define PPRIMTAB(prims) PWRAPPERS(prims) TABLE(prims)
#endif

