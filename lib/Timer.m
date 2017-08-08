(*
 * Timer.m
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
 *)

(** An execution timer *)
MODULE Timer;

(** Now -- elapsed time in milliseconds since the program started *)
PROCEDURE Now*(): INTEGER IS "Now";

END Timer.

--CODE--

/* Care needed: CLOCKS_PER_SEC might be 50 or it might (on a
   POSIX system) be 1000000.  So we represent 1000t as a three-digit
   number in base 1000, and use high-school division. */

#include <time.h>

int Now(void) {
#define B 1000
#define M CLOCKS_PER_SEC

     clock_t t = clock();
     unsigned t1 = t/B;
     unsigned t2 = (t1%M)*B + t%B;
     unsigned t3 = (t2%M)*B;
     return ((t1/M)*B + (t2/M))*B + t3/M; 
}

/* Using y(x/y) + x%y = x repeatedly:

   Mr + t3%M = B^2M(t1/M) + BM(t2/M) + M(t3/M) + t3%M
     = B^2M(t1/M) + BM(t2/M) + B(t2%M)
     = B^2M(t1/M) + Bt2
     = B^2M(t1/M) + B^2(t1%M) + B(t%B)
     = B^2t1 + B(t%B)
     = B^2(t/B) + B(t%B)
     = Bt 

   But 0 <= t3%M < M, so r = floor(Bt/M) */

