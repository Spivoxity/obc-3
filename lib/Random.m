(*
 * Random.m
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

(** Pseudo-random numbers *)
MODULE Random;

(* For portability (especially to Windows) here is a standard random
   number generator.  See Knuth Vol 2, 3rd ed., p185. *)

CONST 
  m = 7FFFFFFFH; (* The Mersenne prime 2^31-1 *)
  a = 48271; q = m DIV a; r = m MOD a;

(** MAXRAND -- maximum value of a random integer *)
CONST MAXRAND* = m - 1;

VAR seed: INTEGER;

(* 
With q and r as above and 0 <= x < m, we have 

	a * (x - x MOD q) = a * q * (x DIV q) = (m - r) * (x DIV q).
So
	a * x == a * (x MOD q) - r * (x DIV q)  (mod m).  	(**)

Provided r <= q (actually q = 44488 and r = 3399) and m is prime we have 

	r * (x DIV q) <= r * ((m-1) DIV q) = r * (a + (r-1) DIV q)
		= r * a <= q * a < m

	a * (x MOD q) < a * q < m

So the RHS of (**) satisfies -m < RHS < m, and there is no risk of overflow.
*)

(** Random -- a random integer in the range [0 .. MAXRAND] *)
PROCEDURE Random*(): INTEGER;
BEGIN
  (* seed := (a * seed) MOD m *)
  seed := (a * (seed MOD q) - r * (seed DIV q)) MOD m;
  RETURN seed
END Random;

(** Uniform -- a random real uniformly distibuted in [0, 1) *)
PROCEDURE Uniform*(): REAL;
BEGIN
  RETURN SHORT(Random() / (MAXRAND + LONG(1.0)))
END Uniform;

(** Roll -- a random integer uniformly distributed in [0..n) *)
PROCEDURE Roll*(n: INTEGER): INTEGER;
BEGIN
  RETURN ENTIER(n * (Random() / (MAXRAND + LONG(1.0))))
END Roll;

(* COPY #include <time.h> 
#ifdef HAVE_GETTIMEOFDAY
#include <sys/time.h>
#endif
*)

PROCEDURE GetSeed(): INTEGER IS "Random_GetSeed";
(* CODE 
#ifdef HAVE_GETTIMEOFDAY
     struct timeval tv;
     gettimeofday(&tv, NULL);
     ob_res.i = 13 * tv.tv_sec + tv.tv_usec;
#else	
     ob_res.i = time(NULL); 
#endif
*)

(** Randomize -- seed the random generator for different results each run *)
PROCEDURE Randomize*;
  VAR dummy: INTEGER;
BEGIN
  seed := GetSeed() MOD m;
  dummy := Random();
  dummy := Random();
  dummy := Random()
END Randomize;

BEGIN
  ASSERT(r <= q);
  seed := 31415926
END Random.
