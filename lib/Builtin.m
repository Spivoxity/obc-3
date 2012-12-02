(*
 * Builtin.m
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
 *)

MODULE Builtin;

(* COPY #include <ctype.h> *)

PROCEDURE HALT(n: INTEGER) IS "HALT";
(* CODE xmain_exit(args[0].i); *)

PROCEDURE NEW(VAR p: INTEGER; desc, size: INTEGER) IS "NEW";
(* CODE 
     value *desc = args[1].p, *block;
     int size = args[2].i;

     ( *args[0].p ).p = NULL;		/* Free old storage */
     block = (value * ) gc_alloc(desc, size, bp);
     ( *args[0].p ).p = block;
*)

(*
Layout for flexible arrays:

	desc

q:	element 0
	...
	element n-1

desc:	map			desc = q + align(n*elsize)
	dim 1
        ...
	dim k

map:	-4
	desc-q
[	GC_REPEAT		if elmap != 0
	0
	n
	elsize
	GC_MAP
	elmap
	GC_END		]
	GC_END

size = align(n*elsize) + 4*k + 16 + (28 if elmap != 0)

Parameters NEWFLEX(VAR p: INTEGER; elmap, elsize, k: INTEGER;
			dim_0, ..., dim_{k-1}: INTEGER)
*)

PROCEDURE NEWFLEX(VAR p: INTEGER; eldesc, elsize, k: INTEGER) IS "NEWFLEX";
(* CODE 
     value *elmap = args[1].p;
     int elsize = args[2].i;
     int k = args[3].i;
     value *dim = &args[4];		/* Array of bounds */

     int size, arsize, i, n;
     value *q, *desc, *map;

     ( *args[0].p ).p = NULL;		/* Free old storage */

     /* Compute no. of elements */
     n = 1;
     for (i = 0; i < k; i++) n *= dim[i].i;
     if (n < 0) liberror("allocating negative size");
     arsize = align(n * elsize, 4); 
     if (n == 0) elmap = NULL;

     /* Allocate the space */
     size = arsize + 4*k + 16;
     if (elmap != NULL) size += 28;

     q = (value * ) gc_alloc(NULL, size, bp);

     desc = q + arsize/4;
     map = desc + k + 1;
     q[-1].p = desc;

     /* Fill in the descriptor */
     desc[DESC_MAP].p = map;
     dim = &args[4];			/* In case our stack moved */
     for (i = 0; i < k; i++) desc[DESC_BOUND+i].i = dim[i].i;

     /* Fill in the map */
     map[0].i = -4;
     map[1].i = 4 * (desc-q);
     if (elmap == NULL)
          map[2].i = GC_END;
     else {
          map[2].i = GC_REPEAT;
	  map[3].i = 0;
	  map[4].i = n;
	  map[5].i = elsize;
	  map[6].i = GC_MAP;
	  map[7].p = elmap;
	  map[8].i = GC_END;
	  map[9].i = GC_END;
     }

     ( *args[0].p ).p = q; 
*)

PROCEDURE COMPARE(s, t: ARRAY OF CHAR): INTEGER IS "COMPARE";
(* CODE 
     uchar *s1 = args[0].x, *s2 = args[2].x;
     int i = 0, n = min(args[1].i, args[3].i);

     while (i < n && s1[i] != '\0' && s1[i] == s2[i]) i++;
     if (i >= n) liberror("string is not null-terminated");
     ob_res.i = s1[i] - s2[i]; 
*)

PROCEDURE CAP(c: CHAR): CHAR IS "CAP";
(* CODE ob_res.i = toupper(align_byte(args[0].i)); *)

PROCEDURE COPY(VAR s, t: ARRAY OF CHAR) IS "COPY";
(* CODE obcopy((char * ) args[2].x, (char * ) args[0].x, 
					min(args[1].i, args[3].i)); *)

PROCEDURE MOVE(src, dest: INTEGER; n: INTEGER) IS "MOVE";
(* CODE memmove(args[1].x, args[0].x, args[2].i); *)

PROCEDURE FLEXASSIGN(elsize, dim: INTEGER (* ... *)) IS "FLEXASSIGN";
(* CODE
     int size = args[0].i;
     int dim = args[1].i;
     uchar *src = args[2].x, *dst = args[dim+3].x;
     value *sbound = &args[3], *dbound = &args[dim+4];
     int i;

     for (i = 0; i < dim; i++) {
	  int b = sbound[i].i;
	  if (b != dbound[i].i) 
	       liberror("bound mismatch in open array assignment");
	  size *= b;
     }

     memmove(dst, src, size);
 *)

PROCEDURE ABSINT(x: INTEGER): INTEGER;
BEGIN
  IF x >= 0 THEN RETURN x ELSE RETURN -x END
END ABSINT;

PROCEDURE ABSREAL(x: REAL): REAL;
BEGIN
  IF x >= 0 THEN RETURN x ELSE RETURN -x END
END ABSREAL;

PROCEDURE ABSLONG(x: LONGREAL): LONGREAL;
BEGIN
  IF x >= 0 THEN RETURN x ELSE RETURN -x END
END ABSLONG;

PROCEDURE INTREAL(x: REAL): INTEGER IS "INTREAL";
(* CODE ob_res.i = (int) args[0].f; *)

PROCEDURE INTLONG(x: REAL): INTEGER IS "INTLONG";
(* CODE put_long(&ob_dres, (longint) get_double(&args[0])); *)

PROCEDURE INCLONG(VAR x: LONGINT; n: LONGINT);
BEGIN
  x := x + n
END INCLONG;

PROCEDURE DECLONG(VAR x: LONGINT; n: LONGINT);
BEGIN
  x := x - n
END DECLONG;

PROCEDURE ASH(a, b: INTEGER): INTEGER;
BEGIN
  IF b >= 0 THEN
    RETURN LSL(a, b)
  ELSE
    RETURN ASR(a, -b)
  END
END ASH;

END Builtin.
