MODULE tSplit;

IMPORT Out, SYSTEM;

PROCEDURE Optimal(VAR a: ARRAY OF INTEGER; n: INTEGER): INTEGER;

  (* Compute the optimal split of a[0..n) into subsequences b[0..r),
     w[0..n-r), minimising cost(b) + cost(w), where
     cost(b[0..r)) = sum_{1 <= i < r} (b[i] - b[i-1])^2 *)

  VAR x, y, k, r: INTEGER;
    c: POINTER TO ARRAY OF ARRAY OF INTEGER;
    u: POINTER TO ARRAY OF INTEGER;

  PROCEDURE d(i, j: INTEGER): INTEGER;
    VAR t: INTEGER;
  BEGIN
    t := a[j] - a[i];
    RETURN t*t
  END d;

(* We consider colouring each element of a either black (if it is in b)
or white (in w).  Array element c[k,r] for 0 <= k < r < n contains
the best cost for colouring a[0..r+1) if a[k] is the last element
coloured white and a[r] is coloured black.  This is equal to the same
thing with white and black reversed.  Element u[r] contains
the cost of a[0..r+1), which we assume to be coloured all black or all
white.

We have u[r+1] = u[r] + d(r, r+1) for r >= 0 with u[0] = 0.

When we add a new element a[r] at the right, we must consider separately
the cases where the new element is coloured the same as its left
neighbour and where it is different.  Let's say the neighbour is
black.  If the new elemant is also black, then the situations before
and after the addition are described by c[k,r] and c[k, r+1] for
some k.  For k < r we have

     c[k, r+1] = c[k,r] + d(r-1, r).

If the new element is white, then the best cost is determined by choosing
an appropriate predecessor element that is white, or choosing all predecessor
elements to be black.  This gives the equation,

    c[r, r+1] = min(u[r], min_{0 <= k < r} c[k,r] + d(k,r))

After computing u and c, the minimum cost is given by
  min(u[n-1], min_{0 <= k < n} c[k, n-1])
*)
BEGIN
  NEW(c, n-1, n); NEW(u, n);
  u[0] := 0;

  FOR r := 0 TO n-2 DO
    u[r+1] := u[r] + d(r, r+1);
    x := u[r];
    FOR k := 0 TO r-1 DO
      c[k, r+1] := c[k,r] + d(r, r+1);
      y := c[k, r] + d(k, r+1);
      IF y < x THEN x := y END
    END;
    c[r, r+1] := x
  END;

  y := u[n-1];
  FOR k := 0 TO n-2 DO
    IF c[k, n-1] < y THEN y := c[k, n-1] END
  END;
  RETURN y
END Optimal;

PROCEDURE Brute(VAR a: ARRAY OF INTEGER; n: INTEGER): INTEGER;

  PROCEDURE d(i, j: INTEGER): INTEGER;
    VAR t: INTEGER;
  BEGIN
    t := a[j] - a[i];
    RETURN t*t
  END d;

  PROCEDURE Cost(T: SET): INTEGER;
    VAR j, u, v, c: INTEGER;
  BEGIN
    u := -1; v := -1; c := 0;
    FOR j := 0 TO n-1 DO
      IF j IN S THEN
        IF u >= 0 THEN c := c + d(u, j) END;
        u := j
      ELSE
        IF v >= 0 THEN c := c + d(v, j) END;
        v := j
      END
    END;
    RETURN c
  END Cost;

  VAR j, x, y: INTEGER; S: SET;

BEGIN
  x := Cost({});
  j := 0; 
  LOOP
    INC(j); S := SYSTEM.VAL(SET, j);
    IF n IN S THEN EXIT END;
    y := Cost(S);
    IF y < x THEN x := y END
  END;
  RETURN x
END Brute;

VAR A: ARRAY 10 OF INTEGER;

BEGIN
  A[0] := 3; A[1] := 1; A[2] := 4; A[3] := 1; A[4] := 5;
  A[5] := 9; A[6] := 2; A[7] := 6; A[8] := 5; A[9] := 3;

  Out.Int(Optimal(A, 10), 0); Out.Ln;
  Out.Int(Brute(A, 10), 0); Out.Ln
END tSplit.

(*<<
30
30
>>*)

(*[[
!! (SYMFILE #tSplit STAMP #tSplit.%main 1 #tSplit.m)
!! (CHKSUM STAMP)
!! 
MODULE tSplit STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tSplit.%1.d 8 4 0
SAVELINK
!   PROCEDURE d(i, j: INTEGER): INTEGER;
!     t := a[j] - a[i];
LDLW -4
LDNW 12
LDLW 16
LDLW -4
LDNW 16
BOUND 18
LDIW
LDLW -4
LDNW 12
LDLW 12
LDLW -4
LDNW 16
BOUND 18
LDIW
MINUS
STLW -8
!     RETURN t*t
LDLW -8
LDLW -8
TIMES
RETURN
END

PROC tSplit.Optimal 36 6 0x00101801
! PROCEDURE Optimal(VAR a: ARRAY OF INTEGER; n: INTEGER): INTEGER;
!   NEW(c, n-1, n); NEW(u, n);
LDLW 20
LDLW 20
DEC
CONST 2
CONST 4
CONST 0
GLOBAL NEWFLEX
CALLW 5
STLW -20
LDLW 20
CONST 1
CONST 4
CONST 0
GLOBAL NEWFLEX
CALLW 4
STLW -24
!   u[0] := 0;
CONST 0
LDLW -24
NCHECK 52
CONST 0
DUP 1
LDNW -4
LDNW 4
BOUND 52
STIW
!   FOR r := 0 TO n-2 DO
LDLW 20
CONST 2
MINUS
STLW -28
CONST 0
STLW -16
LABEL L4
LDLW -16
LDLW -28
JGT L5
!     u[r+1] := u[r] + d(r, r+1);
LDLW -24
NCHECK 55
LDLW -16
DUP 1
LDNW -4
LDNW 4
BOUND 55
LDIW
LDLW -16
INC
LDLW -16
LOCAL 0
STATLINK
GLOBAL tSplit.%1.d
CALLW 2
PLUS
LDLW -24
NCHECK 55
LDLW -16
INC
DUP 1
LDNW -4
LDNW 4
BOUND 55
STIW
!     x := u[r];
LDLW -24
NCHECK 56
LDLW -16
DUP 1
LDNW -4
LDNW 4
BOUND 56
LDIW
STLW -4
!     FOR k := 0 TO r-1 DO
LDLW -16
DEC
STLW -32
CONST 0
STLW -12
LABEL L6
LDLW -12
LDLW -32
JGT L7
!       c[k, r+1] := c[k,r] + d(r, r+1);
LDLW -20
NCHECK 58
LDLW -12
DUP 1
LDNW -4
LDNW 4
BOUND 58
DUP 1
LDNW -4
LDNW 8
TIMES
LDLW -16
DUP 2
LDNW -4
LDNW 8
BOUND 58
PLUS
LDIW
LDLW -16
INC
LDLW -16
LOCAL 0
STATLINK
GLOBAL tSplit.%1.d
CALLW 2
PLUS
LDLW -20
NCHECK 58
LDLW -12
DUP 1
LDNW -4
LDNW 4
BOUND 58
DUP 1
LDNW -4
LDNW 8
TIMES
LDLW -16
INC
DUP 2
LDNW -4
LDNW 8
BOUND 58
PLUS
STIW
!       y := c[k, r] + d(k, r+1);
LDLW -20
NCHECK 59
LDLW -12
DUP 1
LDNW -4
LDNW 4
BOUND 59
DUP 1
LDNW -4
LDNW 8
TIMES
LDLW -16
DUP 2
LDNW -4
LDNW 8
BOUND 59
PLUS
LDIW
LDLW -16
INC
LDLW -12
LOCAL 0
STATLINK
GLOBAL tSplit.%1.d
CALLW 2
PLUS
STLW -8
!       IF y < x THEN x := y END
LDLW -8
LDLW -4
JGEQ L10
LDLW -8
STLW -4
LABEL L10
!     FOR k := 0 TO r-1 DO
INCL -12
JUMP L6
LABEL L7
!     c[r, r+1] := x
LDLW -4
LDLW -20
NCHECK 62
LDLW -16
DUP 1
LDNW -4
LDNW 4
BOUND 62
DUP 1
LDNW -4
LDNW 8
TIMES
LDLW -16
INC
DUP 2
LDNW -4
LDNW 8
BOUND 62
PLUS
STIW
!   FOR r := 0 TO n-2 DO
INCL -16
JUMP L4
LABEL L5
!   y := u[n-1];
LDLW -24
NCHECK 65
LDLW 20
DEC
DUP 1
LDNW -4
LDNW 4
BOUND 65
LDIW
STLW -8
!   FOR k := 0 TO n-2 DO
LDLW 20
CONST 2
MINUS
STLW -36
CONST 0
STLW -12
LABEL L11
LDLW -12
LDLW -36
JGT L12
!     IF c[k, n-1] < y THEN y := c[k, n-1] END
LDLW -20
NCHECK 67
LDLW -12
DUP 1
LDNW -4
LDNW 4
BOUND 67
DUP 1
LDNW -4
LDNW 8
TIMES
LDLW 20
DEC
DUP 2
LDNW -4
LDNW 8
BOUND 67
PLUS
LDIW
LDLW -8
JGEQ L15
LDLW -20
NCHECK 67
LDLW -12
DUP 1
LDNW -4
LDNW 4
BOUND 67
DUP 1
LDNW -4
LDNW 8
TIMES
LDLW 20
DEC
DUP 2
LDNW -4
LDNW 8
BOUND 67
PLUS
LDIW
STLW -8
LABEL L15
!   FOR k := 0 TO n-2 DO
INCL -12
JUMP L11
LABEL L12
!   RETURN y
LDLW -8
RETURN
END

PROC tSplit.%2.d 8 4 0
SAVELINK
!   PROCEDURE d(i, j: INTEGER): INTEGER;
!     t := a[j] - a[i];
LDLW -4
LDNW 12
LDLW 16
LDLW -4
LDNW 16
BOUND 77
LDIW
LDLW -4
LDNW 12
LDLW 12
LDLW -4
LDNW 16
BOUND 77
LDIW
MINUS
STLW -8
!     RETURN t*t
LDLW -8
LDLW -8
TIMES
RETURN
END

PROC tSplit.%3.Cost 24 4 0
SAVELINK
!   PROCEDURE Cost(T: SET): INTEGER;
!     u := -1; v := -1; c := 0;
CONST -1
STLW -12
CONST -1
STLW -16
CONST 0
STLW -20
!     FOR j := 0 TO n-1 DO
LDLW -4
LDNW 20
DEC
STLW -24
CONST 0
STLW -8
LABEL L16
LDLW -8
LDLW -24
JGT L17
!       IF j IN S THEN
LDLW -4
LDNW -16
CONST 1
LDLW -8
CONST 32
BOUND 86
LSL
BITAND
JEQZ L23
!         IF u >= 0 THEN c := c + d(u, j) END;
LDLW -12
JLTZ L26
LDLW -20
LDLW -8
LDLW -12
LDLW -4
STATLINK
GLOBAL tSplit.%2.d
CALLW 2
PLUS
STLW -20
LABEL L26
!         u := j
LDLW -8
STLW -12
JUMP L18
LABEL L23
!         IF v >= 0 THEN c := c + d(v, j) END;
LDLW -16
JLTZ L21
LDLW -20
LDLW -8
LDLW -16
LDLW -4
STATLINK
GLOBAL tSplit.%2.d
CALLW 2
PLUS
STLW -20
LABEL L21
!         v := j
LDLW -8
STLW -16
LABEL L18
!     FOR j := 0 TO n-1 DO
INCL -8
JUMP L16
LABEL L17
!     RETURN c
LDLW -20
RETURN
END

PROC tSplit.Brute 16 4 0x00100001
! PROCEDURE Brute(VAR a: ARRAY OF INTEGER; n: INTEGER): INTEGER;
!   x := Cost({});
CONST 0
LOCAL 0
STATLINK
GLOBAL tSplit.%3.Cost
CALLW 1
STLW -8
!   j := 0; 
CONST 0
STLW -4
LABEL L27
!     INC(j); S := SYSTEM.VAL(SET, j);
INCL -4
LDLW -4
STLW -16
!     IF n IN S THEN EXIT END;
LDLW -16
CONST 1
LDLW 20
CONST 32
BOUND 104
LSL
BITAND
JNEQZ L28
!     y := Cost(S);
LDLW -16
LOCAL 0
STATLINK
GLOBAL tSplit.%3.Cost
CALLW 1
STLW -12
!     IF y < x THEN x := y END
LDLW -12
LDLW -8
JGEQ L27
LDLW -12
STLW -8
JUMP L27
LABEL L28
!   RETURN x
LDLW -8
RETURN
END

PROC tSplit.%main 0 5 0
!   A[0] := 3; A[1] := 1; A[2] := 4; A[3] := 1; A[4] := 5;
CONST 3
STGW tSplit.A
CONST 1
GLOBAL tSplit.A
STNW 4
CONST 4
GLOBAL tSplit.A
STNW 8
CONST 1
GLOBAL tSplit.A
STNW 12
CONST 5
GLOBAL tSplit.A
STNW 16
!   A[5] := 9; A[6] := 2; A[7] := 6; A[8] := 5; A[9] := 3;
CONST 9
GLOBAL tSplit.A
STNW 20
CONST 2
GLOBAL tSplit.A
STNW 24
CONST 6
GLOBAL tSplit.A
STNW 28
CONST 5
GLOBAL tSplit.A
STNW 32
CONST 3
GLOBAL tSplit.A
STNW 36
!   Out.Int(Optimal(A, 10), 0); Out.Ln;
CONST 0
CONST 10
CONST 10
GLOBAL tSplit.A
GLOBAL tSplit.Optimal
CALLW 3
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
!   Out.Int(Brute(A, 10), 0); Out.Ln
CONST 0
CONST 10
CONST 10
GLOBAL tSplit.A
GLOBAL tSplit.Brute
CALLW 3
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tSplit.A 40

! End of file
]]*)
