MODULE tDomino;

IMPORT Out, SYSTEM;

(*<<
12988816
[4458 calls]
[3550 arguments]
[650 results were zero]
>>*)

(*

R(i, a, b) is the number of ways of tiling this shape with dominoes
covering exactly the 1's:

	1   1   ...  1 |  1   a[0]
                       | 
	1   1   ...  1 |  1   a[1]
	.   .        . |  .     .
	.   .        . |  .     .
	.   .        . |  .     .
        1   1   ...  1 |  1   a[j-1]
                       |
	1   1   ...  1 |b[j]    0
                       |
        1   1   ...  1 |b[j+1]  0
        .   .        . |  .
        .   .        . |  .
        .   .        . |  .
        1   1   ...  1 |b[n-1]  0

        <----- i columns ---->

We use a fictitious column of 1's at the left to get things started,
preventing dominoes from sticking out at the left.  It's just a convention!

Recurrence:

	R(i, a0, b)	= R(i, a, 1b)
	R(i, a01, b)	= R(i, a, 10b)
	R(i, a11, b)	= R(i, a, 11b) + R(i, a1, 0b)

	R(i, 1, b)	= R(i, e, 0b)
	R(i+1, e, b)	= R(i, b, e)
        R(0, e, b) 	= I(b = 11...1)

where e is the empty sequence.

[We can show by induction on #a that R(0, a, b) = 0 unless b contains
only 1's.  Then we put H(a) = R(0, a, 11...1) and derive

	H(a0) 		= H(a)
	H(a01) 		= 0
	H(a11) 		= H(a)
	H(e) 		= 1,

a recurrence that gives the number of ways of tiling a single column,
which is 1 if the 1's occur in pairs.]

We represent R(i, a, b) as T[i, j, S] where j = length a,
S = { k | (a++b)[k] = 1 }.  The T array requires 4 * 8 * 9 * 256 = 72K
of storage.  Space and time are both O(n^2 * 2^n).

More than half the entries in the T array are zero, because of an odd
number of 1's in the above layout; but it's easier and probably faster
to compute the whole thing.
*)

CONST n = 8;

CONST pow2n = ASH(1, n);

VAR T: ARRAY n OF ARRAY n+1 OF ARRAY pow2n OF INTEGER;
  calls, count, zero: INTEGER;

PROCEDURE Ind(b: BOOLEAN): INTEGER;
BEGIN
  IF b THEN RETURN 1 ELSE RETURN 0 END
END Ind;

PROCEDURE Init;
  VAR i, j, r: INTEGER;
BEGIN
  FOR i := 0 TO n-1 DO
    FOR j := 0 TO n DO
      FOR r := 0 TO pow2n-1 DO
	T[i, j, r] := -1
      END
    END
  END
END Init;

PROCEDURE Compute(i, j: INTEGER; s: SET): INTEGER;
  VAR r, z: INTEGER;
BEGIN
  INC(calls);

  r := SYSTEM.VAL(INTEGER, s);
  IF T[i, j, r] >= 0 THEN RETURN T[i, j, r] END;

  IF j = 0 THEN
    IF i = 0 THEN
      (* R(0, e, b) = I(b = 11...1) *)
      z := Ind(s = {0..n-1})
    ELSE
      (* R(i+1, e, b) = R(i, b, e) *)
      z := Compute(i-1, n, s)
    END
  ELSIF j = 1 THEN
    (* R(i, 0, b) = R(i, e, 1b) *)
    (* R(i, 1, b) = R(i, e, 0b) *)
    z := Compute(i, 0, s / {0})
  ELSIF ~(j-1 IN s) THEN
   (* R(i, a0, b) = R(i, a, 1b) *)
    z := Compute(i, j-1, s / {j-1})
  ELSIF ~(j-2 IN s) THEN
    (* R(i, a01, b) = R(i, a, 10b) *)
    z := Compute(i, j-2, s / {j-1, j-2})
  ELSE
    (* R(i, a11, b) = R(i, a, 11b) + R(i, a1, 0b) *)
    z := Compute(i, j-2, s) + Compute(i, j-1, s / {j-1})
  END;

  T[i, j, r] := z;
  INC(count);
  IF z = 0 THEN INC(zero) END;
  RETURN z
END Compute;

BEGIN
  Init;
  Out.Int(Compute(n-1, n, {0..n-1}), 0); Out.Ln;
  Out.String("["); Out.Int(calls, 0); Out.String(" calls]"); Out.Ln;
  Out.String("["); Out.Int(count, 0); Out.String(" arguments]"); Out.Ln;
  Out.String("["); Out.Int(zero, 0); Out.String(" results were zero]");
  Out.Ln
END tDomino.

(*[[
!! SYMFILE #tDomino STAMP #tDomino.%main 1
!! END STAMP
!! 
MODULE tDomino STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tDomino.Ind 0 1 0
! PROCEDURE Ind(b: BOOLEAN): INTEGER;
!   IF b THEN RETURN 1 ELSE RETURN 0 END
LDLC 12
JUMPF 6
CONST 1
RETURNW
LABEL 6
CONST 0
RETURNW
END

PROC tDomino.Init 3 5 0
! PROCEDURE Init;
!   FOR i := 0 TO n-1 DO
CONST 0
STLW -4
JUMP 8
LABEL 7
!     FOR j := 0 TO n DO
CONST 0
STLW -8
JUMP 10
LABEL 9
!       FOR r := 0 TO pow2n-1 DO
CONST 0
STLW -12
JUMP 12
LABEL 11
! 	T[i, j, r] := -1
CONST -1
GLOBAL tDomino.T
LDLW -4
CONST 8
BOUND 88
CONST 9
TIMES
LDLW -8
CONST 9
BOUND 88
PLUS
CONST 256
TIMES
LDLW -12
CONST 256
BOUND 88
PLUS
STIW
!       FOR r := 0 TO pow2n-1 DO
INCL -12
LABEL 12
LDLW -12
CONST 255
JLEQ 11
!     FOR j := 0 TO n DO
INCL -8
LABEL 10
LDLW -8
CONST 8
JLEQ 9
!   FOR i := 0 TO n-1 DO
INCL -4
LABEL 8
LDLW -4
CONST 7
JLEQ 7
RETURN
END

PROC tDomino.Compute 2 5 0
! PROCEDURE Compute(i, j: INTEGER; s: SET): INTEGER;
!   INC(calls);
GLOBAL tDomino.calls
DUP 0
LOADW
INC
SWAP
STOREW
!   r := SYSTEM.VAL(INTEGER, s);
LDLW 20
STLW -4
!   IF T[i, j, r] >= 0 THEN RETURN T[i, j, r] END;
GLOBAL tDomino.T
LDLW 12
CONST 8
BOUND 100
CONST 9
TIMES
LDLW 16
CONST 9
BOUND 100
PLUS
CONST 256
TIMES
LDLW -4
CONST 256
BOUND 100
PLUS
LDIW
JLTZ 14
GLOBAL tDomino.T
LDLW 12
CONST 8
BOUND 100
CONST 9
TIMES
LDLW 16
CONST 9
BOUND 100
PLUS
CONST 256
TIMES
LDLW -4
CONST 256
BOUND 100
PLUS
LDIW
RETURNW
LABEL 14
!   IF j = 0 THEN
LDLW 16
JNEQZ 16
!     IF i = 0 THEN
LDLW 12
JNEQZ 18
!       z := Ind(s = {0..n-1})
LDLW 20
CONST 255
EQ
ALIGNC
GLOBAL tDomino.Ind
CALLW 1
STLW -8
JUMP 15
LABEL 18
!       z := Compute(i-1, n, s)
LDLW 20
CONST 8
LDLW 12
DEC
GLOBAL tDomino.Compute
CALLW 3
STLW -8
JUMP 15
LABEL 16
!   ELSIF j = 1 THEN
LDLW 16
CONST 1
JNEQ 19
!     z := Compute(i, 0, s / {0})
LDLW 20
CONST 1
BITXOR
CONST 0
LDLW 12
GLOBAL tDomino.Compute
CALLW 3
STLW -8
JUMP 15
LABEL 19
!   ELSIF ~(j-1 IN s) THEN
LDLW 16
DEC
CONST 32
BOUND 114
BIT
LDLW 20
BITAND
JNEQZ 20
!     z := Compute(i, j-1, s / {j-1})
LDLW 20
LDLW 16
DEC
CONST 32
BOUND 116
BIT
BITXOR
LDLW 16
DEC
LDLW 12
GLOBAL tDomino.Compute
CALLW 3
STLW -8
JUMP 15
LABEL 20
!   ELSIF ~(j-2 IN s) THEN
LDLW 16
CONST 2
MINUS
CONST 32
BOUND 117
BIT
LDLW 20
BITAND
JNEQZ 21
!     z := Compute(i, j-2, s / {j-1, j-2})
LDLW 20
LDLW 16
DEC
CONST 32
BOUND 119
BIT
LDLW 16
CONST 2
MINUS
CONST 32
BOUND 119
BIT
BITOR
BITXOR
LDLW 16
CONST 2
MINUS
LDLW 12
GLOBAL tDomino.Compute
CALLW 3
STLW -8
JUMP 15
LABEL 21
!     z := Compute(i, j-2, s) + Compute(i, j-1, s / {j-1})
LDLW 20
LDLW 16
CONST 2
MINUS
LDLW 12
GLOBAL tDomino.Compute
CALLW 3
LDLW 20
LDLW 16
DEC
CONST 32
BOUND 122
BIT
BITXOR
LDLW 16
DEC
LDLW 12
GLOBAL tDomino.Compute
CALLW 3
PLUS
STLW -8
LABEL 15
!   T[i, j, r] := z;
LDLW -8
GLOBAL tDomino.T
LDLW 12
CONST 8
BOUND 125
CONST 9
TIMES
LDLW 16
CONST 9
BOUND 125
PLUS
CONST 256
TIMES
LDLW -4
CONST 256
BOUND 125
PLUS
STIW
!   INC(count);
GLOBAL tDomino.count
DUP 0
LOADW
INC
SWAP
STOREW
!   IF z = 0 THEN INC(zero) END;
LDLW -8
JNEQZ 23
GLOBAL tDomino.zero
DUP 0
LOADW
INC
SWAP
STOREW
LABEL 23
!   RETURN z
LDLW -8
RETURNW
END

PROC tDomino.%main 0 5 0
!   Init;
GLOBAL tDomino.Init
CALL 0
!   Out.Int(Compute(n-1, n, {0..n-1}), 0); Out.Ln;
CONST 0
CONST 255
CONST 8
CONST 7
GLOBAL tDomino.Compute
CALLW 3
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
!   Out.String("["); Out.Int(calls, 0); Out.String(" calls]"); Out.Ln;
CONST 2
GLOBAL tDomino.%4
GLOBAL Out.String
CALL 2
CONST 0
LDGW tDomino.calls
GLOBAL Out.Int
CALL 2
CONST 8
GLOBAL tDomino.%1
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
!   Out.String("["); Out.Int(count, 0); Out.String(" arguments]"); Out.Ln;
CONST 2
GLOBAL tDomino.%4
GLOBAL Out.String
CALL 2
CONST 0
LDGW tDomino.count
GLOBAL Out.Int
CALL 2
CONST 12
GLOBAL tDomino.%2
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
!   Out.String("["); Out.Int(zero, 0); Out.String(" results were zero]");
CONST 2
GLOBAL tDomino.%4
GLOBAL Out.String
CALL 2
CONST 0
LDGW tDomino.zero
GLOBAL Out.Int
CALL 2
CONST 20
GLOBAL tDomino.%3
GLOBAL Out.String
CALL 2
!   Out.Ln
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tDomino.T 73728
GLOVAR tDomino.calls 4
GLOVAR tDomino.count 4
GLOVAR tDomino.zero 4

! String " calls]"
DEFINE tDomino.%1
STRING 2063616C6C735D00

! String " arguments]"
DEFINE tDomino.%2
STRING 20617267756D656E74735D00

! String " results were zero]"
DEFINE tDomino.%3
STRING 20726573756C74732077657265207A65726F5D00

! String "["
DEFINE tDomino.%4
STRING 5B00

! End of file
]]*)

