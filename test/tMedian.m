MODULE tMedian;

IMPORT Random, Out;

TYPE array = ARRAY 100 OF INTEGER;

PROCEDURE Deal(n: INTEGER; VAR a, b: array);
  VAR j, k: INTEGER;
BEGIN
  j := 0; k := 0;
  WHILE (j < n) OR (k < n) DO
    IF Random.Roll(2*n-j-k) < n-j THEN
      a[j] := j+k; j := j+1
    ELSE
      b[k] := j+k; k := k+1
    END
  END
END Deal;

PROCEDURE Max(x, y: INTEGER): INTEGER;
BEGIN 
  IF x > y THEN RETURN x ELSE RETURN y END 
END Max;

(* Median returns c[n] where c = merge(a[0..n), b[0..n)) *)
PROCEDURE Median(n: INTEGER; VAR a, b: array): INTEGER;
  VAR j, k, m, r: INTEGER;
BEGIN
  j := 0; k := 0; m := n;
  (* Inv: Answer = c[m] where c = merge(a[j..j+m), b[k..k+m)) *)
  WHILE m > 1 DO
    r := m DIV 2;
    IF a[j+r] < b[k+r] THEN j := j+r ELSE k := k+r END;
    m := m-r
  END;
  RETURN Max(a[j], b[k])
END Median;

VAR a, b: array; i, n: INTEGER;

BEGIN
  FOR n := 1 TO 20 DO
    Deal(n, a, b);
    FOR i := 0 TO n-1 DO Out.Int(a[i], 3) END; Out.Ln;
    FOR i := 0 TO n-1 DO Out.Int(b[i], 3) END; Out.Ln;
    Out.Int(Median(n, a, b), 0); Out.Ln
  END
END tMedian.

(*<<
  0
  1
1
  2  3
  0  1
2
  0  1  3
  2  4  5
3
  1  5  6  7
  0  2  3  4
4
  0  1  4  5  9
  2  3  6  7  8
5
  1  3  5  6  7 11
  0  2  4  8  9 10
6
  2  3  4  6 10 11 12
  0  1  5  7  8  9 13
7
  0  3  5  6  7  9 11 14
  1  2  4  8 10 12 13 15
8
  0  1  3  5  6  8  9 10 11
  2  4  7 12 13 14 15 16 17
9
  0  4  5  6  8 13 14 15 18 19
  1  2  3  7  9 10 11 12 16 17
10
  0  3  6  7  9 12 13 14 15 17 18
  1  2  4  5  8 10 11 16 19 20 21
11
  0  4  5  6  7 10 12 14 17 18 20 22
  1  2  3  8  9 11 13 15 16 19 21 23
12
  0  1  2  5  6  9 11 13 15 17 19 21 22
  3  4  7  8 10 12 14 16 18 20 23 24 25
13
  1  2  3  5  6  8 10 12 15 16 17 21 25 27
  0  4  7  9 11 13 14 18 19 20 22 23 24 26
14
  0  1  2  3  4  7  8 12 16 18 20 23 24 25 27
  5  6  9 10 11 13 14 15 17 19 21 22 26 28 29
15
  2  5  6  7 12 14 15 16 17 21 22 24 25 27 30 31
  0  1  3  4  8  9 10 11 13 18 19 20 23 26 28 29
16
  0  4  5  6  7 10 11 13 14 16 17 20 24 25 29 31 33
  1  2  3  8  9 12 15 18 19 21 22 23 26 27 28 30 32
17
  1  2 10 16 17 19 22 23 24 25 27 28 29 30 31 33 34 35
  0  3  4  5  6  7  8  9 11 12 13 14 15 18 20 21 26 32
18
  1  2  3  7  9 11 12 13 14 15 17 20 22 23 24 28 31 36 37
  0  4  5  6  8 10 16 18 19 21 25 26 27 29 30 32 33 34 35
19
  1  2  3  4  7  8  9 12 14 17 19 20 25 26 30 31 32 33 34 38
  0  5  6 10 11 13 15 16 18 21 22 23 24 27 28 29 35 36 37 39
20
>>*)

(*[[
!! (SYMFILE #tMedian STAMP #tMedian.%main 1 #tMedian.m)
!! (CHKSUM STAMP)
!! 
MODULE tMedian STAMP 0
IMPORT Random STAMP
IMPORT Out STAMP
ENDHDR

PROC tMedian.Deal 8 4 0x00600001
! PROCEDURE Deal(n: INTEGER; VAR a, b: array);
!   j := 0; k := 0;
CONST 0
STLW -4
CONST 0
STLW -8
LABEL L1
!   WHILE (j < n) OR (k < n) DO
LDLW -4
LDLW 12
JLT L2
LDLW -8
LDLW 12
JGEQ L3
LABEL L2
!     IF Random.Roll(2*n-j-k) < n-j THEN
LDLW 12
CONST 2
TIMES
LDLW -4
MINUS
LDLW -8
MINUS
GLOBAL Random.Roll
CALLW 1
LDLW 12
LDLW -4
MINUS
JGEQ L6
!       a[j] := j+k; j := j+1
LDLW -4
LDLW -8
PLUS
LDLW 16
LDLW -4
CONST 100
BOUND 13
STIW
INCL -4
JUMP L1
LABEL L6
!       b[k] := j+k; k := k+1
LDLW -4
LDLW -8
PLUS
LDLW 20
LDLW -8
CONST 100
BOUND 15
STIW
INCL -8
JUMP L1
LABEL L3
RETURN
END

PROC tMedian.Max 0 2 0
! PROCEDURE Max(x, y: INTEGER): INTEGER;
!   IF x > y THEN RETURN x ELSE RETURN y END 
LDLW 12
LDLW 16
JLEQ L10
LDLW 12
RETURN
LABEL L10
LDLW 16
RETURN
END

PROC tMedian.Median 16 4 0x00600001
! PROCEDURE Median(n: INTEGER; VAR a, b: array): INTEGER;
!   j := 0; k := 0; m := n;
CONST 0
STLW -4
CONST 0
STLW -8
LDLW 12
STLW -12
LABEL L11
!   WHILE m > 1 DO
LDLW -12
CONST 1
JLEQ L13
!     r := m DIV 2;
LDLW -12
CONST 2
DIV
STLW -16
!     IF a[j+r] < b[k+r] THEN j := j+r ELSE k := k+r END;
LDLW 16
LDLW -4
LDLW -16
PLUS
CONST 100
BOUND 33
LDIW
LDLW 20
LDLW -8
LDLW -16
PLUS
CONST 100
BOUND 33
LDIW
JGEQ L16
LDLW -4
LDLW -16
PLUS
STLW -4
JUMP L14
LABEL L16
LDLW -8
LDLW -16
PLUS
STLW -8
LABEL L14
!     m := m-r
LDLW -12
LDLW -16
MINUS
STLW -12
JUMP L11
LABEL L13
!   RETURN Max(a[j], b[k])
LDLW 20
LDLW -8
CONST 100
BOUND 36
LDIW
LDLW 16
LDLW -4
CONST 100
BOUND 36
LDIW
GLOBAL tMedian.Max
CALLW 2
RETURN
END

PROC tMedian.%main 8 5 0
!   FOR n := 1 TO 20 DO
CONST 1
STGW tMedian.n
LABEL L17
LDGW tMedian.n
CONST 20
JGT L18
!     Deal(n, a, b);
GLOBAL tMedian.b
GLOBAL tMedian.a
LDGW tMedian.n
GLOBAL tMedian.Deal
CALL 3
!     FOR i := 0 TO n-1 DO Out.Int(a[i], 3) END; Out.Ln;
LDGW tMedian.n
DEC
STLW -4
CONST 0
STGW tMedian.i
LABEL L19
LDGW tMedian.i
LDLW -4
JGT L20
CONST 3
GLOBAL tMedian.a
LDGW tMedian.i
CONST 100
BOUND 44
LDIW
GLOBAL Out.Int
CALL 2
LDGW tMedian.i
INC
STGW tMedian.i
JUMP L19
LABEL L20
GLOBAL Out.Ln
CALL 0
!     FOR i := 0 TO n-1 DO Out.Int(b[i], 3) END; Out.Ln;
LDGW tMedian.n
DEC
STLW -8
CONST 0
STGW tMedian.i
LABEL L21
LDGW tMedian.i
LDLW -8
JGT L22
CONST 3
GLOBAL tMedian.b
LDGW tMedian.i
CONST 100
BOUND 45
LDIW
GLOBAL Out.Int
CALL 2
LDGW tMedian.i
INC
STGW tMedian.i
JUMP L21
LABEL L22
GLOBAL Out.Ln
CALL 0
!     Out.Int(Median(n, a, b), 0); Out.Ln
CONST 0
GLOBAL tMedian.b
GLOBAL tMedian.a
LDGW tMedian.n
GLOBAL tMedian.Median
CALLW 3
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
!   FOR n := 1 TO 20 DO
LDGW tMedian.n
INC
STGW tMedian.n
JUMP L17
LABEL L18
RETURN
END

! Global variables
GLOVAR tMedian.a 400
GLOVAR tMedian.b 400
GLOVAR tMedian.i 4
GLOVAR tMedian.n 4

! End of file
]]*)
