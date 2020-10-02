MODULE tTiling;

IMPORT Out;

CONST N = 8;

CONST SMAX = LSL(1, N);

(* max -- maximum element of each (nonempty) set *)
VAR max: ARRAY SMAX OF INTEGER;

(* Init -- set up the max array *)
PROCEDURE Init;
  VAR i, j: INTEGER;
BEGIN
  FOR i := 0 TO N-1 DO
    FOR j := LSL(1, i) TO LSL(1, i+1)-1 DO
      max[j] := i
    END
  END
END Init;

(* The predecessors of a set s with repect to a set u are all the sets
   we can reach by removing from u the roots of a cover of s.  Each
   element of s is covered either by a horizontal domino with no root
   (covering two adjacent elements), or with a vertical domino with
   a root descending into the next layer *)

(* DoPred -- visit all predecessors of a set *)
PROCEDURE DoPred(s, u: SET; p: PROCEDURE (t: SET));
  VAR m: INTEGER;
BEGIN
  IF s = {} THEN
    p(u)
  ELSE
    m := max[ORD(s)];
    IF (m > 0) & (m-1 IN s) THEN
      DoPred(s - {m-1, m}, u, p)  (* Horizontal *)
    END;
    DoPred(s - {m}, u - {m}, p)   (* Vertical *)
  END
END DoPred;

(* We memoize the Calc subroutine with this array of flags and values. *)

VAR memo:
    ARRAY N OF ARRAY SMAX OF
      RECORD set: BOOLEAN; val: INTEGER END;
  nvals, nzeros: INTEGER;

(* Calc -- calculate the number of tilings of s from previous vector v *)
PROCEDURE Calc(k: INTEGER; s: SET): INTEGER;
  VAR ways: INTEGER;

  PROCEDURE Inc(u: SET);
  BEGIN
    ways := ways + Calc(k-1, u)
  END Inc;
BEGIN
  IF k = 0 THEN
    IF s = {0..N-1} THEN RETURN 1 ELSE RETURN 0 END
  ELSIF memo[k-1][ORD(s)].set THEN
    RETURN memo[k-1][ORD(s)].val
  ELSE
    ways := 0;
    DoPred(s, {0..N-1}, Inc);
    memo[k-1][ORD(s)].set := TRUE;
    memo[k-1][ORD(s)].val := ways;
    INC(nvals);
    IF ways = 0 THEN INC(nzeros) END;
    RETURN ways
  END
END Calc;

PROCEDURE Main;
BEGIN
  Out.Int(Calc(N, {0..N-1}), 0); Out.Ln;
  Out.String("("); Out.Int(nvals, 0);
  Out.String(" values were needed)"); Out.Ln;
  Out.String("("); Out.Int(nzeros, 0);
  Out.String(" values were zero)"); Out.Ln
END Main;

BEGIN
  Init; Main
END tTiling.

(*<<
12988816
(445 values were needed)
(46 values were zero)
>>*)

(*[[
!! (SYMFILE #tTiling STAMP #tTiling.%main 1 #tTiling.m)
!! (CHKSUM STAMP)
!! 
MODULE tTiling STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tTiling.Init 12 4 0
! PROCEDURE Init;
!   FOR i := 0 TO N-1 DO
CONST 0
STLW -4
LABEL L6
LDLW -4
CONST 7
JGT L7
!     FOR j := LSL(1, i) TO LSL(1, i+1)-1 DO
CONST 1
LDLW -4
INC
LSL
DEC
STLW -12
CONST 1
LDLW -4
LSL
STLW -8
LABEL L8
LDLW -8
LDLW -12
JGT L9
!       max[j] := i
LDLW -4
GLOBAL tTiling.max
LDLW -8
CONST 256
BOUND 18
STIW
!     FOR j := LSL(1, i) TO LSL(1, i+1)-1 DO
INCL -8
JUMP L8
LABEL L9
!   FOR i := 0 TO N-1 DO
INCL -4
JUMP L6
LABEL L7
RETURN
END

PROC tTiling.DoPred 4 8 0
! PROCEDURE DoPred(s, u: SET; p: PROCEDURE (t: SET));
!   IF s = {} THEN
LDLW 12
JNEQZ L16
!     p(u)
LDLW 16
LDLW 24
STATLINK
LDLW 20
NCHECK 34
CALL 1
RETURN
LABEL L16
!     m := max[ORD(s)];
GLOBAL tTiling.max
LDLW 12
CONST 256
BOUND 36
LDIW
STLW -4
!     IF (m > 0) & (m-1 IN s) THEN
LDLW -4
JLEQZ L13
LDLW 12
CONST 1
LDLW -4
DEC
CONST 32
BOUND 37
LSL
BITAND
JEQZ L13
!       DoPred(s - {m-1, m}, u, p)  (* Horizontal *)
LDLW 24
LDLW 20
LDLW 16
LDLW 12
CONST 1
LDLW -4
DEC
CONST 32
BOUND 38
LSL
CONST 1
LDLW -4
CONST 32
BOUND 38
LSL
BITOR
BITNOT
BITAND
GLOBAL tTiling.DoPred
CALL 4
LABEL L13
!     DoPred(s - {m}, u - {m}, p)   (* Vertical *)
LDLW 24
LDLW 20
LDLW 16
CONST 1
LDLW -4
CONST 32
BOUND 40
LSL
BITNOT
BITAND
LDLW 12
CONST 1
LDLW -4
CONST 32
BOUND 40
LSL
BITNOT
BITAND
GLOBAL tTiling.DoPred
CALL 4
RETURN
END

PROC tTiling.%4.Inc 4 4 0
SAVELINK
!   PROCEDURE Inc(u: SET);
!     ways := ways + Calc(k-1, u)
LDLW -4
LDNW -4
LDLW 12
LDLW -4
LDNW 12
DEC
GLOBAL tTiling.Calc
CALLW 2
PLUS
LDLW -4
STNW -4
RETURN
END

PROC tTiling.Calc 4 5 0
! PROCEDURE Calc(k: INTEGER; s: SET): INTEGER;
!   IF k = 0 THEN
LDLW 12
JNEQZ L22
!     IF s = {0..N-1} THEN RETURN 1 ELSE RETURN 0 END
LDLW 16
CONST 255
JNEQ L25
CONST 1
RETURN
LABEL L25
CONST 0
RETURN
LABEL L22
!   ELSIF memo[k-1][ORD(s)].set THEN
GLOBAL tTiling.memo
LDLW 12
DEC
CONST 8
BOUND 62
CONST 256
TIMES
LDLW 16
CONST 256
BOUND 62
PLUS
INDEXD
LOADC
JEQZ L27
!     RETURN memo[k-1][ORD(s)].val
GLOBAL tTiling.memo
LDLW 12
DEC
CONST 8
BOUND 63
CONST 256
TIMES
LDLW 16
CONST 256
BOUND 63
PLUS
INDEXD
LDNW 4
RETURN
LABEL L27
!     ways := 0;
CONST 0
STLW -4
!     DoPred(s, {0..N-1}, Inc);
LOCAL 0
GLOBAL tTiling.%4.Inc
CONST 255
LDLW 16
GLOBAL tTiling.DoPred
CALL 4
!     memo[k-1][ORD(s)].set := TRUE;
CONST 1
GLOBAL tTiling.memo
LDLW 12
DEC
CONST 8
BOUND 67
CONST 256
TIMES
LDLW 16
CONST 256
BOUND 67
PLUS
INDEXD
STOREC
!     memo[k-1][ORD(s)].val := ways;
LDLW -4
GLOBAL tTiling.memo
LDLW 12
DEC
CONST 8
BOUND 68
CONST 256
TIMES
LDLW 16
CONST 256
BOUND 68
PLUS
INDEXD
STNW 4
!     INC(nvals);
LDGW tTiling.nvals
INC
STGW tTiling.nvals
!     IF ways = 0 THEN INC(nzeros) END;
LDLW -4
JNEQZ L20
LDGW tTiling.nzeros
INC
STGW tTiling.nzeros
LABEL L20
!     RETURN ways
LDLW -4
RETURN
END

PROC tTiling.Main 0 4 0
! PROCEDURE Main;
!   Out.Int(Calc(N, {0..N-1}), 0); Out.Ln;
CONST 0
CONST 255
CONST 8
GLOBAL tTiling.Calc
CALLW 2
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
!   Out.String("("); Out.Int(nvals, 0);
CONST 2
GLOBAL tTiling.%5
GLOBAL Out.String
CALL 2
CONST 0
LDGW tTiling.nvals
GLOBAL Out.Int
CALL 2
!   Out.String(" values were needed)"); Out.Ln;
CONST 21
GLOBAL tTiling.%1
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
!   Out.String("("); Out.Int(nzeros, 0);
CONST 2
GLOBAL tTiling.%5
GLOBAL Out.String
CALL 2
CONST 0
LDGW tTiling.nzeros
GLOBAL Out.Int
CALL 2
!   Out.String(" values were zero)"); Out.Ln
CONST 19
GLOBAL tTiling.%2
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

PROC tTiling.%main 0 1 0
!   Init; Main
GLOBAL tTiling.Init
CALL 0
GLOBAL tTiling.Main
CALL 0
RETURN
END

! Global variables
GLOVAR tTiling.max 1024
GLOVAR tTiling.memo 16384
GLOVAR tTiling.nvals 4
GLOVAR tTiling.nzeros 4

! String " values were needed)"
DEFINE tTiling.%1
STRING 2076616C7565732077657265206E65656465642900

! String " values were zero)"
DEFINE tTiling.%2
STRING 2076616C7565732077657265207A65726F2900

! String "("
DEFINE tTiling.%5
STRING 2800

! Descriptor for *anon*
DEFINE tTiling.%3
WORD 0
WORD 0
WORD tTiling.%3.%anc

DEFINE tTiling.%3.%anc
WORD tTiling.%3

! End of file
]]*)
