MODULE tLaminae;

IMPORT Out;

VAR count: ARRAY 250001 OF INTEGER;

PROCEDURE Tally;
  VAR a, d: INTEGER;
BEGIN
  FOR a := 3 TO 250001 DO
    d := 1;
    WHILE (2*d < a) & (d*(a-d) <= 250000) DO
      INC(count[d*(a-d)]); d := d+1
    END
  END
END Tally;

PROCEDURE Solve(x: INTEGER): INTEGER;
  VAR a, b, n: INTEGER;
BEGIN
  n := 0;
  FOR a := 3 TO x+1 DO
    b := a-2;
    WHILE (b > 0) & ((a+b)*(a-b) <= 4*x) DO
      n := n+1; b := b-2
    END
  END;
  RETURN n
END Solve;

VAR i, total: INTEGER;

BEGIN
  Out.Int(Solve(8), 0); Out.Ln;
  Out.Int(Solve(25), 0); Out.Ln;
  Out.Int(Solve(250000), 0); Out.Ln;
  Out.Ln;

  Tally;
  FOR i := 1 TO 250000 DO
    IF (count[i] > 0) & (count[i] <= 10) THEN
      total := total + 1
    END
  END;
  Out.Int(total, 0); Out.Ln
END tLaminae.

(*<<
9
41
1572729

209566
>>*)

(*[[
!! (SYMFILE #tLaminae STAMP #tLaminae.%main 1 #tLaminae.m)
!! (CHKSUM STAMP)
!! 
MODULE tLaminae STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tLaminae.Tally 8 4 0
! PROCEDURE Tally;
!   FOR a := 3 TO 250001 DO
CONST 3
STLW -4
LABEL L1
LDLW -4
CONST 250001
JGT L2
!     d := 1;
CONST 1
STLW -8
LABEL L3
!     WHILE (2*d < a) & (d*(a-d) <= 250000) DO
LDLW -8
CONST 2
TIMES
LDLW -4
JGEQ L5
LDLW -8
LDLW -4
LDLW -8
MINUS
TIMES
CONST 250000
JGT L5
!       INC(count[d*(a-d)]); d := d+1
GLOBAL tLaminae.count
LDLW -8
LDLW -4
LDLW -8
MINUS
TIMES
CONST 250001
BOUND 13
INDEXW
DUP 0
LOADW
INC
SWAP
STOREW
INCL -8
JUMP L3
LABEL L5
!   FOR a := 3 TO 250001 DO
INCL -4
JUMP L1
LABEL L2
RETURN
END

PROC tLaminae.Solve 16 3 0
! PROCEDURE Solve(x: INTEGER): INTEGER;
!   n := 0;
CONST 0
STLW -12
!   FOR a := 3 TO x+1 DO
LDLW 12
INC
STLW -16
CONST 3
STLW -4
LABEL L7
LDLW -4
LDLW -16
JGT L8
!     b := a-2;
LDLW -4
CONST 2
MINUS
STLW -8
LABEL L9
!     WHILE (b > 0) & ((a+b)*(a-b) <= 4*x) DO
LDLW -8
JLEQZ L11
LDLW -4
LDLW -8
PLUS
LDLW -4
LDLW -8
MINUS
TIMES
LDLW 12
CONST 4
TIMES
JGT L11
!       n := n+1; b := b-2
INCL -12
LDLW -8
CONST 2
MINUS
STLW -8
JUMP L9
LABEL L11
!   FOR a := 3 TO x+1 DO
INCL -4
JUMP L7
LABEL L8
!   RETURN n
LDLW -12
RETURN
END

PROC tLaminae.%main 0 3 0
!   Out.Int(Solve(8), 0); Out.Ln;
CONST 0
CONST 8
GLOBAL tLaminae.Solve
CALLW 1
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
!   Out.Int(Solve(25), 0); Out.Ln;
CONST 0
CONST 25
GLOBAL tLaminae.Solve
CALLW 1
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
!   Out.Int(Solve(250000), 0); Out.Ln;
CONST 0
CONST 250000
GLOBAL tLaminae.Solve
CALLW 1
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
!   Out.Ln;
GLOBAL Out.Ln
CALL 0
!   Tally;
GLOBAL tLaminae.Tally
CALL 0
!   FOR i := 1 TO 250000 DO
CONST 1
STGW tLaminae.i
LABEL L13
LDGW tLaminae.i
CONST 250000
JGT L14
!     IF (count[i] > 0) & (count[i] <= 10) THEN
GLOBAL tLaminae.count
LDGW tLaminae.i
CONST 250001
BOUND 41
LDIW
JLEQZ L17
GLOBAL tLaminae.count
LDGW tLaminae.i
CONST 250001
BOUND 41
LDIW
CONST 10
JGT L17
!       total := total + 1
LDGW tLaminae.total
INC
STGW tLaminae.total
LABEL L17
!   FOR i := 1 TO 250000 DO
LDGW tLaminae.i
INC
STGW tLaminae.i
JUMP L13
LABEL L14
!   Out.Int(total, 0); Out.Ln
CONST 0
LDGW tLaminae.total
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tLaminae.count 1000004
GLOVAR tLaminae.i 4
GLOVAR tLaminae.total 4

! End of file
]]*)
