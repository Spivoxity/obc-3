MODULE tAParam;

(*<<
15
>>*)

IMPORT Out;

TYPE row = ARRAY 5 OF INTEGER;

PROCEDURE Sum(dummy: LONGREAL; a: row): INTEGER;
  VAR i, s: INTEGER;
BEGIN
  dummy := 0.0;
  s := 0;
  FOR i := 0 TO LEN(a)-1 DO 
    s := s + a[i]
  END;
  RETURN s
END Sum;

VAR j: INTEGER; b: row;

BEGIN
  FOR j := 0 TO LEN(b)-1 DO 
    b[j] := j+1
  END;
  Out.Int(Sum(0, b), 0); Out.Ln;
END tAParam.

(*[[
!! (SYMFILE #tAParam STAMP #tAParam.%main 1 #tAParam.m)
!! (CHKSUM STAMP)
!! 
MODULE tAParam STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tAParam.Sum 28 4 0
! PROCEDURE Sum(dummy: LONGREAL; a: row): INTEGER;
LOCAL -28
LDLW 20
CONST 20
FIXCOPY
!   dummy := 0.0;
DCONST 0.0
STLD 12
!   s := 0;
CONST 0
STLW -8
!   FOR i := 0 TO LEN(a)-1 DO 
CONST 0
STLW -4
LABEL L1
LDLW -4
CONST 4
JGT L2
!     s := s + a[i]
LDLW -8
LOCAL -28
LDLW -4
CONST 5
BOUND 17
LDIW
PLUS
STLW -8
!   FOR i := 0 TO LEN(a)-1 DO 
INCL -4
JUMP L1
LABEL L2
!   RETURN s
LDLW -8
RETURN
END

PROC tAParam.%main 0 5 0
!   FOR j := 0 TO LEN(b)-1 DO 
CONST 0
STGW tAParam.j
LABEL L3
LDGW tAParam.j
CONST 4
JGT L4
!     b[j] := j+1
LDGW tAParam.j
INC
GLOBAL tAParam.b
LDGW tAParam.j
CONST 5
BOUND 26
STIW
!   FOR j := 0 TO LEN(b)-1 DO 
LDGW tAParam.j
INC
STGW tAParam.j
JUMP L3
LABEL L4
!   Out.Int(Sum(0, b), 0); Out.Ln;
CONST 0
GLOBAL tAParam.b
DCONST 0.0
GLOBAL tAParam.Sum
CALLW 3
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tAParam.j 4
GLOVAR tAParam.b 20

! End of file
]]*)
