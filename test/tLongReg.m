MODULE tLongReg;

(*<<
3229
>>*)

IMPORT Out;

PROCEDURE Sum(a: ARRAY OF LONGINT): LONGINT;
  VAR i: INTEGER;
BEGIN
  i := 0;
  RETURN a[i] + a[i+1] * a[i+2]
END Sum;

VAR b: ARRAY 3 OF LONGINT;

BEGIN
  b[0] := 37;
  b[1] := 42;
  b[2] := 76;
  Out.LongInt(Sum(b), 0); Out.Ln
END tLongReg.

(*[[
!! (SYMFILE #tLongReg STAMP #tLongReg.%main 1 #tLongReg.m)
!! (CHKSUM STAMP)
!! 
MODULE tLongReg STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tLongReg.Sum 4 7 0
! PROCEDURE Sum(a: ARRAY OF LONGINT): LONGINT;
LOCAL 12
LDLW 16
CONST 8
TIMES
FLEXCOPY
!   i := 0;
CONST 0
STLW -4
!   RETURN a[i] + a[i+1] * a[i+2]
LDLW 12
LDLW -4
LDLW 16
BOUND 13
LDIQ
LDLW 12
LDLW -4
INC
LDLW 16
BOUND 13
LDIQ
LDLW 12
LDLW -4
CONST 2
PLUS
LDLW 16
BOUND 13
LDIQ
QTIMES
QPLUS
RETURN
END

PROC tLongReg.%main 0 5 0
!   b[0] := 37;
CONST 37
CONVNQ
STGQ tLongReg.b
!   b[1] := 42;
CONST 42
CONVNQ
GLOBAL tLongReg.b
CONST 1
STIQ
!   b[2] := 76;
CONST 76
CONVNQ
GLOBAL tLongReg.b
CONST 2
STIQ
!   Out.LongInt(Sum(b), 0); Out.Ln
CONST 0
CONST 3
GLOBAL tLongReg.b
GLOBAL tLongReg.Sum
CALLQ 2
GLOBAL Out.LongInt
CALL 3
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tLongReg.b 24

! End of file
]]*)
