MODULE tLongReg07;

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
END tLongReg07.

(*[[
!! (SYMFILE #tLongReg07 STAMP #tLongReg07.%main 1 #tLongReg07.m)
!! (CHKSUM STAMP)
!! 
MODULE tLongReg07 STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tLongReg07.Sum 4 7 0x00100001
! PROCEDURE Sum(a: ARRAY OF LONGINT): LONGINT;
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

PROC tLongReg07.%main 0 5 0
!   b[0] := 37;
CONST 37
CONVNQ
STGQ tLongReg07.b
!   b[1] := 42;
CONST 42
CONVNQ
GLOBAL tLongReg07.b
STNQ 8
!   b[2] := 76;
CONST 76
CONVNQ
GLOBAL tLongReg07.b
STNQ 16
!   Out.LongInt(Sum(b), 0); Out.Ln
CONST 0
CONST 3
GLOBAL tLongReg07.b
GLOBAL tLongReg07.Sum
CALLQ 2
GLOBAL Out.LongInt
CALL 3
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tLongReg07.b 24

! End of file
]]*)
