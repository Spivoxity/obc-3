MODULE tFlexAsgn;

IMPORT Out;

TYPE vec = ARRAY OF INTEGER;

PROCEDURE Sum(a: vec): INTEGER;
  VAR i, s: INTEGER; b: POINTER TO vec;
BEGIN
  NEW(b, LEN(a));
  b^ := a;
  s := 0;
  FOR i := 0 TO LEN(b^)-1 DO s := s + b[i] END;
  RETURN s
END Sum;

VAR u: ARRAY 4 OF INTEGER;

BEGIN
  u[0] := 3; u[1] := 1; u[2] := 4; u[3] := 1;
  Out.Int(Sum(u), 0); Out.Ln
END tFlexAsgn.

(*<<
9
>>*)

(*[[
!! (SYMFILE #tFlexAsgn STAMP #tFlexAsgn.%main 1 #tFlexAsgn.m)
!! (CHKSUM STAMP)
!! 
MODULE tFlexAsgn STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tFlexAsgn.Sum 16 7 0x00004001
! PROCEDURE Sum(a: vec): INTEGER;
LOCAL 12
LDLW 16
CONST 4
TIMES
FLEXCOPY
!   NEW(b, LEN(a));
LDLW 16
CONST 1
CONST 4
CONST 0
GLOBAL NEWFLEX
CALLW 4
STLW -12
!   b^ := a;
LDLW -12
NCHECK 11
DUP 0
LDNW -4
LDNW 4
SWAP
LDLW 16
LDLW 12
CONST 1
CONST 4
GLOBAL FLEXASSIGN
CALL 6
!   s := 0;
CONST 0
STLW -8
!   FOR i := 0 TO LEN(b^)-1 DO s := s + b[i] END;
LDLW -12
NCHECK 13
LDNW -4
LDNW 4
DEC
STLW -16
CONST 0
STLW -4
LABEL L1
LDLW -4
LDLW -16
JGT L2
LDLW -8
LDLW -12
NCHECK 13
LDLW -4
DUP 1
LDNW -4
LDNW 4
BOUND 13
LDIW
PLUS
STLW -8
INCL -4
JUMP L1
LABEL L2
!   RETURN s
LDLW -8
RETURN
END

PROC tFlexAsgn.%main 0 4 0
!   u[0] := 3; u[1] := 1; u[2] := 4; u[3] := 1;
CONST 3
STGW tFlexAsgn.u
CONST 1
GLOBAL tFlexAsgn.u
STNW 4
CONST 4
GLOBAL tFlexAsgn.u
STNW 8
CONST 1
GLOBAL tFlexAsgn.u
STNW 12
!   Out.Int(Sum(u), 0); Out.Ln
CONST 0
CONST 4
GLOBAL tFlexAsgn.u
GLOBAL tFlexAsgn.Sum
CALLW 2
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tFlexAsgn.u 16

! End of file
]]*)
