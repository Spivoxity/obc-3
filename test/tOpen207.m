MODULE tOpen207;

(*<<
15
>>*)

IMPORT Out;

TYPE flex = POINTER TO ARRAY OF INTEGER;

PROCEDURE Sum(a: flex): INTEGER;
  VAR i, s: INTEGER;
BEGIN
  s := 0;
  FOR i := 0 TO LEN(a^)-1 DO s := s + a[i] END;
  RETURN s
END Sum;

VAR j: INTEGER; b: flex;

BEGIN
  NEW(b, 5);
  FOR j := 0 TO 4 DO b[j] := j+1 END;
  Out.Int(Sum(b), 0); Out.Ln;
END tOpen207.

(*[[
!! (SYMFILE #tOpen207 STAMP #tOpen207.%main 1)
!! (CHKSUM STAMP)
!! 
MODULE tOpen207 STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tOpen207.Sum 12 5 0x00100001
FRAME
! PROCEDURE Sum(a: flex): INTEGER;
!   s := 0;
CONST 0
STLW -8
!   FOR i := 0 TO LEN(a^)-1 DO s := s + a[i] END;
LDLW 12
NCHECK 15
LDNW -4
LDNW 4
DEC
STLW -12
CONST 0
STLW -4
LABEL L1
LDLW -4
LDLW -12
JGT L2
LDLW -8
LDLW 12
NCHECK 15
LDLW -4
DUP 1
LDNW -4
LDNW 4
BOUND 15
LDIW
PLUS
STLW -8
INCL -4
JUMP L1
LABEL L2
!   RETURN s
LDLW -8
RETURNW
END

PROC tOpen207.%main 0 6 0
!   NEW(b, 5);
CONST 5
CONST 1
CONST 4
CONST 0
GLOBAL NEWFLEX
CALLW 4
STGW tOpen207.b
!   FOR j := 0 TO 4 DO b[j] := j+1 END;
CONST 0
STGW tOpen207.j
LABEL L3
LDGW tOpen207.j
CONST 4
JGT L4
LDGW tOpen207.j
INC
LDGW tOpen207.b
NCHECK 23
LDGW tOpen207.j
DUP 1
LDNW -4
LDNW 4
BOUND 23
STIW
LDGW tOpen207.j
INC
STGW tOpen207.j
JUMP L3
LABEL L4
!   Out.Int(Sum(b), 0); Out.Ln;
CONST 0
LDGW tOpen207.b
GLOBAL tOpen207.Sum
CALLW 1
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tOpen207.j 4
GLOVAR tOpen207.b 4

! Global pointer map
DEFINE tOpen207.%gcmap
WORD GC_POINTER
WORD tOpen207.b
WORD GC_END

! End of file
]]*)
