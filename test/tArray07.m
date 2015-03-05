MODULE tArray07;

(*<<
45
>>*)

IMPORT Out;

VAR i, s: INTEGER;
  a: ARRAY 10 OF SHORTINT;

BEGIN
  FOR i := 0 TO 9 DO 
    a[i] := i
  END;

  s := 0;
  FOR i := 0 TO 9 DO 
    s := s + a[i] 
  END;
  Out.Int(s, 0); Out.Ln
END tArray07.

(*[[
!! SYMFILE #tArray07 STAMP #tArray07.%main 1
!! END STAMP
!! 
MODULE tArray07 STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tArray07.%main 0 4 0
!   FOR i := 0 TO 9 DO 
CONST 0
STGW tArray07.i
LABEL 1
LDGW tArray07.i
CONST 9
JGT 2
!     a[i] := i
LDGW tArray07.i
CONVNS
GLOBAL tArray07.a
LDGW tArray07.i
CONST 10
BOUND 14
STIS
!   FOR i := 0 TO 9 DO 
LDGW tArray07.i
INC
STGW tArray07.i
JUMP 1
LABEL 2
!   s := 0;
CONST 0
STGW tArray07.s
!   FOR i := 0 TO 9 DO 
CONST 0
STGW tArray07.i
LABEL 3
LDGW tArray07.i
CONST 9
JGT 4
!     s := s + a[i] 
LDGW tArray07.s
GLOBAL tArray07.a
LDGW tArray07.i
CONST 10
BOUND 19
LDIS
PLUS
STGW tArray07.s
!   FOR i := 0 TO 9 DO 
LDGW tArray07.i
INC
STGW tArray07.i
JUMP 3
LABEL 4
!   Out.Int(s, 0); Out.Ln
CONST 0
LDGW tArray07.s
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tArray07.i 4
GLOVAR tArray07.s 4
GLOVAR tArray07.a 20

! End of file
]]*)
