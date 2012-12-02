MODULE tArray;

(*<<
45
>>*)

IMPORT Out;

VAR i, s: INTEGER;
  a: ARRAY 10 OF SHORTINT;

BEGIN
  FOR i := 0 TO 9 DO 
    a[i] := SHORT(i) 
  END;

  s := 0;
  FOR i := 0 TO 9 DO 
    s := s + a[i] 
  END;
  Out.Int(s, 0); Out.Ln
END tArray.

(*[[
!! SYMFILE #tArray STAMP #tArray.%main 1
!! END STAMP
!! 
MODULE tArray STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tArray.%main 0 4 0
!   FOR i := 0 TO 9 DO 
CONST 0
STGW tArray.i
JUMP 2
LABEL 1
!     a[i] := SHORT(i) 
LDGW tArray.i
CONVNS
GLOBAL tArray.a
LDGW tArray.i
CONST 10
BOUND 14
STIS
!   FOR i := 0 TO 9 DO 
LDGW tArray.i
INC
STGW tArray.i
LABEL 2
LDGW tArray.i
CONST 9
JLEQ 1
!   s := 0;
CONST 0
STGW tArray.s
!   FOR i := 0 TO 9 DO 
CONST 0
STGW tArray.i
JUMP 4
LABEL 3
!     s := s + a[i] 
LDGW tArray.s
GLOBAL tArray.a
LDGW tArray.i
CONST 10
BOUND 19
LDIS
PLUS
STGW tArray.s
!   FOR i := 0 TO 9 DO 
LDGW tArray.i
INC
STGW tArray.i
LABEL 4
LDGW tArray.i
CONST 9
JLEQ 3
!   Out.Int(s, 0); Out.Ln
CONST 0
LDGW tArray.s
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tArray.i 4
GLOVAR tArray.s 4
GLOVAR tArray.a 20

! End of file
]]*)
