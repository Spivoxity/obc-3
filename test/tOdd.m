MODULE tOdd;

IMPORT Out;

(*<<
1133557799
>>*)

VAR b, r: BOOLEAN; i : INTEGER;

BEGIN
  r := TRUE;
  FOR i := 0 TO 10 DO
    IF ODD(i) THEN Out.Int(i,0) END;
    b := ODD(i);
    IF b = r THEN Out.Int(i,0) END
  END;
  Out.Ln
END tOdd.

(*[[
!! (SYMFILE #tOdd STAMP #tOdd.%main 1 #tOdd.m)
!! (CHKSUM STAMP)
!! 
MODULE tOdd STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tOdd.%main 0 3 0
!   r := TRUE;
CONST 1
STGC tOdd.r
!   FOR i := 0 TO 10 DO
CONST 0
STGW tOdd.i
LABEL L1
LDGW tOdd.i
CONST 10
JGT L2
!     IF ODD(i) THEN Out.Int(i,0) END;
LDGW tOdd.i
CONST 1
BITAND
JEQZ L5
CONST 0
LDGW tOdd.i
GLOBAL Out.Int
CALL 2
LABEL L5
!     b := ODD(i);
LDGW tOdd.i
CONST 1
BITAND
STGC tOdd.b
!     IF b = r THEN Out.Int(i,0) END
LDGC tOdd.b
LDGC tOdd.r
JNEQ L8
CONST 0
LDGW tOdd.i
GLOBAL Out.Int
CALL 2
LABEL L8
!   FOR i := 0 TO 10 DO
LDGW tOdd.i
INC
STGW tOdd.i
JUMP L1
LABEL L2
!   Out.Ln
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tOdd.b 1
GLOVAR tOdd.r 1
GLOVAR tOdd.i 4

! End of file
]]*)
