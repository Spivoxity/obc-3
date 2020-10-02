MODULE tOdd07;

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
END tOdd07.

(*[[
!! (SYMFILE #tOdd07 STAMP #tOdd07.%main 1 #tOdd07.m)
!! (CHKSUM STAMP)
!! 
MODULE tOdd07 STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tOdd07.%main 0 3 0
!   r := TRUE;
CONST 1
STGC tOdd07.r
!   FOR i := 0 TO 10 DO
CONST 0
STGW tOdd07.i
LABEL L1
LDGW tOdd07.i
CONST 10
JGT L2
!     IF ODD(i) THEN Out.Int(i,0) END;
LDGW tOdd07.i
CONST 1
BITAND
JEQZ L5
CONST 0
LDGW tOdd07.i
GLOBAL Out.Int
CALL 2
LABEL L5
!     b := ODD(i);
LDGW tOdd07.i
CONST 1
BITAND
STGC tOdd07.b
!     IF b = r THEN Out.Int(i,0) END
LDGC tOdd07.b
LDGC tOdd07.r
JNEQ L8
CONST 0
LDGW tOdd07.i
GLOBAL Out.Int
CALL 2
LABEL L8
!   FOR i := 0 TO 10 DO
LDGW tOdd07.i
INC
STGW tOdd07.i
JUMP L1
LABEL L2
!   Out.Ln
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tOdd07.b 1
GLOVAR tOdd07.r 1
GLOVAR tOdd07.i 4

! End of file
]]*)
