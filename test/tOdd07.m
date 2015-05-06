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
!! SYMFILE #tOdd07 STAMP #tOdd07.%main 1
!! END STAMP
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
LABEL 1
LDGW tOdd07.i
CONST 10
JGT 2
!     IF ODD(i) THEN Out.Int(i,0) END;
LDGW tOdd07.i
CONST 1
BITAND
JEQZ 5
CONST 0
LDGW tOdd07.i
GLOBAL Out.Int
CALL 2
LABEL 5
!     b := ODD(i);
LDGW tOdd07.i
CONST 1
BITAND
STGC tOdd07.b
!     IF b = r THEN Out.Int(i,0) END
LDGC tOdd07.b
LDGC tOdd07.r
JNEQ 8
CONST 0
LDGW tOdd07.i
GLOBAL Out.Int
CALL 2
LABEL 8
!   FOR i := 0 TO 10 DO
LDGW tOdd07.i
INC
STGW tOdd07.i
JUMP 1
LABEL 2
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
