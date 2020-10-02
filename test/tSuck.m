MODULE tSuck;

(* This code sucks, but so what? *)

(*<<
BA
>>*)

IMPORT Out;

VAR b: BOOLEAN;

BEGIN
  b := FALSE;
  REPEAT
    IF (b = (TRUE & TRUE)) & TRUE THEN Out.String("A") END;
    IF b = FALSE THEN Out.String("B") END;
    b := (b = FALSE)
  UNTIL b # TRUE;
  Out.Ln
END tSuck.

(*[[
!! (SYMFILE #tSuck STAMP #tSuck.%main 1 #tSuck.m)
!! (CHKSUM STAMP)
!! 
MODULE tSuck STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tSuck.%main 0 3 0
!   b := FALSE;
CONST 0
STGC tSuck.b
LABEL L3
!     IF (b = (TRUE & TRUE)) & TRUE THEN Out.String("A") END;
LDGC tSuck.b
JEQZ L7
CONST 2
GLOBAL tSuck.%1
GLOBAL Out.String
CALL 2
LABEL L7
!     IF b = FALSE THEN Out.String("B") END;
LDGC tSuck.b
JNEQZ L11
CONST 2
GLOBAL tSuck.%2
GLOBAL Out.String
CALL 2
LABEL L11
!     b := (b = FALSE)
LDGC tSuck.b
NOT
STGC tSuck.b
!   UNTIL b # TRUE;
LDGC tSuck.b
JNEQZ L3
!   Out.Ln
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tSuck.b 1

! String "A"
DEFINE tSuck.%1
STRING 4100

! String "B"
DEFINE tSuck.%2
STRING 4200

! End of file
]]*)
