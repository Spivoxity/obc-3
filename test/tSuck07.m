MODULE tSuck07;

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
END tSuck07.

(*[[
!! (SYMFILE #tSuck07 STAMP #tSuck07.%main 1 #tSuck07.m)
!! (CHKSUM STAMP)
!! 
MODULE tSuck07 STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tSuck07.%main 0 3 0
!   b := FALSE;
CONST 0
STGC tSuck07.b
LABEL L3
!     IF (b = (TRUE & TRUE)) & TRUE THEN Out.String("A") END;
LDGC tSuck07.b
JEQZ L7
CONST 2
GLOBAL tSuck07.%1
GLOBAL Out.String
CALL 2
LABEL L7
!     IF b = FALSE THEN Out.String("B") END;
LDGC tSuck07.b
JNEQZ L11
CONST 2
GLOBAL tSuck07.%2
GLOBAL Out.String
CALL 2
LABEL L11
!     b := (b = FALSE)
LDGC tSuck07.b
NOT
STGC tSuck07.b
!   UNTIL b # TRUE;
LDGC tSuck07.b
JNEQZ L3
!   Out.Ln
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tSuck07.b 1

! String "A"
DEFINE tSuck07.%1
STRING 4100

! String "B"
DEFINE tSuck07.%2
STRING 4200

! End of file
]]*)
