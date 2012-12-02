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
!! SYMFILE #tSuck STAMP #tSuck.%main 1
!! END STAMP
!! 
MODULE tSuck STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tSuck.%main 0 12 0
!   b := FALSE;
CONST 0
STGC tSuck.b
LABEL 3
!     IF (b = (TRUE & TRUE)) & TRUE THEN Out.String("A") END;
LDGC tSuck.b
JUMPF 5
CONST 2
CONST tSuck.%1
CONST Out.String
CALL 2
LABEL 5
!     IF b = FALSE THEN Out.String("B") END;
LDGC tSuck.b
JUMPT 7
CONST 2
CONST tSuck.%2
CONST Out.String
CALL 2
LABEL 7
!     b := (b = FALSE)
LDGC tSuck.b
NOT
STGC tSuck.b
!   UNTIL b # TRUE;
LDGC tSuck.b
JUMPT 3
!   Out.Ln
CONST Out.Ln
CALL 0
RETURN
END

! Global variables
GLOBAL tSuck.b 1

! String "A"
DEFINE tSuck.%1
STRING 4100

! String "B"
DEFINE tSuck.%2
STRING 4200

! End of file
]]*)
