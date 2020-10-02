MODULE tCompLong;

(* Comparison of LONGREAL with Boolean result *)

IMPORT Out;

PROCEDURE one(x, y: REAL): BOOLEAN;
BEGIN
  RETURN (x >= y)
END one;

PROCEDURE two(x, y: LONGREAL): BOOLEAN;
BEGIN
  RETURN (x >= y)
END two;

PROCEDURE three(x, y: LONGREAL): BOOLEAN;
BEGIN
  IF x >= y THEN
    RETURN TRUE
  ELSE
    RETURN FALSE
  END
END three;

BEGIN
  IF one(3.0, 2.0) THEN Out.String("one"); Out.Ln END;
  IF two(3.0, 2.0) THEN Out.String("two"); Out.Ln END;
  IF three(3.0, 2.0) THEN Out.String("three"); Out.Ln END
END tCompLong.

(*<<
one
two
three
>>*)

(*[[
!! (SYMFILE #tCompLong STAMP #tCompLong.%main 1 #tCompLong.m)
!! (CHKSUM STAMP)
!! 
MODULE tCompLong STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tCompLong.one 0 2 0
! PROCEDURE one(x, y: REAL): BOOLEAN;
!   RETURN (x >= y)
LDLF 12
LDLF 16
FGEQ
RETURN
END

PROC tCompLong.two 0 4 0
! PROCEDURE two(x, y: LONGREAL): BOOLEAN;
!   RETURN (x >= y)
LDLD 12
LDLD 20
DGEQ
RETURN
END

PROC tCompLong.three 0 4 0
! PROCEDURE three(x, y: LONGREAL): BOOLEAN;
!   IF x >= y THEN
LDLD 12
LDLD 20
DJNGEQ L6
!     RETURN TRUE
CONST 1
RETURN
LABEL L6
!     RETURN FALSE
CONST 0
RETURN
END

PROC tCompLong.%main 0 5 0
!   IF one(3.0, 2.0) THEN Out.String("one"); Out.Ln END;
FCONST 2.0
FCONST 3.0
GLOBAL tCompLong.one
CALLW 2
JEQZ L9
CONST 4
GLOBAL tCompLong.%1
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
LABEL L9
!   IF two(3.0, 2.0) THEN Out.String("two"); Out.Ln END;
DCONST 2.0
DCONST 3.0
GLOBAL tCompLong.two
CALLW 4
JEQZ L12
CONST 4
GLOBAL tCompLong.%2
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
LABEL L12
!   IF three(3.0, 2.0) THEN Out.String("three"); Out.Ln END
DCONST 2.0
DCONST 3.0
GLOBAL tCompLong.three
CALLW 4
JEQZ L15
CONST 6
GLOBAL tCompLong.%3
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
LABEL L15
RETURN
END

! String "one"
DEFINE tCompLong.%1
STRING 6F6E6500

! String "two"
DEFINE tCompLong.%2
STRING 74776F00

! String "three"
DEFINE tCompLong.%3
STRING 746872656500

! End of file
]]*)
