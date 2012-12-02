MODULE tFCmp;

(*<<
true
false
true
false
>>*)

IMPORT Out;

PROCEDURE FCmp(x, y: REAL): BOOLEAN;
BEGIN
  RETURN x < y
END FCmp;

PROCEDURE SCmp(a, b: ARRAY OF CHAR): BOOLEAN;
BEGIN
  RETURN a < b
END SCmp;

PROCEDURE PBool(b: BOOLEAN);
BEGIN
  IF b THEN
    Out.String("true")
  ELSE
    Out.String("false")
  END;
  Out.Ln
END PBool;

BEGIN
  (* Use negative numbers so float and integer comparisons disagree *)
  PBool(FCmp(-2.0, -1.0));
  PBool(FCmp(-1.0, -2.0));
  PBool(SCmp("one", "two"));
  PBool(SCmp("two", "one"))
END tFCmp.

(*[[
!! SYMFILE #tFCmp STAMP #tFCmp.%main 1
!! END STAMP
!! 
MODULE tFCmp STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tFCmp.FCmp 0 8 0
! PROCEDURE FCmp(x, y: REAL): BOOLEAN;
!   RETURN x < y
LDLF 12
LDLF 16
FLT
RETURNW
END

PROC tFCmp.SCmp 0 20 0
! PROCEDURE SCmp(a, b: ARRAY OF CHAR): BOOLEAN;
LOCAL 12
LDLW 16
FLEXCOPY
LOCAL 20
LDLW 24
FLEXCOPY
!   RETURN a < b
LDLW 24
LDLW 20
LDLW 16
LDLW 12
CONST COMPARE
CALLW 4
CONST 0
LT
RETURNW
END

PROC tFCmp.PBool 0 20 0
! PROCEDURE PBool(b: BOOLEAN);
!   IF b THEN
LDLC 12
JUMPF 6
!     Out.String("true")
CONST 5
CONST tFCmp.%1
CONST Out.String
CALL 2
JUMP 5
LABEL 6
!     Out.String("false")
CONST 6
CONST tFCmp.%2
CONST Out.String
CALL 2
LABEL 5
!   Out.Ln
CONST Out.Ln
CALL 0
RETURN
END

PROC tFCmp.%main 0 20 0
!   PBool(FCmp(-2.0, -1.0));
FCONST -1.
FCONST -2.
CONST tFCmp.FCmp
CALLW 2
ALIGNC
CONST tFCmp.PBool
CALL 1
!   PBool(FCmp(-1.0, -2.0));
FCONST -2.
FCONST -1.
CONST tFCmp.FCmp
CALLW 2
ALIGNC
CONST tFCmp.PBool
CALL 1
!   PBool(SCmp("one", "two"));
CONST 4
CONST tFCmp.%4
CONST 4
CONST tFCmp.%3
CONST tFCmp.SCmp
CALLW 4
ALIGNC
CONST tFCmp.PBool
CALL 1
!   PBool(SCmp("two", "one"))
CONST 4
CONST tFCmp.%3
CONST 4
CONST tFCmp.%4
CONST tFCmp.SCmp
CALLW 4
ALIGNC
CONST tFCmp.PBool
CALL 1
RETURN
END

! String "true"
DEFINE tFCmp.%1
STRING 7472756500

! String "false"
DEFINE tFCmp.%2
STRING 66616C736500

! String "one"
DEFINE tFCmp.%3
STRING 6F6E6500

! String "two"
DEFINE tFCmp.%4
STRING 74776F00

! End of file
]]*)
