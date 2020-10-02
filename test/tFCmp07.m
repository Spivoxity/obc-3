MODULE tFCmp07;

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
END tFCmp07.

(*[[
!! (SYMFILE #tFCmp07 STAMP #tFCmp07.%main 1 #tFCmp07.m)
!! (CHKSUM STAMP)
!! 
MODULE tFCmp07 STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tFCmp07.FCmp 0 2 0
! PROCEDURE FCmp(x, y: REAL): BOOLEAN;
!   RETURN x < y
LDLF 12
LDLF 16
FLT
RETURN
END

PROC tFCmp07.SCmp 0 5 0x00500001
! PROCEDURE SCmp(a, b: ARRAY OF CHAR): BOOLEAN;
!   RETURN a < b
LDLW 24
LDLW 20
LDLW 16
LDLW 12
GLOBAL COMPARE
CALLW 4
CONST 0
LT
RETURN
END

PROC tFCmp07.PBool 0 3 0
! PROCEDURE PBool(b: BOOLEAN);
!   IF b THEN
LDLC 12
JEQZ L7
!     Out.String("true")
CONST 5
GLOBAL tFCmp07.%1
GLOBAL Out.String
CALL 2
JUMP L5
LABEL L7
!     Out.String("false")
CONST 6
GLOBAL tFCmp07.%2
GLOBAL Out.String
CALL 2
LABEL L5
!   Out.Ln
GLOBAL Out.Ln
CALL 0
RETURN
END

PROC tFCmp07.%main 0 5 0
!   PBool(FCmp(-2.0, -1.0));
FCONST -1.0
FCONST -2.0
GLOBAL tFCmp07.FCmp
CALLW 2
ALIGNC
GLOBAL tFCmp07.PBool
CALL 1
!   PBool(FCmp(-1.0, -2.0));
FCONST -2.0
FCONST -1.0
GLOBAL tFCmp07.FCmp
CALLW 2
ALIGNC
GLOBAL tFCmp07.PBool
CALL 1
!   PBool(SCmp("one", "two"));
CONST 4
GLOBAL tFCmp07.%4
CONST 4
GLOBAL tFCmp07.%3
GLOBAL tFCmp07.SCmp
CALLW 4
ALIGNC
GLOBAL tFCmp07.PBool
CALL 1
!   PBool(SCmp("two", "one"))
CONST 4
GLOBAL tFCmp07.%3
CONST 4
GLOBAL tFCmp07.%4
GLOBAL tFCmp07.SCmp
CALLW 4
ALIGNC
GLOBAL tFCmp07.PBool
CALL 1
RETURN
END

! String "true"
DEFINE tFCmp07.%1
STRING 7472756500

! String "false"
DEFINE tFCmp07.%2
STRING 66616C736500

! String "one"
DEFINE tFCmp07.%3
STRING 6F6E6500

! String "two"
DEFINE tFCmp07.%4
STRING 74776F00

! End of file
]]*)
