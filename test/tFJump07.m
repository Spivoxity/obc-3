MODULE tFJump07;

(*<<
ok
>>*)

IMPORT Out;

VAR x: REAL;

BEGIN
  x := 1.0;
  IF 0.0 < x THEN
    IF x > 0.5 THEN
      Out.String("ok"); Out.Ln
    END
  END
END tFJump07.

(*[[
!! (SYMFILE #tFJump07 STAMP #tFJump07.%main 1 #tFJump07.m)
!! (CHKSUM STAMP)
!! 
MODULE tFJump07 STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tFJump07.%main 0 3 0
!   x := 1.0;
FCONST 1.0
STGF tFJump07.x
!   IF 0.0 < x THEN
LDGF tFJump07.x
FCONST 0.0
FJNGT L4
!     IF x > 0.5 THEN
LDGF tFJump07.x
FCONST 0.5
FJNGT L4
!       Out.String("ok"); Out.Ln
CONST 3
GLOBAL tFJump07.%1
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
LABEL L4
RETURN
END

! Global variables
GLOVAR tFJump07.x 4

! String "ok"
DEFINE tFJump07.%1
STRING 6F6B00

! End of file
]]*)
