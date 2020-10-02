MODULE tNasty07;

IMPORT Out;

(* 
The JIT translator produces the result 7 by delaying the
load from y until after the call of Nasty.  The bytecode interpreter
produces 11 as the result.
*)

(*<<
OK
>>*)

VAR x, y, z: INTEGER;

PROCEDURE Nasty(x: INTEGER): INTEGER;
BEGIN
  y := 1;
  RETURN 2 * x
END Nasty;

BEGIN
  x := 3; y := 5;
  z := y + Nasty(x);
  IF (z = 7) OR (z = 11) THEN Out.String("OK"); Out.Ln END
END tNasty07.

(*[[
!! (SYMFILE #tNasty07 STAMP #tNasty07.%main 1 #tNasty07.m)
!! (CHKSUM STAMP)
!! 
MODULE tNasty07 STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tNasty07.Nasty 0 2 0
! PROCEDURE Nasty(x: INTEGER): INTEGER;
!   y := 1;
CONST 1
STGW tNasty07.y
!   RETURN 2 * x
LDLW 12
CONST 2
TIMES
RETURN
END

PROC tNasty07.%main 0 3 0
!   x := 3; y := 5;
CONST 3
STGW tNasty07.x
CONST 5
STGW tNasty07.y
!   z := y + Nasty(x);
LDGW tNasty07.y
LDGW tNasty07.x
GLOBAL tNasty07.Nasty
CALLW 1
PLUS
STGW tNasty07.z
!   IF (z = 7) OR (z = 11) THEN Out.String("OK"); Out.Ln END
LDGW tNasty07.z
CONST 7
JEQ L3
LDGW tNasty07.z
CONST 11
JNEQ L4
LABEL L3
CONST 3
GLOBAL tNasty07.%1
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
LABEL L4
RETURN
END

! Global variables
GLOVAR tNasty07.x 4
GLOVAR tNasty07.y 4
GLOVAR tNasty07.z 4

! String "OK"
DEFINE tNasty07.%1
STRING 4F4B00

! End of file
]]*)
