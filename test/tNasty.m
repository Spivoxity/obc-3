MODULE tNasty;

IMPORT Out;

(* 
The JIT translator produces the result 7 by delaying the
load from y until after the call of Nasty.  The bytecode interpreter
produces 11 as the result.
*)

(*<<
7
>>*)

VAR x, y, z: INTEGER;

PROCEDURE Nasty(x: INTEGER): INTEGER;
BEGIN
  y := 1;
  RETURN 2 * x;
END Nasty;

BEGIN
  x := 3; y := 5;
  z := y + Nasty(x);
  Out.Int(z, 0); Out.Ln
END tNasty.

(*[[
!! SYMFILE #tNasty STAMP #tNasty.%main 1
!! END STAMP
!! 
MODULE tNasty STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tNasty.Nasty 0 2 0
! PROCEDURE Nasty(x: INTEGER): INTEGER;
!   y := 1;
CONST 1
STGW tNasty.y
!   RETURN 2 * x;
LDLW 12
CONST 2
TIMES
RETURNW
END

PROC tNasty.%main 0 3 0
!   x := 3; y := 5;
CONST 3
STGW tNasty.x
CONST 5
STGW tNasty.y
!   z := y + Nasty(x);
LDGW tNasty.y
LDGW tNasty.x
GLOBAL tNasty.Nasty
CALLW 1
PLUS
STGW tNasty.z
!   Out.Int(z, 0); Out.Ln
CONST 0
LDGW tNasty.z
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tNasty.x 4
GLOVAR tNasty.y 4
GLOVAR tNasty.z 4

! End of file
]]*)
