MODULE tANest07;

IMPORT Out;

(* Bug in references to aggregate value parameters in an outer
   procedure from the body of an inner procedure *)

TYPE row = ARRAY 10 OF INTEGER;

PROCEDURE f(i: INTEGER; a: row);
  PROCEDURE g(); BEGIN Out.Int(a[i], 0); Out.Ln END g;
BEGIN
  g()
END f;

VAR b: row;

BEGIN
  b[3] := 345;
  f(3, b)
END tANest07.

(*<<
345
>>*)

(*[[
!! (SYMFILE #tANest07 0x00000301 #tANest07.%main 1)
!! (CHKSUM 0x274d86a1)
!! 
MODULE tANest07 STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tANest07.%1.g 4 4 0
!   PROCEDURE g(); BEGIN Out.Int(a[i], 0); Out.Ln END g;
SAVELINK
CONST 0
LDEW 16
LDEW 12
CONST 10
BOUND 11
LDIW
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

PROC tANest07.f 0 4 0x00200001
! PROCEDURE f(i: INTEGER; a: row);
!   g()
LOCAL 0
LINK
GLOBAL tANest07.%1.g
CALL 0
RETURN
END

PROC tANest07.%main 0 4 0
!   b[3] := 345;
CONST 345
GLOBAL tANest07.b
STNW 12
!   f(3, b)
GLOBAL tANest07.b
CONST 3
GLOBAL tANest07.f
CALL 2
RETURN
END

! Global variables
GLOVAR tANest07.b 40

! End of file
]]*)
