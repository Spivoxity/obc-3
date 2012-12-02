MODULE tANest;

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
END tANest.

(*<<
345
>>*)

(*[[
!! SYMFILE #tANest STAMP #tANest.%main 1
!! END STAMP
!! 
MODULE tANest STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tANest.%1.g 1 16 0
!   PROCEDURE g(); BEGIN Out.Int(a[i], 0); Out.Ln END g;
SAVELINK
CONST 0
LDLW -4
CONST -40
PLUSA
LDEW 12
CONST 10
BOUND 11
LDIW
CONST Out.Int
CALL 2
CONST Out.Ln
CALL 0
RETURN
END

PROC tANest.f 10 16 0
! PROCEDURE f(i: INTEGER; a: row);
LOCAL -40
LDLW 16
CONST 40
FIXCOPY
!   g()
LOCAL 0
LINK
CONST tANest.%1.g
CALL 0
RETURN
END

PROC tANest.%main 0 16 0
!   b[3] := 345;
CONST 345
CONST tANest.b
STNW 12
!   f(3, b)
CONST tANest.b
CONST 3
CONST tANest.f
CALL 2
RETURN
END

! Global variables
GLOBAL tANest.b 40

! End of file
]]*)
