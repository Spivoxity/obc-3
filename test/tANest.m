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
!! (SYMFILE #tANest STAMP #tANest.%main 1 #tANest.m)
!! (CHKSUM STAMP)
!! 
MODULE tANest STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tANest.%1.g 4 4 0
SAVELINK
!   PROCEDURE g(); BEGIN Out.Int(a[i], 0); Out.Ln END g;
CONST 0
LDLW -4
CONST -40
OFFSET
LDLW -4
LDNW 12
CONST 10
BOUND 11
LDIW
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

PROC tANest.f 40 3 0
! PROCEDURE f(i: INTEGER; a: row);
LOCAL -40
LDLW 16
CONST 40
FIXCOPY
!   g()
LOCAL 0
STATLINK
GLOBAL tANest.%1.g
CALL 0
RETURN
END

PROC tANest.%main 0 4 0
!   b[3] := 345;
CONST 345
GLOBAL tANest.b
STNW 12
!   f(3, b)
GLOBAL tANest.b
CONST 3
GLOBAL tANest.f
CALL 2
RETURN
END

! Global variables
GLOVAR tANest.b 40

! End of file
]]*)
