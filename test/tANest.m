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
!! (SYMFILE #tANest 0x00000301 #tANest.%main 1)
!! (CHKSUM 0x1a165943)
!! 
MODULE tANest STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tANest.%1.g 4 4 0
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
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

PROC tANest.f 40 4 0
! PROCEDURE f(i: INTEGER; a: row);
LOCAL -40
LDLW 16
CONST 40
FIXCOPY
!   g()
LOCAL 0
LINK
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
