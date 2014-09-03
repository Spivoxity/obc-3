MODULE tFibConv;

(* Sloane A001629 *)

IMPORT Out;

PROCEDURE FibConv(n: INTEGER): INTEGER;
  VAR k, x, y, u, v: INTEGER;
BEGIN
  k := 0;
  x, y, u, v := 0, 1, 0, 0;
  WHILE k < n DO
    k := k+1;
    x, y, u, v := y, x+y, v, u+v+y
  END;
  RETURN u
END FibConv;

BEGIN
  Out.Int(FibConv(10), 0); Out.Ln
END tFibConv.

(*<<
235
>>*)

(*[[
!! SYMFILE #tFibConv STAMP #tFibConv.%main 1
!! END STAMP
!! 
MODULE tFibConv STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tFibConv.FibConv 20 8 0
! PROCEDURE FibConv(n: INTEGER): INTEGER;
!   k := 0;
CONST 0
STLW -4
!   x, y, u, v := 0, 1, 0, 0;
CONST 0
LOCAL -8
CONST 1
LOCAL -12
CONST 0
CONST 0
STLW -20
STLW -16
STOREW
STOREW
LABEL 1
!   WHILE k < n DO
LDLW -4
LDLW 12
JGEQ 3
!     k := k+1;
INCL -4
!     x, y, u, v := y, x+y, v, u+v+y
LDLW -12
LOCAL -8
LDLW -8
LDLW -12
PLUS
LOCAL -12
LDLW -20
LOCAL -16
LDLW -16
LDLW -20
PLUS
LDLW -12
PLUS
STLW -20
STOREW
STOREW
STOREW
JUMP 1
LABEL 3
!   RETURN u
LDLW -16
RETURNW
END

PROC tFibConv.%main 0 8 0
!   Out.Int(FibConv(10), 0); Out.Ln
CONST 0
CONST 10
GLOBAL tFibConv.FibConv
CALLW 1
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

! End of file
]]*)
  
