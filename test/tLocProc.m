MODULE tLocProc;

VAR p: PROCEDURE;

PROCEDURE A(q: PROCEDURE);
BEGIN
  p := q
END A;

PROCEDURE Q;
  PROCEDURE R; END R;
BEGIN
  A(R)
END Q;

BEGIN
  Q
END tLocProc.

(*<<
Runtime error: assignment of local procedure on line 7 in module tLocProc
In procedure tLocProc.A
   called from tLocProc.Q
   called from tLocProc.%main
   called from MAIN
>>*)

(*[[
!! (SYMFILE #tLocProc STAMP #tLocProc.%main 1 #tLocProc.m)
!! (CHKSUM STAMP)
!! 
MODULE tLocProc STAMP 0
ENDHDR

PROC tLocProc.A 0 2 0
! PROCEDURE A(q: PROCEDURE);
!   p := q
LDLW 16
GCHECK 7
LDLW 12
STGW tLocProc.p
RETURN
END

PROC tLocProc.%1.R 4 0 0
SAVELINK
!   PROCEDURE R; END R;
RETURN
END

PROC tLocProc.Q 0 3 0
! PROCEDURE Q;
!   A(R)
LOCAL 0
GLOBAL tLocProc.%1.R
GLOBAL tLocProc.A
CALL 2
RETURN
END

PROC tLocProc.%main 0 1 0
!   Q
GLOBAL tLocProc.Q
CALL 0
RETURN
END

! Global variables
GLOVAR tLocProc.p 4

! End of file
]]*)
