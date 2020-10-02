MODULE tDive07;

(*<<
Runtime error: stack overflow in module tDive07
In procedure tDive07.p
   called from tDive07.p
   called from tDive07.p
   called from tDive07.p
   called from tDive07.p
   called from tDive07.p
   ... 87336 intervening frames omitted ...
   called from tDive07.p
   called from tDive07.p
   called from tDive07.p
   called from tDive07.%main
   called from MAIN
>>*)

PROCEDURE p;
BEGIN
  p; p
END p;

BEGIN
  p
END tDive07.

(*[[
!! (SYMFILE #tDive07 STAMP #tDive07.%main 1 #tDive07.m)
!! (CHKSUM STAMP)
!! 
MODULE tDive07 STAMP 0
ENDHDR

PROC tDive07.p 0 1 0
! PROCEDURE p;
!   p; p
GLOBAL tDive07.p
CALL 0
GLOBAL tDive07.p
CALL 0
RETURN
END

PROC tDive07.%main 0 1 0
!   p
GLOBAL tDive07.p
CALL 0
RETURN
END

! End of file
]]*)
