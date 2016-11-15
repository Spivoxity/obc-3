MODULE tDive;

(*<<
Runtime error: stack overflow in module tDive
In procedure tDive.p
   called from tDive.p
   called from tDive.p
   called from tDive.p
   called from tDive.p
   called from tDive.p
   ... 87336 intervening frames omitted ...
   called from tDive.p
   called from tDive.p
   called from tDive.p
   called from tDive.%main
   called from MAIN
>>*)

PROCEDURE p;
BEGIN
  p; p
END p;

BEGIN
  p
END tDive.

(*[[
!! (SYMFILE #tDive 0x00000301 #tDive.%main 1)
!! (CHKSUM 0x28e9671d)
!! 
MODULE tDive STAMP 0
ENDHDR

PROC tDive.p 0 1 0
! PROCEDURE p;
!   p; p
GLOBAL tDive.p
CALL 0
GLOBAL tDive.p
CALL 0
RETURN
END

PROC tDive.%main 0 1 0
!   p
GLOBAL tDive.p
CALL 0
RETURN
END

! End of file
]]*)
