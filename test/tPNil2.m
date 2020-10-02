MODULE tPNil2;

PROCEDURE foo(p: PROCEDURE); BEGIN p END foo;

BEGIN
  foo(NIL);
END tPNil2.

(*<<
Runtime error: null pointer error on line 3 in module tPNil2
In procedure tPNil2.foo
   called from tPNil2.%main
   called from MAIN
>>*)

(*[[
!! (SYMFILE #tPNil2 STAMP #tPNil2.%main 1 #tPNil2.m)
!! (CHKSUM STAMP)
!! 
MODULE tPNil2 STAMP 0
ENDHDR

PROC tPNil2.foo 0 1 0
! PROCEDURE foo(p: PROCEDURE); BEGIN p END foo;
LDLW 16
STATLINK
LDLW 12
NCHECK 3
CALL 0
RETURN
END

PROC tPNil2.%main 0 3 0
!   foo(NIL);
CONST 0
CONST 0
GLOBAL tPNil2.foo
CALL 2
RETURN
END

! End of file
]]*)
