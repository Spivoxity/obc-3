MODULE tPNil207;

PROCEDURE foo(p: PROCEDURE); BEGIN p END foo;

BEGIN
  foo(NIL);
END tPNil207.

(*<<
Runtime error: null pointer error on line 3 in module tPNil207
In procedure tPNil207.foo
   called from tPNil207.%main
   called from MAIN
>>*)

(*[[
!! (SYMFILE #tPNil207 STAMP #tPNil207.%main 1)
!! (CHKSUM STAMP)
!! 
MODULE tPNil207 STAMP 0
ENDHDR

PROC tPNil207.foo 0 2 0
! PROCEDURE foo(p: PROCEDURE); BEGIN p END foo;
LDLW 12
NCHECK 3
LDLW 16
LCALL 0
RETURN
END

PROC tPNil207.%main 0 4 0
!   foo(NIL);
CONST 0
CONST 0
GLOBAL tPNil207.foo
CALL 2
RETURN
END

! End of file
]]*)
