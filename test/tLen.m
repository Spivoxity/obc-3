MODULE tLen;

IMPORT Out;

VAR foo: POINTER TO ARRAY 80, 25 OF CHAR;

PROCEDURE OutInt(z: INTEGER);
BEGIN 
  Out.Int(z, 0); Out.Ln
END OutInt;

PROCEDURE Say(bar: ARRAY OF ARRAY OF CHAR);
  VAR n: INTEGER;
BEGIN
  n := LEN(bar); OutInt(n);
  n := LEN(bar,1); OutInt(n);
  n := LEN(bar[0]); OutInt(n)
END Say;

BEGIN 
  NEW(foo); Say(foo^)
END tLen.

(*<<
80
25
25
>>*)

(*[[
!! (SYMFILE #tLen STAMP #tLen.%main 1 #tLen.m)
!! (CHKSUM STAMP)
!! 
MODULE tLen STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tLen.OutInt 0 3 0
! PROCEDURE OutInt(z: INTEGER);
!   Out.Int(z, 0); Out.Ln
CONST 0
LDLW 12
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

PROC tLen.Say 4 3 0
! PROCEDURE Say(bar: ARRAY OF ARRAY OF CHAR);
LOCAL 12
LDLW 16
LDLW 20
TIMES
FLEXCOPY
!   n := LEN(bar); OutInt(n);
LDLW 16
STLW -4
LDLW -4
GLOBAL tLen.OutInt
CALL 1
!   n := LEN(bar,1); OutInt(n);
LDLW 20
STLW -4
LDLW -4
GLOBAL tLen.OutInt
CALL 1
!   n := LEN(bar[0]); OutInt(n)
LDLW 12
CONST 0
LDLW 16
BOUND 17
POP 1
POP 1
LDLW 20
STLW -4
LDLW -4
GLOBAL tLen.OutInt
CALL 1
RETURN
END

PROC tLen.%main 0 4 0
!   NEW(foo); Say(foo^)
CONST 2000
CONST 0
GLOBAL NEW
CALLW 2
STGW tLen.foo
CONST 25
CONST 80
LDGW tLen.foo
NCHECK 21
GLOBAL tLen.Say
CALL 3
RETURN
END

! Global variables
GLOVAR tLen.foo 4

! Global pointer map
DEFINE tLen.%gcmap
WORD GC_POINTER
WORD tLen.foo
WORD GC_END

! End of file
]]*)
