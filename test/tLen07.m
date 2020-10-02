MODULE tLen07;

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
END tLen07.

(*<<
80
25
25
>>*)

(*[[
!! (SYMFILE #tLen07 STAMP #tLen07.%main 1 #tLen07.m)
!! (CHKSUM STAMP)
!! 
MODULE tLen07 STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tLen07.OutInt 0 3 0
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

PROC tLen07.Say 4 3 0x00100001
! PROCEDURE Say(bar: ARRAY OF ARRAY OF CHAR);
!   n := LEN(bar); OutInt(n);
LDLW 16
STLW -4
LDLW -4
GLOBAL tLen07.OutInt
CALL 1
!   n := LEN(bar,1); OutInt(n);
LDLW 20
STLW -4
LDLW -4
GLOBAL tLen07.OutInt
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
GLOBAL tLen07.OutInt
CALL 1
RETURN
END

PROC tLen07.%main 0 4 0
!   NEW(foo); Say(foo^)
CONST 2000
CONST 0
GLOBAL NEW
CALLW 2
STGW tLen07.foo
CONST 25
CONST 80
LDGW tLen07.foo
NCHECK 21
GLOBAL tLen07.Say
CALL 3
RETURN
END

! Global variables
GLOVAR tLen07.foo 4

! Global pointer map
DEFINE tLen07.%gcmap
WORD GC_POINTER
WORD tLen07.foo
WORD GC_END

! End of file
]]*)
