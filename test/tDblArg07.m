MODULE tDblArg07;

IMPORT Out;

PROCEDURE f(b: BOOLEAN; x: LONGREAL); 
BEGIN
  IF b THEN Out.String("*** Failed!") ELSE Out.LongReal(x) END; 
  Out.Ln
END f;

VAR p, q: INTEGER; m: BOOLEAN;

BEGIN
  p := 3; q := 2;
  m := (p < q) & (q DIV 0 > 2);
  f((p < q) & (q DIV 2 > 0), 3.14)
END tDblArg07.

(*<<
3.14000000000
>>*)

(*[[
!! (SYMFILE #tDblArg07 STAMP #tDblArg07.%main 1 #tDblArg07.m)
!! (CHKSUM STAMP)
!! 
MODULE tDblArg07 STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tDblArg07.f 0 3 0
! PROCEDURE f(b: BOOLEAN; x: LONGREAL); 
!   IF b THEN Out.String("*** Failed!") ELSE Out.LongReal(x) END; 
LDLC 12
JEQZ L4
CONST 12
GLOBAL tDblArg07.%1
GLOBAL Out.String
CALL 2
JUMP L2
LABEL L4
LDLD 16
GLOBAL Out.LongReal
CALL 2
LABEL L2
!   Out.Ln
GLOBAL Out.Ln
CALL 0
RETURN
END

PROC tDblArg07.%main 0 4 0
!   p := 3; q := 2;
CONST 3
STGW tDblArg07.p
CONST 2
STGW tDblArg07.q
!   m := (p < q) & (q DIV 0 > 2);
LDGW tDblArg07.p
LDGW tDblArg07.q
JLT L6
CONST 0
JUMP L7
LABEL L6
LDGW tDblArg07.q
CONST 0
ZCHECK 15
DIV
CONST 2
GT
LABEL L7
STGC tDblArg07.m
!   f((p < q) & (q DIV 2 > 0), 3.14)
DCONST 3.14
LDGW tDblArg07.p
LDGW tDblArg07.q
JLT L9
CONST 0
JUMP L10
LABEL L9
LDGW tDblArg07.q
CONST 2
DIV
CONST 0
GT
LABEL L10
ALIGNC
GLOBAL tDblArg07.f
CALL 3
RETURN
END

! Global variables
GLOVAR tDblArg07.p 4
GLOVAR tDblArg07.q 4
GLOVAR tDblArg07.m 1

! String "*** Failed!"
DEFINE tDblArg07.%1
STRING 2A2A2A204661696C65642100

! End of file
]]*)
