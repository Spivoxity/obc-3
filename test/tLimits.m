MODULE tLimits;

(*<<
5
-32768
32767
-2147483648
2147483647
-2147483648
3.40282E+38
1.79769313486E+308
1073741823
-1073741824
444
Pass
>>*)

IMPORT Out, xPrelude;

TYPE t = ARRAY 37 OF RECORD x: INTEGER; d: LONGREAL END;

CONST kbool = FALSE;

PROCEDURE TestAsh(x, n: INTEGER);
BEGIN
  Out.Int(ASH(x, n), 0); Out.Ln
END TestAsh;

VAR mymax: LONGREAL;

(* Make sure MAX(LONGREAL) and xPrelude.maxlong are mentioned in
different procedures; otherwise the compiler optimizes away the
comparison, or merges the two constants *)
PROCEDURE SetMax; BEGIN mymax := MAX(LONGREAL) END SetMax;

PROCEDURE MaxReal(): REAL;
  VAR i, j: INTEGER; x: REAL;
BEGIN
  (* Need to defeat constant folding here too *)
  i := 0FFFFFFH; j := 800000H;
  x := i / j;
  FOR i := 1 TO 127 DO x := 2 * x END;
  RETURN x
END MaxReal;

BEGIN
  Out.Int(ASH(20, -2), 0); Out.Ln;

  Out.Int(MIN(SHORTINT), 0); Out.Ln;
  Out.Int(MAX(SHORTINT), 0); Out.Ln;

  Out.Int(MIN(INTEGER), 0); Out.Ln;
  Out.Int(MAX(INTEGER), 0); Out.Ln;
  Out.Int(-2147483648, 0); Out.Ln;

  Out.Real(MAX(REAL)); Out.Ln;
  Out.LongReal(MAX(LONGREAL)); Out.Ln;

  IF MAX(REAL) # MaxReal() THEN
    Out.String("Fail: MAX(REAL)"); Out.Ln
  END;

  SetMax;
  IF xPrelude.maxlong # mymax THEN 
    Out.String("Fail: MAX(LONG)"); Out.Ln
  END;

  TestAsh(MAX(INTEGER), -1);
  TestAsh(MIN(INTEGER), -1);

  Out.Int(SIZE(t), 0); Out.Ln;

  (* Lexer bug: digit string followed by ".." should return DECIMAL *)
  CASE MIN(INTEGER) OF 
    -2147483648..0: Out.String("Pass")
  ELSE
    Out.String("Fail")
  END;
  Out.Ln;

  (* Jump optimization to delete unused code *)
  IF kbool THEN
    Out.String("Fail")
  END
END tLimits.

(*[[
!! (SYMFILE #tLimits STAMP #tLimits.%main 1 #tLimits.m)
!! (CHKSUM STAMP)
!! 
MODULE tLimits STAMP 0
IMPORT Out STAMP
IMPORT xPrelude STAMP
ENDHDR

PROC tLimits.TestAsh 0 4 0
! PROCEDURE TestAsh(x, n: INTEGER);
!   Out.Int(ASH(x, n), 0); Out.Ln
CONST 0
LDLW 16
LDLW 12
GLOBAL ASH
CALLW 2
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

PROC tLimits.SetMax 0 3 0
! PROCEDURE SetMax; BEGIN mymax := MAX(LONGREAL) END SetMax;
DCONST 1.79769313486e+308
STGD tLimits.mymax
RETURN
END

PROC tLimits.MaxReal 12 2 0
! PROCEDURE MaxReal(): REAL;
!   i := 0FFFFFFH; j := 800000H;
CONST 16777215
STLW -4
CONST 8388608
STLW -8
!   x := i / j;
LDLW -4
CONVNF
LDLW -8
CONVNF
FZCHECK 41
FDIV
STLF -12
!   FOR i := 1 TO 127 DO x := 2 * x END;
CONST 1
STLW -4
LABEL L6
LDLW -4
CONST 127
JGT L7
LDLF -12
FCONST 2.0
FTIMES
STLF -12
INCL -4
JUMP L6
LABEL L7
!   RETURN x
LDLF -12
RETURN
END

PROC tLimits.%main 0 4 0
!   Out.Int(ASH(20, -2), 0); Out.Ln;
CONST 0
CONST 5
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
!   Out.Int(MIN(SHORTINT), 0); Out.Ln;
CONST 0
CONST -32768
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
!   Out.Int(MAX(SHORTINT), 0); Out.Ln;
CONST 0
CONST 32767
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
!   Out.Int(MIN(INTEGER), 0); Out.Ln;
CONST 0
CONST -2147483648
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
!   Out.Int(MAX(INTEGER), 0); Out.Ln;
CONST 0
CONST 2147483647
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
!   Out.Int(-2147483648, 0); Out.Ln;
CONST 0
CONST -2147483648
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
!   Out.Real(MAX(REAL)); Out.Ln;
FCONST 3.40282346639e+38
GLOBAL Out.Real
CALL 1
GLOBAL Out.Ln
CALL 0
!   Out.LongReal(MAX(LONGREAL)); Out.Ln;
DCONST 1.79769313486e+308
GLOBAL Out.LongReal
CALL 2
GLOBAL Out.Ln
CALL 0
!   IF MAX(REAL) # MaxReal() THEN
GLOBAL tLimits.MaxReal
CALLF 0
FCONST 3.40282346639e+38
FJEQ L10
!     Out.String("Fail: MAX(REAL)"); Out.Ln
CONST 16
GLOBAL tLimits.%1
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
LABEL L10
!   SetMax;
GLOBAL tLimits.SetMax
CALL 0
!   IF xPrelude.maxlong # mymax THEN 
LDGD tLimits.mymax
DCONST 1.79769313486e+308
DJEQ L13
!     Out.String("Fail: MAX(LONG)"); Out.Ln
CONST 16
GLOBAL tLimits.%2
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
LABEL L13
!   TestAsh(MAX(INTEGER), -1);
CONST -1
CONST 2147483647
GLOBAL tLimits.TestAsh
CALL 2
!   TestAsh(MIN(INTEGER), -1);
CONST -1
CONST -2147483648
GLOBAL tLimits.TestAsh
CALL 2
!   Out.Int(SIZE(t), 0); Out.Ln;
CONST 0
CONST 444
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
!     -2147483648..0: Out.String("Pass")
CONST 5
GLOBAL tLimits.%3
GLOBAL Out.String
CALL 2
!   Out.Ln;
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tLimits.mymax 8

! String "Fail: MAX(REAL)"
DEFINE tLimits.%1
STRING 4661696C3A204D4158285245414C2900

! String "Fail: MAX(LONG)"
DEFINE tLimits.%2
STRING 4661696C3A204D4158284C4F4E472900

! String "Pass"
DEFINE tLimits.%3
STRING 5061737300

! String "Fail"
DEFINE tLimits.%4
STRING 4661696C00

! Descriptor for *anon*
DEFINE tLimits.%5
WORD 0
WORD 0
WORD tLimits.%5.%anc

DEFINE tLimits.%5.%anc
WORD tLimits.%5

! End of file
]]*)
