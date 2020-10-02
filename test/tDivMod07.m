MODULE tDivMod07;

(*<<
(-7) DIV 3 = -3
(-7) MOD 3 = 2
(-7) DIV (-3) = 2
(-7) MOD (-3) = -1
(-7) DIV 3 = -3
(-7) MOD 3 = 2
(-7) DIV (-3) = 2
(-7) MOD (-3) = -1
>>*)

IMPORT Out;

VAR failed: BOOLEAN;

PROCEDURE Test(b: INTEGER);
VAR a, q, r, q1, r1: INTEGER;
BEGIN
  FOR a := -10 TO 10 DO
    q := a DIV b; r := a MOD b;
    q1 := (-a) DIV (-b); r1 := (-a) MOD (-b);
    IF (q * b + r # a) OR (q1 * (-b) + r1 # -a) OR (q1 # q) THEN
      Out.Int(a, 0); Out.Char(' ');
      Out.Int(b, 0); Out.Char(' ');
      Out.Int(q, 0); Out.Char(' ');
      Out.Int(r, 0); Out.Char(' ');
      Out.Int(q1, 0); Out.Char(' ');
      Out.Int(r1, 0); Out.Ln;
      failed := TRUE
    END
  END
END Test;

VAR m, n: INTEGER;

BEGIN
  failed := FALSE;

  (* Run time *)
  m := -7; n := 3;
  Out.String("(-7) DIV 3 = "); Out.Int(m DIV n, 0); Out.Ln;
  Out.String("(-7) MOD 3 = "); Out.Int(m MOD n, 0); Out.Ln;
  Out.String("(-7) DIV (-3) = "); Out.Int(m DIV (-n), 0); Out.Ln;
  Out.String("(-7) MOD (-3) = "); Out.Int(m MOD (-n), 0); Out.Ln;

  (* Compile time *)
  Out.String("(-7) DIV 3 = "); Out.Int((-7) DIV 3, 0); Out.Ln;
  Out.String("(-7) MOD 3 = "); Out.Int((-7) MOD 3, 0); Out.Ln;
  Out.String("(-7) DIV (-3) = "); Out.Int((-7) DIV (-3), 0); Out.Ln;
  Out.String("(-7) MOD (-3) = "); Out.Int((-7) MOD (-3), 0); Out.Ln;

  FOR n := 1 TO 10 DO Test(n) END;
  IF failed THEN Out.String("Failed!"); Out.Ln END  
END tDivMod07.

(*[[
!! (SYMFILE #tDivMod07 STAMP #tDivMod07.%main 1 #tDivMod07.m)
!! (CHKSUM STAMP)
!! 
MODULE tDivMod07 STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tDivMod07.Test 20 3 0
! PROCEDURE Test(b: INTEGER);
!   FOR a := -10 TO 10 DO
CONST -10
STLW -4
LABEL L6
LDLW -4
CONST 10
JGT L7
!     q := a DIV b; r := a MOD b;
LDLW -4
LDLW 12
ZCHECK 22
DIV
STLW -8
LDLW -4
LDLW 12
ZCHECK 22
MOD
STLW -12
!     q1 := (-a) DIV (-b); r1 := (-a) MOD (-b);
LDLW -4
UMINUS
LDLW 12
UMINUS
ZCHECK 23
DIV
STLW -16
LDLW -4
UMINUS
LDLW 12
UMINUS
ZCHECK 23
MOD
STLW -20
!     IF (q * b + r # a) OR (q1 * (-b) + r1 # -a) OR (q1 # q) THEN
LDLW -8
LDLW 12
TIMES
LDLW -12
PLUS
LDLW -4
JNEQ L9
LDLW -16
LDLW 12
UMINUS
TIMES
LDLW -20
PLUS
LDLW -4
UMINUS
JNEQ L9
LDLW -16
LDLW -8
JEQ L10
LABEL L9
!       Out.Int(a, 0); Out.Char(' ');
CONST 0
LDLW -4
GLOBAL Out.Int
CALL 2
CONST 32
ALIGNC
GLOBAL Out.Char
CALL 1
!       Out.Int(b, 0); Out.Char(' ');
CONST 0
LDLW 12
GLOBAL Out.Int
CALL 2
CONST 32
ALIGNC
GLOBAL Out.Char
CALL 1
!       Out.Int(q, 0); Out.Char(' ');
CONST 0
LDLW -8
GLOBAL Out.Int
CALL 2
CONST 32
ALIGNC
GLOBAL Out.Char
CALL 1
!       Out.Int(r, 0); Out.Char(' ');
CONST 0
LDLW -12
GLOBAL Out.Int
CALL 2
CONST 32
ALIGNC
GLOBAL Out.Char
CALL 1
!       Out.Int(q1, 0); Out.Char(' ');
CONST 0
LDLW -16
GLOBAL Out.Int
CALL 2
CONST 32
ALIGNC
GLOBAL Out.Char
CALL 1
!       Out.Int(r1, 0); Out.Ln;
CONST 0
LDLW -20
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
!       failed := TRUE
CONST 1
STGC tDivMod07.failed
LABEL L10
!   FOR a := -10 TO 10 DO
INCL -4
JUMP L6
LABEL L7
RETURN
END

PROC tDivMod07.%main 0 3 0
!   failed := FALSE;
CONST 0
STGC tDivMod07.failed
!   m := -7; n := 3;
CONST -7
STGW tDivMod07.m
CONST 3
STGW tDivMod07.n
!   Out.String("(-7) DIV 3 = "); Out.Int(m DIV n, 0); Out.Ln;
CONST 14
GLOBAL tDivMod07.%1
GLOBAL Out.String
CALL 2
CONST 0
LDGW tDivMod07.m
LDGW tDivMod07.n
ZCHECK 43
DIV
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
!   Out.String("(-7) MOD 3 = "); Out.Int(m MOD n, 0); Out.Ln;
CONST 14
GLOBAL tDivMod07.%2
GLOBAL Out.String
CALL 2
CONST 0
LDGW tDivMod07.m
LDGW tDivMod07.n
ZCHECK 44
MOD
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
!   Out.String("(-7) DIV (-3) = "); Out.Int(m DIV (-n), 0); Out.Ln;
CONST 17
GLOBAL tDivMod07.%3
GLOBAL Out.String
CALL 2
CONST 0
LDGW tDivMod07.m
LDGW tDivMod07.n
UMINUS
ZCHECK 45
DIV
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
!   Out.String("(-7) MOD (-3) = "); Out.Int(m MOD (-n), 0); Out.Ln;
CONST 17
GLOBAL tDivMod07.%4
GLOBAL Out.String
CALL 2
CONST 0
LDGW tDivMod07.m
LDGW tDivMod07.n
UMINUS
ZCHECK 46
MOD
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
!   Out.String("(-7) DIV 3 = "); Out.Int((-7) DIV 3, 0); Out.Ln;
CONST 14
GLOBAL tDivMod07.%1
GLOBAL Out.String
CALL 2
CONST 0
CONST -3
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
!   Out.String("(-7) MOD 3 = "); Out.Int((-7) MOD 3, 0); Out.Ln;
CONST 14
GLOBAL tDivMod07.%2
GLOBAL Out.String
CALL 2
CONST 0
CONST 2
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
!   Out.String("(-7) DIV (-3) = "); Out.Int((-7) DIV (-3), 0); Out.Ln;
CONST 17
GLOBAL tDivMod07.%3
GLOBAL Out.String
CALL 2
CONST 0
CONST 2
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
!   Out.String("(-7) MOD (-3) = "); Out.Int((-7) MOD (-3), 0); Out.Ln;
CONST 17
GLOBAL tDivMod07.%4
GLOBAL Out.String
CALL 2
CONST 0
CONST -1
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
!   FOR n := 1 TO 10 DO Test(n) END;
CONST 1
STGW tDivMod07.n
LABEL L13
LDGW tDivMod07.n
CONST 10
JGT L14
LDGW tDivMod07.n
GLOBAL tDivMod07.Test
CALL 1
LDGW tDivMod07.n
INC
STGW tDivMod07.n
JUMP L13
LABEL L14
!   IF failed THEN Out.String("Failed!"); Out.Ln END  
LDGC tDivMod07.failed
JEQZ L17
CONST 8
GLOBAL tDivMod07.%5
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
LABEL L17
RETURN
END

! Global variables
GLOVAR tDivMod07.failed 1
GLOVAR tDivMod07.m 4
GLOVAR tDivMod07.n 4

! String "(-7) DIV 3 = "
DEFINE tDivMod07.%1
STRING 282D3729204449562033203D2000

! String "(-7) MOD 3 = "
DEFINE tDivMod07.%2
STRING 282D3729204D4F442033203D2000

! String "(-7) DIV (-3) = "
DEFINE tDivMod07.%3
STRING 282D37292044495620282D3329203D2000

! String "(-7) MOD (-3) = "
DEFINE tDivMod07.%4
STRING 282D3729204D4F4420282D3329203D2000

! String "Failed!"
DEFINE tDivMod07.%5
STRING 4661696C65642100

! End of file
]]*)
