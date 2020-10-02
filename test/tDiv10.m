MODULE tDiv10;

IMPORT Out;

CONST B = 15;

CONST inv10* = (ASH(1, B) + 9) DIV 10;

PROCEDURE Div10(n: INTEGER): INTEGER;
BEGIN
  RETURN ASH(n * inv10, -B)
END Div10;

PROCEDURE Test;
  VAR x, y, z: INTEGER;
BEGIN
  FOR x := 0 TO ASH(1, 12) - 1 DO
    y := x DIV 10;
    z := Div10(x);
    IF y # z THEN 
      Out.Int(x, 0); Out.String(": "); Out.Int(y, 0);
      Out.String(" # "); Out.Int(z, 0); Out.Ln
    END
  END
END Test;

BEGIN
  Test;
  Out.String("Done"); Out.Ln
END tDiv10.

(*<<
Done
>>*)

(*[[
!! (SYMFILE #tDiv10 STAMP #tDiv10.%main 1 #tDiv10.m)
!! (CONST #inv10* INTEGER 3277)
!! (CHKSUM STAMP)
!! 
MODULE tDiv10 STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tDiv10.Div10 0 2 0
! PROCEDURE Div10(n: INTEGER): INTEGER;
!   RETURN ASH(n * inv10, -B)
LDLW 12
CONST 3277
TIMES
CONST 15
ASR
RETURN
END

PROC tDiv10.Test 12 3 0
! PROCEDURE Test;
!   FOR x := 0 TO ASH(1, 12) - 1 DO
CONST 0
STLW -4
LABEL L4
LDLW -4
CONST 4095
JGT L5
!     y := x DIV 10;
LDLW -4
CONST 10
DIV
STLW -8
!     z := Div10(x);
LDLW -4
GLOBAL tDiv10.Div10
CALLW 1
STLW -12
!     IF y # z THEN 
LDLW -8
LDLW -12
JEQ L8
!       Out.Int(x, 0); Out.String(": "); Out.Int(y, 0);
CONST 0
LDLW -4
GLOBAL Out.Int
CALL 2
CONST 3
GLOBAL tDiv10.%1
GLOBAL Out.String
CALL 2
CONST 0
LDLW -8
GLOBAL Out.Int
CALL 2
!       Out.String(" # "); Out.Int(z, 0); Out.Ln
CONST 4
GLOBAL tDiv10.%2
GLOBAL Out.String
CALL 2
CONST 0
LDLW -12
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
LABEL L8
!   FOR x := 0 TO ASH(1, 12) - 1 DO
INCL -4
JUMP L4
LABEL L5
RETURN
END

PROC tDiv10.%main 0 3 0
!   Test;
GLOBAL tDiv10.Test
CALL 0
!   Out.String("Done"); Out.Ln
CONST 5
GLOBAL tDiv10.%3
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

! String ": "
DEFINE tDiv10.%1
STRING 3A2000

! String " # "
DEFINE tDiv10.%2
STRING 20232000

! String "Done"
DEFINE tDiv10.%3
STRING 446F6E6500

! End of file
]]*)
