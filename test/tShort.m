MODULE tShort;

IMPORT Out;

(*<<
      8      0      8
      8      3      8
>>*)

PROCEDURE Main;
  VAR w: INTEGER; x, y, z: SHORTINT;
BEGIN
  x := 8; y := -1; z := 8;
  INC(y);
  Out.Int(x, 7); Out.Int(y, 7); Out.Int(z, 7); Out.Ln;

  w := 2; x := 8; y := -1; z := 8; 
  FOR y := -1 TO SHORT(w) DO END;
  Out.Int(x, 7); Out.Int(y, 7); Out.Int(z, 7); Out.Ln;
END Main;

BEGIN
  Main
END tShort.

(*[[
!! (SYMFILE #tShort STAMP #tShort.%main 1 #tShort.m)
!! (CHKSUM STAMP)
!! 
MODULE tShort STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tShort.Main 12 3 0
! PROCEDURE Main;
!   x := 8; y := -1; z := 8;
CONST 8
STLS -6
CONST -1
STLS -8
CONST 8
STLS -10
!   INC(y);
LDLS -8
INC
STLS -8
!   Out.Int(x, 7); Out.Int(y, 7); Out.Int(z, 7); Out.Ln;
CONST 7
LDLS -6
GLOBAL Out.Int
CALL 2
CONST 7
LDLS -8
GLOBAL Out.Int
CALL 2
CONST 7
LDLS -10
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
!   w := 2; x := 8; y := -1; z := 8; 
CONST 2
STLW -4
CONST 8
STLS -6
CONST -1
STLS -8
CONST 8
STLS -10
!   FOR y := -1 TO SHORT(w) DO END;
LDLW -4
STLS -12
CONST -1
STLS -8
LABEL L1
LDLS -8
LDLS -12
JGT L2
LDLS -8
INC
STLS -8
JUMP L1
LABEL L2
!   Out.Int(x, 7); Out.Int(y, 7); Out.Int(z, 7); Out.Ln;
CONST 7
LDLS -6
GLOBAL Out.Int
CALL 2
CONST 7
LDLS -8
GLOBAL Out.Int
CALL 2
CONST 7
LDLS -10
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

PROC tShort.%main 0 1 0
!   Main
GLOBAL tShort.Main
CALL 0
RETURN
END

! End of file
]]*)
