MODULE tMob07;

IMPORT Out;

TYPE Fun = PROCEDURE (): REAL;

PROCEDURE A(k: INTEGER; x1, x2, x3, x4, x5: Fun): REAL;
  PROCEDURE B(): REAL;
  BEGIN
    k := k-1;
    RETURN A(k, B, x1, x2, x3, x4)
  END B;
VAR r: REAL;
BEGIN
  IF k <= 0 THEN r := x4() + x5() ELSE r := B() END
RETURN r
END A;

PROCEDURE One(): REAL; BEGIN RETURN FLT(1) END One;
PROCEDURE MOne(): REAL; BEGIN RETURN FLT(-1) END MOne;
PROCEDURE Zero(): REAL; BEGIN RETURN FLT(0) END Zero;

BEGIN
  Out.Real(A(10, One, MOne, MOne, One, Zero)); Out.Ln
END tMob07.

(*<<
-67.0000
>>*)

(*[[
!! (SYMFILE #tMob07 0x00000301 #tMob07.%main 1)
!! (CHKSUM 0x0d2ae8f7)
!! 
MODULE tMob07 STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tMob07.%1.B 4 12 0
!   PROCEDURE B(): REAL;
SAVELINK
!     k := k-1;
LDEW 12
DEC
STEW 12
!     RETURN A(k, B, x1, x2, x3, x4)
LDEW 44
LDEW 40
LDEW 36
LDEW 32
LDEW 28
LDEW 24
LDEW 20
LDEW 16
LDLW -4
GLOBAL tMob07.%1.B
LDEW 12
GLOBAL tMob07.A
CALLF 11
RETURNF
END

PROC tMob07.A 4 12 0
! PROCEDURE A(k: INTEGER; x1, x2, x3, x4, x5: Fun): REAL;
!   IF k <= 0 THEN r := x4() + x5() ELSE r := B() END
LDLW 12
JGTZ L4
LDLW 44
LINK
LDLW 40
NCHECK 15
CALLF 0
LDLW 52
LINK
LDLW 48
NCHECK 15
CALLF 0
FPLUS
STLF -4
JUMP L2
LABEL L4
LOCAL 0
LINK
GLOBAL tMob07.%1.B
CALLF 0
STLF -4
LABEL L2
! RETURN r
LDLF -4
RETURNF
END

PROC tMob07.One 0 12 0
! PROCEDURE One(): REAL; BEGIN RETURN FLT(1) END One;
FCONST 1.0
RETURNF
END

PROC tMob07.MOne 0 12 0
! PROCEDURE MOne(): REAL; BEGIN RETURN FLT(-1) END MOne;
FCONST -1.0
RETURNF
END

PROC tMob07.Zero 0 12 0
! PROCEDURE Zero(): REAL; BEGIN RETURN FLT(0) END Zero;
FCONST 0.0
RETURNF
END

PROC tMob07.%main 0 12 0
!   Out.Real(A(10, One, MOne, MOne, One, Zero)); Out.Ln
CONST 0
GLOBAL tMob07.Zero
CONST 0
GLOBAL tMob07.One
CONST 0
GLOBAL tMob07.MOne
CONST 0
GLOBAL tMob07.MOne
CONST 0
GLOBAL tMob07.One
CONST 10
GLOBAL tMob07.A
CALLF 11
GLOBAL Out.Real
CALL 1
GLOBAL Out.Ln
CALL 0
RETURN
END

! End of file
]]*)
