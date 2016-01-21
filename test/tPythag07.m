MODULE tPythag07;

IMPORT Out;

CONST eps = 1.0E-10;

PROCEDURE Pythag(x, y: LONGREAL): LONGREAL;
  VAR r: LONGREAL;
BEGIN
  LOOP
    Out.LongReal(x); Out.Char(' '); Out.LongReal(y); Out.Ln;
    r := y/x; r := r * r;
    IF r < eps THEN EXIT END;
    r := r/(4.0+r);
    x := x + 2.0*x*r;
    y := y*r
  END;
  RETURN x
END Pythag;

BEGIN
  Out.LongReal(Pythag(FLT(1), FLT(1))); Out.Ln;
  Out.LongReal(Pythag(FLT(3), FLT(4))); Out.Ln
END tPythag07.

(*<<
1.00000000000 1.00000000000
1.40000000000 0.200000000000
1.41421319797 0.00101522842640
1.41421356237 1.30798116260E-10
1.41421356237
3.00000000000 4.00000000000
4.84615384615 1.23076923077
4.99996185317 0.0195311754945
5.00000000000 7.45058059692E-08
5.00000000000
>>*)

(*[[
!! SYMFILE #tPythag07 STAMP #tPythag07.%main 1
!! END STAMP
!! 
MODULE tPythag07 STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tPythag07.Pythag 8 6 0
! PROCEDURE Pythag(x, y: LONGREAL): LONGREAL;
LABEL L1
!     Out.LongReal(x); Out.Char(' '); Out.LongReal(y); Out.Ln;
LDLD 12
GLOBAL Out.LongReal
CALL 2
CONST 32
ALIGNC
GLOBAL Out.Char
CALL 1
LDLD 20
GLOBAL Out.LongReal
CALL 2
GLOBAL Out.Ln
CALL 0
!     r := y/x; r := r * r;
LDLD 20
LDLD 12
DZCHECK 12
DDIV
STLD -8
LDLD -8
LDLD -8
DTIMES
STLD -8
!     IF r < eps THEN EXIT END;
LDLD -8
DCONST 1.0e-10
DJLT L2
!     r := r/(4.0+r);
LDLD -8
LDLD -8
DCONST 4.0
DPLUS
DZCHECK 14
DDIV
STLD -8
!     x := x + 2.0*x*r;
LDLD 12
LDLD 12
DCONST 2.0
DTIMES
LDLD -8
DTIMES
DPLUS
STLD 12
!     y := y*r
LDLD 20
LDLD -8
DTIMES
STLD 20
JUMP L1
LABEL L2
!   RETURN x
LDLD 12
RETURND
END

PROC tPythag07.%main 0 6 0
!   Out.LongReal(Pythag(FLT(1), FLT(1))); Out.Ln;
DCONST 1.0
DCONST 1.0
GLOBAL tPythag07.Pythag
CALLD 4
GLOBAL Out.LongReal
CALL 2
GLOBAL Out.Ln
CALL 0
!   Out.LongReal(Pythag(FLT(3), FLT(4))); Out.Ln
DCONST 4.0
DCONST 3.0
GLOBAL tPythag07.Pythag
CALLD 4
GLOBAL Out.LongReal
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

! End of file
]]*)
