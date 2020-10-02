MODULE tOver07;

(*<<
1.000000
0.500000
0.333333
0.250000
2.000000
1.000000
0.666667
0.500000
3.000000
1.500000
1.000000
0.750000
4.000000
2.000000
1.333333
1.000000
>>*)

IMPORT Out;

PROCEDURE Print(x: LONGREAL);
BEGIN
  Out.Fixed(x, 0, 6); Out.Ln
END Print;

VAR s: SHORTINT; n: INTEGER; f: REAL; d, x: LONGREAL;

BEGIN
  s := 1; n := 2; f := FLT(3); d := FLT(4);
  x := FLT(s)/FLT(s); Print(x);
  x := FLT(s)/FLT(n); Print(x);
  x := FLT(s)/f; Print(x);
  x := FLT(s)/d; Print(x);
  x := FLT(n)/FLT(s); Print(x);
  x := FLT(n)/FLT(n); Print(x);
  x := FLT(n)/f; Print(x);
  x := FLT(n)/d; Print(x);
  x := f/FLT(s); Print(x);
  x := f/FLT(n); Print(x);
  x := f/f; Print(x);
  x := f/d; Print(x);
  x := d/FLT(s); Print(x);
  x := d/FLT(n); Print(x);
  x := d/f; Print(x);
  x := d/d; Print(x);
END tOver07.

(*[[
!! (SYMFILE #tOver07 STAMP #tOver07.%main 1 #tOver07.m)
!! (CHKSUM STAMP)
!! 
MODULE tOver07 STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tOver07.Print 0 5 0
! PROCEDURE Print(x: LONGREAL);
!   Out.Fixed(x, 0, 6); Out.Ln
CONST 6
CONST 0
LDLD 12
GLOBAL Out.Fixed
CALL 4
GLOBAL Out.Ln
CALL 0
RETURN
END

PROC tOver07.%main 0 4 0
!   s := 1; n := 2; f := FLT(3); d := FLT(4);
CONST 1
STGS tOver07.s
CONST 2
STGW tOver07.n
FCONST 3.0
STGF tOver07.f
DCONST 4.0
STGD tOver07.d
!   x := FLT(s)/FLT(s); Print(x);
LDGS tOver07.s
CONVNF
LDGS tOver07.s
CONVNF
FZCHECK 33
FDIV
CONVFD
STGD tOver07.x
LDGD tOver07.x
GLOBAL tOver07.Print
CALL 2
!   x := FLT(s)/FLT(n); Print(x);
LDGS tOver07.s
CONVNF
LDGW tOver07.n
CONVNF
FZCHECK 34
FDIV
CONVFD
STGD tOver07.x
LDGD tOver07.x
GLOBAL tOver07.Print
CALL 2
!   x := FLT(s)/f; Print(x);
LDGS tOver07.s
CONVNF
LDGF tOver07.f
FZCHECK 35
FDIV
CONVFD
STGD tOver07.x
LDGD tOver07.x
GLOBAL tOver07.Print
CALL 2
!   x := FLT(s)/d; Print(x);
LDGS tOver07.s
CONVND
LDGD tOver07.d
DZCHECK 36
DDIV
STGD tOver07.x
LDGD tOver07.x
GLOBAL tOver07.Print
CALL 2
!   x := FLT(n)/FLT(s); Print(x);
LDGW tOver07.n
CONVNF
LDGS tOver07.s
CONVNF
FZCHECK 37
FDIV
CONVFD
STGD tOver07.x
LDGD tOver07.x
GLOBAL tOver07.Print
CALL 2
!   x := FLT(n)/FLT(n); Print(x);
LDGW tOver07.n
CONVNF
LDGW tOver07.n
CONVNF
FZCHECK 38
FDIV
CONVFD
STGD tOver07.x
LDGD tOver07.x
GLOBAL tOver07.Print
CALL 2
!   x := FLT(n)/f; Print(x);
LDGW tOver07.n
CONVNF
LDGF tOver07.f
FZCHECK 39
FDIV
CONVFD
STGD tOver07.x
LDGD tOver07.x
GLOBAL tOver07.Print
CALL 2
!   x := FLT(n)/d; Print(x);
LDGW tOver07.n
CONVND
LDGD tOver07.d
DZCHECK 40
DDIV
STGD tOver07.x
LDGD tOver07.x
GLOBAL tOver07.Print
CALL 2
!   x := f/FLT(s); Print(x);
LDGF tOver07.f
LDGS tOver07.s
CONVNF
FZCHECK 41
FDIV
CONVFD
STGD tOver07.x
LDGD tOver07.x
GLOBAL tOver07.Print
CALL 2
!   x := f/FLT(n); Print(x);
LDGF tOver07.f
LDGW tOver07.n
CONVNF
FZCHECK 42
FDIV
CONVFD
STGD tOver07.x
LDGD tOver07.x
GLOBAL tOver07.Print
CALL 2
!   x := f/f; Print(x);
LDGF tOver07.f
LDGF tOver07.f
FZCHECK 43
FDIV
CONVFD
STGD tOver07.x
LDGD tOver07.x
GLOBAL tOver07.Print
CALL 2
!   x := f/d; Print(x);
LDGF tOver07.f
CONVFD
LDGD tOver07.d
DZCHECK 44
DDIV
STGD tOver07.x
LDGD tOver07.x
GLOBAL tOver07.Print
CALL 2
!   x := d/FLT(s); Print(x);
LDGD tOver07.d
LDGS tOver07.s
CONVND
DZCHECK 45
DDIV
STGD tOver07.x
LDGD tOver07.x
GLOBAL tOver07.Print
CALL 2
!   x := d/FLT(n); Print(x);
LDGD tOver07.d
LDGW tOver07.n
CONVND
DZCHECK 46
DDIV
STGD tOver07.x
LDGD tOver07.x
GLOBAL tOver07.Print
CALL 2
!   x := d/f; Print(x);
LDGD tOver07.d
LDGF tOver07.f
CONVFD
DZCHECK 47
DDIV
STGD tOver07.x
LDGD tOver07.x
GLOBAL tOver07.Print
CALL 2
!   x := d/d; Print(x);
LDGD tOver07.d
LDGD tOver07.d
DZCHECK 48
DDIV
STGD tOver07.x
LDGD tOver07.x
GLOBAL tOver07.Print
CALL 2
RETURN
END

! Global variables
GLOVAR tOver07.s 2
GLOVAR tOver07.n 4
GLOVAR tOver07.f 4
GLOVAR tOver07.d 8
GLOVAR tOver07.x 8

! End of file
]]*)
