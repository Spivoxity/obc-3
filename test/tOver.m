MODULE tOver;

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
  s := 1; n := 2; f := 3; d := 4;
  x := s/s; Print(x);
  x := s/n; Print(x);
  x := s/f; Print(x);
  x := s/d; Print(x);
  x := n/s; Print(x);
  x := n/n; Print(x);
  x := n/f; Print(x);
  x := n/d; Print(x);
  x := f/s; Print(x);
  x := f/n; Print(x);
  x := f/f; Print(x);
  x := f/d; Print(x);
  x := d/s; Print(x);
  x := d/n; Print(x);
  x := d/f; Print(x);
  x := d/d; Print(x);
END tOver.

(*[[
!! (SYMFILE #tOver STAMP #tOver.%main 1 #tOver.m)
!! (CHKSUM STAMP)
!! 
MODULE tOver STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tOver.Print 0 5 0
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

PROC tOver.%main 0 4 0
!   s := 1; n := 2; f := 3; d := 4;
CONST 1
STGS tOver.s
CONST 2
STGW tOver.n
FCONST 3.0
STGF tOver.f
DCONST 4.0
STGD tOver.d
!   x := s/s; Print(x);
LDGS tOver.s
CONVNF
LDGS tOver.s
CONVNF
FZCHECK 33
FDIV
CONVFD
STGD tOver.x
LDGD tOver.x
GLOBAL tOver.Print
CALL 2
!   x := s/n; Print(x);
LDGS tOver.s
CONVNF
LDGW tOver.n
CONVNF
FZCHECK 34
FDIV
CONVFD
STGD tOver.x
LDGD tOver.x
GLOBAL tOver.Print
CALL 2
!   x := s/f; Print(x);
LDGS tOver.s
CONVNF
LDGF tOver.f
FZCHECK 35
FDIV
CONVFD
STGD tOver.x
LDGD tOver.x
GLOBAL tOver.Print
CALL 2
!   x := s/d; Print(x);
LDGS tOver.s
CONVND
LDGD tOver.d
DZCHECK 36
DDIV
STGD tOver.x
LDGD tOver.x
GLOBAL tOver.Print
CALL 2
!   x := n/s; Print(x);
LDGW tOver.n
CONVNF
LDGS tOver.s
CONVNF
FZCHECK 37
FDIV
CONVFD
STGD tOver.x
LDGD tOver.x
GLOBAL tOver.Print
CALL 2
!   x := n/n; Print(x);
LDGW tOver.n
CONVNF
LDGW tOver.n
CONVNF
FZCHECK 38
FDIV
CONVFD
STGD tOver.x
LDGD tOver.x
GLOBAL tOver.Print
CALL 2
!   x := n/f; Print(x);
LDGW tOver.n
CONVNF
LDGF tOver.f
FZCHECK 39
FDIV
CONVFD
STGD tOver.x
LDGD tOver.x
GLOBAL tOver.Print
CALL 2
!   x := n/d; Print(x);
LDGW tOver.n
CONVND
LDGD tOver.d
DZCHECK 40
DDIV
STGD tOver.x
LDGD tOver.x
GLOBAL tOver.Print
CALL 2
!   x := f/s; Print(x);
LDGF tOver.f
LDGS tOver.s
CONVNF
FZCHECK 41
FDIV
CONVFD
STGD tOver.x
LDGD tOver.x
GLOBAL tOver.Print
CALL 2
!   x := f/n; Print(x);
LDGF tOver.f
LDGW tOver.n
CONVNF
FZCHECK 42
FDIV
CONVFD
STGD tOver.x
LDGD tOver.x
GLOBAL tOver.Print
CALL 2
!   x := f/f; Print(x);
LDGF tOver.f
LDGF tOver.f
FZCHECK 43
FDIV
CONVFD
STGD tOver.x
LDGD tOver.x
GLOBAL tOver.Print
CALL 2
!   x := f/d; Print(x);
LDGF tOver.f
CONVFD
LDGD tOver.d
DZCHECK 44
DDIV
STGD tOver.x
LDGD tOver.x
GLOBAL tOver.Print
CALL 2
!   x := d/s; Print(x);
LDGD tOver.d
LDGS tOver.s
CONVND
DZCHECK 45
DDIV
STGD tOver.x
LDGD tOver.x
GLOBAL tOver.Print
CALL 2
!   x := d/n; Print(x);
LDGD tOver.d
LDGW tOver.n
CONVND
DZCHECK 46
DDIV
STGD tOver.x
LDGD tOver.x
GLOBAL tOver.Print
CALL 2
!   x := d/f; Print(x);
LDGD tOver.d
LDGF tOver.f
CONVFD
DZCHECK 47
DDIV
STGD tOver.x
LDGD tOver.x
GLOBAL tOver.Print
CALL 2
!   x := d/d; Print(x);
LDGD tOver.d
LDGD tOver.d
DZCHECK 48
DDIV
STGD tOver.x
LDGD tOver.x
GLOBAL tOver.Print
CALL 2
RETURN
END

! Global variables
GLOVAR tOver.s 2
GLOVAR tOver.n 4
GLOVAR tOver.f 4
GLOVAR tOver.d 8
GLOVAR tOver.x 8

! End of file
]]*)
