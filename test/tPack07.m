MODULE tPack07;

IMPORT Out;

VAR x: REAL; y: LONGREAL; n: INTEGER;

BEGIN
  x := 256.0 * 1.61803;
  Out.Real(x); Out.Ln;
  UNPK(x, n);
  Out.Real(x); Out.Char(' '); Out.Int(n, 0); Out.Ln;
  PACK(x, n);
  Out.Real(x); Out.Ln;

  y := 256.0 * 1.61803398874;
  Out.LongReal(y); Out.Ln;
  UNPK(y, n);
  Out.LongReal(y); Out.Char(' '); Out.Int(n, 0); Out.Ln;
  PACK(y, n);
  Out.LongReal(y); Out.Ln
END tPack07.

(*<<
414.216
1.61803 8
414.216
414.216701117
1.61803398874 8
414.216701117
>>*)

(*[[
!! (SYMFILE #tPack07 STAMP #tPack07.%main 1 #tPack07.m)
!! (CHKSUM STAMP)
!! 
MODULE tPack07 STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tPack07.%main 0 3 0
!   x := 256.0 * 1.61803;
FCONST 414.21568
STGF tPack07.x
!   Out.Real(x); Out.Ln;
LDGF tPack07.x
GLOBAL Out.Real
CALL 1
GLOBAL Out.Ln
CALL 0
!   UNPK(x, n);
GLOBAL tPack07.n
GLOBAL tPack07.x
GLOBAL UNPK
CALL 2
!   Out.Real(x); Out.Char(' '); Out.Int(n, 0); Out.Ln;
LDGF tPack07.x
GLOBAL Out.Real
CALL 1
CONST 32
ALIGNC
GLOBAL Out.Char
CALL 1
CONST 0
LDGW tPack07.n
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
!   PACK(x, n);
LDGW tPack07.n
GLOBAL tPack07.x
GLOBAL PACK
CALL 2
!   Out.Real(x); Out.Ln;
LDGF tPack07.x
GLOBAL Out.Real
CALL 1
GLOBAL Out.Ln
CALL 0
!   y := 256.0 * 1.61803398874;
DCONST 414.216701117
STGD tPack07.y
!   Out.LongReal(y); Out.Ln;
LDGD tPack07.y
GLOBAL Out.LongReal
CALL 2
GLOBAL Out.Ln
CALL 0
!   UNPK(y, n);
GLOBAL tPack07.n
GLOBAL tPack07.y
GLOBAL UNPKLONG
CALL 2
!   Out.LongReal(y); Out.Char(' '); Out.Int(n, 0); Out.Ln;
LDGD tPack07.y
GLOBAL Out.LongReal
CALL 2
CONST 32
ALIGNC
GLOBAL Out.Char
CALL 1
CONST 0
LDGW tPack07.n
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
!   PACK(y, n);
LDGW tPack07.n
GLOBAL tPack07.y
GLOBAL PACKLONG
CALL 2
!   Out.LongReal(y); Out.Ln
LDGD tPack07.y
GLOBAL Out.LongReal
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tPack07.x 4
GLOVAR tPack07.y 8
GLOVAR tPack07.n 4

! End of file
]]*)
