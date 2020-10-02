MODULE tGC307;

(*<<
     888     888
>>*)

IMPORT Out;

CONST N = 1000;

CONST a = 3141592; b = 1618033; M = 2718281;

VAR seed: INTEGER;

PROCEDURE Random(N: INTEGER): INTEGER;
BEGIN
  seed := (seed * a + b) MOD M;
  RETURN seed MOD N
END Random;

TYPE tree = POINTER TO cell;
  cell = RECORD left, right: tree END;

PROCEDURE Size(t: tree): INTEGER;
VAR s: INTEGER;
BEGIN
  IF t = NIL THEN
s := 0
  ELSE
s:= Size(t.left) + Size(t.right) + 1
  END
RETURN s
END Size;

VAR 
  aa: ARRAY N OF INTEGER;
  pool: ARRAY N OF tree;

PROCEDURE Build(count: INTEGER);
  VAR i, x, y, z, w: INTEGER; t: tree;
BEGIN
  FOR i := 0 TO count-1 DO
    x := Random(N); y := Random(N); z := Random(N); w := Random(N);

    NEW(t);
    t.left := pool[x];
    t.right := pool[y];
    pool[z] := t; aa[z] := aa[x] + aa[y] + 1;
    pool[w] := NIL; aa[w] := 0
  END
END Build;

PROCEDURE Print;
  VAR i, s, r: INTEGER;
BEGIN
  s := 0; r := 0;
  FOR i := 0 TO N-1 DO s := s + Size(pool[i]); r := r + aa[i] END;
  Out.Int(s, 8); Out.Int(r, 8); Out.Ln
END Print;

CONST K = 500000;

BEGIN
  Build(K); Print
END tGC307.

(*[[
!! (SYMFILE #tGC307 STAMP #tGC307.%main 1 #tGC307.m)
!! (CHKSUM STAMP)
!! 
MODULE tGC307 STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tGC307.Random 0 2 0
! PROCEDURE Random(N: INTEGER): INTEGER;
!   seed := (seed * a + b) MOD M;
LDGW tGC307.seed
CONST 3141592
TIMES
CONST 1618033
PLUS
CONST 2718281
MOD
STGW tGC307.seed
!   RETURN seed MOD N
LDGW tGC307.seed
LDLW 12
ZCHECK 18
MOD
RETURN
END

PROC tGC307.Size 4 3 0x00100001
! PROCEDURE Size(t: tree): INTEGER;
!   IF t = NIL THEN
LDLW 12
JNEQZ L3
! s := 0
CONST 0
STLW -4
JUMP L1
LABEL L3
! s:= Size(t.left) + Size(t.right) + 1
LDLW 12
NCHECK 30
LOADW
GLOBAL tGC307.Size
CALLW 1
LDLW 12
NCHECK 30
LDNW 4
GLOBAL tGC307.Size
CALLW 1
PLUS
INC
STLW -4
LABEL L1
! RETURN s
LDLW -4
RETURN
END

PROC tGC307.Build 28 4 0x00000801
! PROCEDURE Build(count: INTEGER);
!   FOR i := 0 TO count-1 DO
LDLW 12
DEC
STLW -28
CONST 0
STLW -4
LABEL L4
LDLW -4
LDLW -28
JGT L5
!     x := Random(N); y := Random(N); z := Random(N); w := Random(N);
CONST 1000
GLOBAL tGC307.Random
CALLW 1
STLW -8
CONST 1000
GLOBAL tGC307.Random
CALLW 1
STLW -12
CONST 1000
GLOBAL tGC307.Random
CALLW 1
STLW -16
CONST 1000
GLOBAL tGC307.Random
CALLW 1
STLW -20
!     NEW(t);
CONST 8
GLOBAL tGC307.cell
GLOBAL NEW
CALLW 2
STLW -24
!     t.left := pool[x];
GLOBAL tGC307.pool
LDLW -8
CONST 1000
BOUND 46
LDIW
LDLW -24
NCHECK 46
STOREW
!     t.right := pool[y];
GLOBAL tGC307.pool
LDLW -12
CONST 1000
BOUND 47
LDIW
LDLW -24
NCHECK 47
STNW 4
!     pool[z] := t; aa[z] := aa[x] + aa[y] + 1;
LDLW -24
GLOBAL tGC307.pool
LDLW -16
CONST 1000
BOUND 48
STIW
GLOBAL tGC307.aa
LDLW -8
CONST 1000
BOUND 48
LDIW
GLOBAL tGC307.aa
LDLW -12
CONST 1000
BOUND 48
LDIW
PLUS
INC
GLOBAL tGC307.aa
LDLW -16
CONST 1000
BOUND 48
STIW
!     pool[w] := NIL; aa[w] := 0
CONST 0
GLOBAL tGC307.pool
LDLW -20
CONST 1000
BOUND 49
STIW
CONST 0
GLOBAL tGC307.aa
LDLW -20
CONST 1000
BOUND 49
STIW
!   FOR i := 0 TO count-1 DO
INCL -4
JUMP L4
LABEL L5
RETURN
END

PROC tGC307.Print 12 4 0
! PROCEDURE Print;
!   s := 0; r := 0;
CONST 0
STLW -8
CONST 0
STLW -12
!   FOR i := 0 TO N-1 DO s := s + Size(pool[i]); r := r + aa[i] END;
CONST 0
STLW -4
LABEL L6
LDLW -4
CONST 999
JGT L7
LDLW -8
GLOBAL tGC307.pool
LDLW -4
CONST 1000
BOUND 57
LDIW
GLOBAL tGC307.Size
CALLW 1
PLUS
STLW -8
LDLW -12
GLOBAL tGC307.aa
LDLW -4
CONST 1000
BOUND 57
LDIW
PLUS
STLW -12
INCL -4
JUMP L6
LABEL L7
!   Out.Int(s, 8); Out.Int(r, 8); Out.Ln
CONST 8
LDLW -8
GLOBAL Out.Int
CALL 2
CONST 8
LDLW -12
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

PROC tGC307.%main 0 2 0
!   Build(K); Print
CONST 500000
GLOBAL tGC307.Build
CALL 1
GLOBAL tGC307.Print
CALL 0
RETURN
END

! Global variables
GLOVAR tGC307.seed 4
GLOVAR tGC307.aa 4000
GLOVAR tGC307.pool 4000

! Global pointer map
DEFINE tGC307.%gcmap
WORD GC_BASE
WORD tGC307.pool
WORD GC_BLOCK
WORD 0
WORD 1000
WORD GC_END

! Descriptor for cell
DEFINE tGC307.cell
WORD 0x00000007
WORD 0
WORD tGC307.cell.%anc

DEFINE tGC307.cell.%anc
WORD tGC307.cell

! End of file
]]*)
