MODULE tGC3;

(*<<
     888     888
2097152
>>*)

IMPORT Out, GC;

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
BEGIN
  IF t = NIL THEN
    RETURN 0
  ELSE
    RETURN Size(t.left) + Size(t.right) + 1
  END
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
  Build(K); Print;
  Out.Int(GC.HeapSize(), 0); Out.Ln
END tGC3.

(*[[
!! SYMFILE #tGC3 STAMP #tGC3.%main 1
!! END STAMP
!! 
MODULE tGC3 STAMP 0
IMPORT Out STAMP
IMPORT GC STAMP
ENDHDR

PROC tGC3.Random 0 8 0
! PROCEDURE Random(N: INTEGER): INTEGER;
!   seed := (seed * a + b) MOD M;
LDGW tGC3.seed
CONST 3141592
TIMES
CONST 1618033
PLUS
CONST 2718281
MOD
STGW tGC3.seed
!   RETURN seed MOD N
LDGW tGC3.seed
LDLW 12
ZCHECK 19
MOD
RETURNW
END

PROC tGC3.Size 0 12 0x00100001
! PROCEDURE Size(t: tree): INTEGER;
!   IF t = NIL THEN
LDLW 12
JNEQZ 2
!     RETURN 0
CONST 0
RETURNW
LABEL 2
!     RETURN Size(t.left) + Size(t.right) + 1
LDLW 12
NCHECK 30
LOADW
CONST tGC3.Size
CALLW 1
LDLW 12
NCHECK 30
LDNW 4
CONST tGC3.Size
CALLW 1
PLUS
INC
RETURNW
END

PROC tGC3.Build 7 16 0x00000801
! PROCEDURE Build(count: INTEGER);
!   FOR i := 0 TO count-1 DO
LDLW 12
DEC
STLW -28
CONST 0
STLW -4
JUMP 4
LABEL 3
!     x := Random(N); y := Random(N); z := Random(N); w := Random(N);
CONST 1000
CONST tGC3.Random
CALLW 1
STLW -8
CONST 1000
CONST tGC3.Random
CALLW 1
STLW -12
CONST 1000
CONST tGC3.Random
CALLW 1
STLW -16
CONST 1000
CONST tGC3.Random
CALLW 1
STLW -20
!     NEW(t);
CONST 8
CONST tGC3.cell
LOCAL -24
CONST NEW
CALL 3
!     t.left := pool[x];
CONST tGC3.pool
LDLW -8
CONST 1000
BOUND 45
LDIW
LDLW -24
NCHECK 45
STOREW
!     t.right := pool[y];
CONST tGC3.pool
LDLW -12
CONST 1000
BOUND 46
LDIW
LDLW -24
NCHECK 46
STNW 4
!     pool[z] := t; aa[z] := aa[x] + aa[y] + 1;
LDLW -24
CONST tGC3.pool
LDLW -16
CONST 1000
BOUND 47
STIW
CONST tGC3.aa
LDLW -8
CONST 1000
BOUND 47
LDIW
CONST tGC3.aa
LDLW -12
CONST 1000
BOUND 47
LDIW
PLUS
INC
CONST tGC3.aa
LDLW -16
CONST 1000
BOUND 47
STIW
!     pool[w] := NIL; aa[w] := 0
CONST 0
CONST tGC3.pool
LDLW -20
CONST 1000
BOUND 48
STIW
CONST 0
CONST tGC3.aa
LDLW -20
CONST 1000
BOUND 48
STIW
!   FOR i := 0 TO count-1 DO
INCL -4
LABEL 4
LDLW -4
LDLW -28
JLEQ 3
RETURN
END

PROC tGC3.Print 3 16 0
! PROCEDURE Print;
!   s := 0; r := 0;
CONST 0
STLW -8
CONST 0
STLW -12
!   FOR i := 0 TO N-1 DO s := s + Size(pool[i]); r := r + aa[i] END;
CONST 0
STLW -4
JUMP 6
LABEL 5
LDLW -8
CONST tGC3.pool
LDLW -4
CONST 1000
BOUND 56
LDIW
CONST tGC3.Size
CALLW 1
PLUS
STLW -8
LDLW -12
CONST tGC3.aa
LDLW -4
CONST 1000
BOUND 56
LDIW
PLUS
STLW -12
INCL -4
LABEL 6
LDLW -4
CONST 999
JLEQ 5
!   Out.Int(s, 8); Out.Int(r, 8); Out.Ln
CONST 8
LDLW -8
CONST Out.Int
CALL 2
CONST 8
LDLW -12
CONST Out.Int
CALL 2
CONST Out.Ln
CALL 0
RETURN
END

PROC tGC3.%main 0 16 0
!   Build(K); Print;
CONST 500000
CONST tGC3.Build
CALL 1
CONST tGC3.Print
CALL 0
!   Out.Int(GC.HeapSize(), 0); Out.Ln
CONST 0
CONST GC.HeapSize
CALLW 0
CONST Out.Int
CALL 2
CONST Out.Ln
CALL 0
RETURN
END

! Global variables
GLOBAL tGC3.seed 4
GLOBAL tGC3.aa 4000
GLOBAL tGC3.pool 4000

! Pointer map
DEFINE tGC3.%gcmap
WORD GC_BASE
WORD tGC3.pool
WORD GC_BLOCK
WORD 0
WORD 1000
WORD GC_END

! Descriptor for cell
DEFINE tGC3.cell
WORD 0x00000007
WORD 0
WORD tGC3.cell.%anc

DEFINE tGC3.cell.%anc
WORD tGC3.cell

! End of file
]]*)
