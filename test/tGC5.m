MODULE tGC5;

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
  cell = RECORD key, value: INTEGER; left, right: tree END;

PROCEDURE Size(t: tree): INTEGER;
BEGIN
  IF t = NIL THEN
    RETURN 0
  ELSE
    RETURN t.value + Size(t.left) + Size(t.right)
  END
END Size;

PROCEDURE Cons(key, value: INTEGER; left, right: tree): tree;
  VAR t: tree;
BEGIN
  NEW(t);
  t.key := key; t.value := value;
  t.left := left; t.right := right;
  RETURN t
END Cons;

PROCEDURE Increment(key: INTEGER; t: tree): tree;
BEGIN
  IF t = NIL THEN
    RETURN Cons(key, 1, NIL, NIL)
  ELSIF key = t.key THEN
    RETURN Cons(key, t.value+1, t.left, t.right)
  ELSIF key < t.key THEN
    RETURN Cons(t.key, t.value, Increment(key, t.left), t.right)
  ELSE
    RETURN Cons(t.key, t.value, t.left, Increment(key, t.right))
  END
END Increment;
    
PROCEDURE Ordered(t: tree; lo, hi: INTEGER): BOOLEAN;
BEGIN
  IF t = NIL THEN
    RETURN TRUE
  ELSE
    RETURN (lo <= t.key) & (t.key < hi) 
	& Ordered(t.left, lo, t.key)
	& Ordered(t.right, t.key+1, hi)
  END
END Ordered;

VAR 
  i: INTEGER;
  t: tree;

CONST K = 50000;

BEGIN
  t := NIL;
  FOR i := 1 TO K DO
    t := Increment(Random(100), t)
  END;
  Out.Int(Size(t), 0); Out.Ln;
  IF Ordered(t, 0, 100) THEN Out.String("ordered"); Out.Ln END;
END tGC5.

(*<<
50000
ordered
>>*)

(*[[
!! (SYMFILE #tGC5 STAMP #tGC5.%main 1 #tGC5.m)
!! (CHKSUM STAMP)
!! 
MODULE tGC5 STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tGC5.Random 0 2 0
! PROCEDURE Random(N: INTEGER): INTEGER;
!   seed := (seed * a + b) MOD M;
LDGW tGC5.seed
CONST 3141592
TIMES
CONST 1618033
PLUS
CONST 2718281
MOD
STGW tGC5.seed
!   RETURN seed MOD N
LDGW tGC5.seed
LDLW 12
ZCHECK 14
MOD
RETURN
END

PROC tGC5.Size 0 3 0x00100001
! PROCEDURE Size(t: tree): INTEGER;
!   IF t = NIL THEN
LDLW 12
JNEQZ L4
!     RETURN 0
CONST 0
RETURN
LABEL L4
!     RETURN t.value + Size(t.left) + Size(t.right)
LDLW 12
NCHECK 25
LDNW 4
LDLW 12
NCHECK 25
LDNW 8
GLOBAL tGC5.Size
CALLW 1
PLUS
LDLW 12
NCHECK 25
LDNW 12
GLOBAL tGC5.Size
CALLW 1
PLUS
RETURN
END

PROC tGC5.Cons 4 3 0x00c10001
! PROCEDURE Cons(key, value: INTEGER; left, right: tree): tree;
!   NEW(t);
CONST 16
GLOBAL tGC5.cell
GLOBAL NEW
CALLW 2
STLW -4
!   t.key := key; t.value := value;
LDLW 12
LDLW -4
NCHECK 33
STOREW
LDLW 16
LDLW -4
NCHECK 33
STNW 4
!   t.left := left; t.right := right;
LDLW 20
LDLW -4
NCHECK 34
STNW 8
LDLW 24
LDLW -4
NCHECK 34
STNW 12
!   RETURN t
LDLW -4
RETURN
END

PROC tGC5.Increment 0 5 0x00200001
! PROCEDURE Increment(key: INTEGER; t: tree): tree;
!   IF t = NIL THEN
LDLW 16
JNEQZ L7
!     RETURN Cons(key, 1, NIL, NIL)
CONST 0
CONST 0
CONST 1
LDLW 12
GLOBAL tGC5.Cons
CALLW 4
RETURN
LABEL L7
!   ELSIF key = t.key THEN
LDLW 12
LDLW 16
NCHECK 42
LOADW
JNEQ L9
!     RETURN Cons(key, t.value+1, t.left, t.right)
LDLW 16
NCHECK 43
LDNW 12
LDLW 16
NCHECK 43
LDNW 8
LDLW 16
NCHECK 43
LDNW 4
INC
LDLW 12
GLOBAL tGC5.Cons
CALLW 4
RETURN
LABEL L9
!   ELSIF key < t.key THEN
LDLW 12
LDLW 16
NCHECK 44
LOADW
JGEQ L11
!     RETURN Cons(t.key, t.value, Increment(key, t.left), t.right)
LDLW 16
NCHECK 45
LDNW 12
LDLW 16
NCHECK 45
LDNW 8
LDLW 12
GLOBAL tGC5.Increment
STKMAP 0x00000009
CALLW 2
LDLW 16
NCHECK 45
LDNW 4
LDLW 16
NCHECK 45
LOADW
GLOBAL tGC5.Cons
CALLW 4
RETURN
LABEL L11
!     RETURN Cons(t.key, t.value, t.left, Increment(key, t.right))
LDLW 16
NCHECK 47
LDNW 12
LDLW 12
GLOBAL tGC5.Increment
CALLW 2
LDLW 16
NCHECK 47
LDNW 8
LDLW 16
NCHECK 47
LDNW 4
LDLW 16
NCHECK 47
LOADW
GLOBAL tGC5.Cons
CALLW 4
RETURN
END

PROC tGC5.Ordered 0 4 0x00100001
! PROCEDURE Ordered(t: tree; lo, hi: INTEGER): BOOLEAN;
!   IF t = NIL THEN
LDLW 12
JNEQZ L20
!     RETURN TRUE
CONST 1
RETURN
LABEL L20
!     RETURN (lo <= t.key) & (t.key < hi) 
LDLW 16
LDLW 12
NCHECK 56
LOADW
JGT L14
LDLW 12
NCHECK 56
LOADW
LDLW 20
JGEQ L14
LDLW 12
NCHECK 57
LOADW
LDLW 16
LDLW 12
NCHECK 57
LDNW 8
GLOBAL tGC5.Ordered
CALLW 3
JNEQZ L15
LABEL L14
CONST 0
RETURN
LABEL L15
LDLW 20
LDLW 12
NCHECK 58
LOADW
INC
LDLW 12
NCHECK 58
LDNW 12
GLOBAL tGC5.Ordered
CALLW 3
RETURN
END

PROC tGC5.%main 0 4 0
!   t := NIL;
CONST 0
STGW tGC5.t
!   FOR i := 1 TO K DO
CONST 1
STGW tGC5.i
LABEL L21
LDGW tGC5.i
CONST 50000
JGT L22
!     t := Increment(Random(100), t)
LDGW tGC5.t
CONST 100
GLOBAL tGC5.Random
STKMAP 0x00000005
CALLW 1
GLOBAL tGC5.Increment
CALLW 2
STGW tGC5.t
!   FOR i := 1 TO K DO
LDGW tGC5.i
INC
STGW tGC5.i
JUMP L21
LABEL L22
!   Out.Int(Size(t), 0); Out.Ln;
CONST 0
LDGW tGC5.t
GLOBAL tGC5.Size
CALLW 1
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
!   IF Ordered(t, 0, 100) THEN Out.String("ordered"); Out.Ln END;
CONST 100
CONST 0
LDGW tGC5.t
GLOBAL tGC5.Ordered
CALLW 3
JEQZ L25
CONST 8
GLOBAL tGC5.%1
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
LABEL L25
RETURN
END

! Global variables
GLOVAR tGC5.seed 4
GLOVAR tGC5.i 4
GLOVAR tGC5.t 4

! Global pointer map
DEFINE tGC5.%gcmap
WORD GC_POINTER
WORD tGC5.t
WORD GC_END

! String "ordered"
DEFINE tGC5.%1
STRING 6F72646572656400

! Descriptor for cell
DEFINE tGC5.cell
WORD 0x00000019
WORD 0
WORD tGC5.cell.%anc

DEFINE tGC5.cell.%anc
WORD tGC5.cell

! End of file
]]*)
