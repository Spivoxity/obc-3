MODULE tFibTree3;

(*<<
.
Count = 1

.
Count = 1

(..)
Count = 2

(.(..))
Count = 3

((..)(.(..)))
Count = 5

((.(..))((..)(.(..))))
Count = 8

(((..)(.(..)))((.(..))((..)(.(..)))))
Count = 13

(((.(..))((..)(.(..))))(((..)(.(..)))((.(..))((..)(.(..))))))
Count = 21

((((..)(.(..)))((.(..))((..)(.(..)))))(((.(..))((..)(.(..))))(((..)(.(..)))((.(..))((..)(.(..)))))))
Count = 34

((((.(..))((..)(.(..))))(((..)(.(..)))((.(..))((..)(.(..))))))((((..)(.(..)))((.(..))((..)(.(..)))))(((.(..))((..)(.(..))))(((..)(.(..)))((.(..))((..)(.(..))))))))
Count = 55

(((((..)(.(..)))((.(..))((..)(.(..)))))(((.(..))((..)(.(..))))(((..)(.(..)))((.(..))((..)(.(..)))))))((((.(..))((..)(.(..))))(((..)(.(..)))((.(..))((..)(.(..))))))((((..)(.(..)))((.(..))((..)(.(..)))))(((.(..))((..)(.(..))))(((..)(.(..)))((.(..))((..)(.(..)))))))))
Count = 89

>>*)

IMPORT Out, GC, Random;

TYPE 
  tree = POINTER TO node;
  node = ARRAY OF tree;

PROCEDURE Alloc(a: node; VAR b: node): tree;
  VAR i: INTEGER; p: tree;
BEGIN
  (* Trash the array b *)
  FOR i := 0 TO LEN(b)-1 DO b[i] := NIL END;
  
  GC.Collect;

  NEW(p, LEN(a) + Random.Roll(10));
  FOR i := 0 TO LEN(a)-1 DO p[i] := a[i] END;
  RETURN p
END Alloc;  

PROCEDURE Cons(l, r: tree): tree;
  VAR a: ARRAY 2 OF tree;
BEGIN
  a[0] := l; a[1] := r;
  l := NIL; r := NIL;
  RETURN Alloc(a, a)
END Cons;

PROCEDURE Build(n: INTEGER): tree;
BEGIN
  IF n <= 1 THEN
    RETURN NIL
  ELSE
    RETURN Cons(Build(n-2), Build(n-1))
  END
END Build;

PROCEDURE Print(t:tree);
BEGIN
  IF t = NIL THEN
    Out.Char('.')
  ELSE
    Out.Char('(');
    Print(t[0]);
    Print(t[1]);
    Out.Char(')')
  END
END Print;

PROCEDURE count(t:tree): INTEGER;
BEGIN
  IF t = NIL THEN
    RETURN 1
  ELSE
    RETURN count(t[0]) + count(t[1])
  END
END count;

VAR i: INTEGER; p: tree;

BEGIN 
  GC.Debug("s");

  FOR i := 0 TO 10 DO
    p := Build(i);
    GC.Collect;
    Print(p); Out.Ln();
    Out.String("Count = "); Out.Int(count(p), 0); 
    Out.Ln(); Out.Ln();
  END
END tFibTree3.

(*[[
!! (SYMFILE #tFibTree3 STAMP #tFibTree3.%main 1)
!! (CHKSUM STAMP)
!! 
MODULE tFibTree3 STAMP 0
IMPORT Out STAMP
IMPORT GC STAMP
IMPORT Random STAMP
ENDHDR

PROC tFibTree3.Alloc 16 5 tFibTree3.Alloc.%map
! PROCEDURE Alloc(a: node; VAR b: node): tree;
LOCAL 12
LDLW 16
CONST 4
TIMES
FLEXCOPY
!   FOR i := 0 TO LEN(b)-1 DO b[i] := NIL END;
LDLW 24
DEC
STLW -12
CONST 0
STLW -4
LABEL L3
LDLW -4
LDLW -12
JGT L4
CONST 0
LDLW 20
LDLW -4
LDLW 24
BOUND 49
STIW
INCL -4
JUMP L3
LABEL L4
!   GC.Collect;
GLOBAL GC.Collect
CALL 0
!   NEW(p, LEN(a) + Random.Roll(10));
LDLW 16
CONST 10
GLOBAL Random.Roll
CALLW 1
PLUS
CONST 1
CONST 4
CONST 3
GLOBAL NEWFLEX
CALLW 4
STLW -8
!   FOR i := 0 TO LEN(a)-1 DO p[i] := a[i] END;
LDLW 16
DEC
STLW -16
CONST 0
STLW -4
LABEL L5
LDLW -4
LDLW -16
JGT L6
LDLW 12
LDLW -4
LDLW 16
BOUND 54
LDIW
LDLW -8
NCHECK 54
LDLW -4
DUP 1
LDNW -4
LDNW 4
BOUND 54
STIW
INCL -4
JUMP L5
LABEL L6
!   RETURN p
LDLW -8
RETURNW
END

PROC tFibTree3.Cons 8 5 0x00318001
! PROCEDURE Cons(l, r: tree): tree;
!   a[0] := l; a[1] := r;
LDLW 12
STLW -8
LDLW 16
STLW -4
!   l := NIL; r := NIL;
CONST 0
STLW 12
CONST 0
STLW 16
!   RETURN Alloc(a, a)
CONST 2
LOCAL -8
CONST 2
LOCAL -8
GLOBAL tFibTree3.Alloc
CALLW 4
RETURNW
END

PROC tFibTree3.Build 0 5 0
! PROCEDURE Build(n: INTEGER): tree;
!   IF n <= 1 THEN
LDLW 12
CONST 1
JGT L9
!     RETURN NIL
CONST 0
RETURNW
LABEL L9
!     RETURN Cons(Build(n-2), Build(n-1))
LDLW 12
DEC
GLOBAL tFibTree3.Build
CALLW 1
LDLW 12
CONST 2
MINUS
GLOBAL tFibTree3.Build
STKMAP 0x00000005
CALLW 1
GLOBAL tFibTree3.Cons
CALLW 2
RETURNW
END

PROC tFibTree3.Print 0 5 0x00100001
! PROCEDURE Print(t:tree);
!   IF t = NIL THEN
LDLW 12
JNEQZ L13
!     Out.Char('.')
CONST 46
ALIGNC
GLOBAL Out.Char
CALL 1
RETURN
LABEL L13
!     Out.Char('(');
CONST 40
ALIGNC
GLOBAL Out.Char
CALL 1
!     Print(t[0]);
LDLW 12
NCHECK 81
CONST 0
DUP 1
LDNW -4
LDNW 4
BOUND 81
LDIW
GLOBAL tFibTree3.Print
CALL 1
!     Print(t[1]);
LDLW 12
NCHECK 82
CONST 1
DUP 1
LDNW -4
LDNW 4
BOUND 82
LDIW
GLOBAL tFibTree3.Print
CALL 1
!     Out.Char(')')
CONST 41
ALIGNC
GLOBAL Out.Char
CALL 1
RETURN
END

PROC tFibTree3.count 0 5 0x00100001
! PROCEDURE count(t:tree): INTEGER;
!   IF t = NIL THEN
LDLW 12
JNEQZ L16
!     RETURN 1
CONST 1
RETURNW
LABEL L16
!     RETURN count(t[0]) + count(t[1])
LDLW 12
NCHECK 92
CONST 0
DUP 1
LDNW -4
LDNW 4
BOUND 92
LDIW
GLOBAL tFibTree3.count
CALLW 1
LDLW 12
NCHECK 92
CONST 1
DUP 1
LDNW -4
LDNW 4
BOUND 92
LDIW
GLOBAL tFibTree3.count
CALLW 1
PLUS
RETURNW
END

PROC tFibTree3.%main 0 5 0
!   GC.Debug("s");
CONST 2
GLOBAL tFibTree3.%2
GLOBAL GC.Debug
CALL 2
!   FOR i := 0 TO 10 DO
CONST 0
STGW tFibTree3.i
LABEL L17
LDGW tFibTree3.i
CONST 10
JGT L18
!     p := Build(i);
LDGW tFibTree3.i
GLOBAL tFibTree3.Build
CALLW 1
STGW tFibTree3.p
!     GC.Collect;
GLOBAL GC.Collect
CALL 0
!     Print(p); Out.Ln();
LDGW tFibTree3.p
GLOBAL tFibTree3.Print
CALL 1
GLOBAL Out.Ln
CALL 0
!     Out.String("Count = "); Out.Int(count(p), 0); 
CONST 9
GLOBAL tFibTree3.%1
GLOBAL Out.String
CALL 2
CONST 0
LDGW tFibTree3.p
GLOBAL tFibTree3.count
CALLW 1
GLOBAL Out.Int
CALL 2
!     Out.Ln(); Out.Ln();
GLOBAL Out.Ln
CALL 0
GLOBAL Out.Ln
CALL 0
!   FOR i := 0 TO 10 DO
LDGW tFibTree3.i
INC
STGW tFibTree3.i
JUMP L17
LABEL L18
RETURN
END

! Global variables
GLOVAR tFibTree3.i 4
GLOVAR tFibTree3.p 4

! Pointer map
DEFINE tFibTree3.%gcmap
WORD GC_BASE
WORD tFibTree3.p
WORD 0
WORD GC_END

! String "Count = "
DEFINE tFibTree3.%1
STRING 436F756E74203D2000

! String "s"
DEFINE tFibTree3.%2
STRING 7300

! Pointer maps
DEFINE tFibTree3.Alloc.%map
WORD GC_FLEX
WORD 12
WORD 1
WORD 4
WORD 0
WORD GC_END
WORD 20
WORD -8
WORD GC_END

! End of file
]]*)
