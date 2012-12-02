MODULE tGC4;

(*<<
42
42
53
>>*)

IMPORT Out, GC;

TYPE foo = POINTER TO RECORD a, b: INTEGER END;
TYPE bar = POINTER TO ARRAY OF INTEGER;
TYPE qux = ARRAY 1 OF INTEGER;

VAR p: foo; q: bar; r: POINTER TO qux;

(* VAR parameters should be included in the GC map of a procedure *)

PROCEDURE baz(VAR x: INTEGER);
BEGIN
  p := NIL; q := NIL;
  x := 42;
  GC.Collect;
  Out.Int(x, 0); Out.Ln
END baz;

(* Read-only parameters also *)

PROCEDURE bop(CONST x: qux);
BEGIN
  r := NIL;
  GC.Collect;
  Out.Int(x[0], 0); Out.Ln
END bop;

BEGIN
  NEW(p);
  baz(p.b);
  NEW(q, 20000);
  baz(q[19500]);

  NEW(r); r[0] := 53;
  bop(r^);
END tGC4.

(*[[
!! SYMFILE #tGC4 STAMP #tGC4.%main 1
!! END STAMP
!! 
MODULE tGC4 STAMP 0
IMPORT Out STAMP
IMPORT GC STAMP
ENDHDR

PROC tGC4.baz 0 3 0x00100001
! PROCEDURE baz(VAR x: INTEGER);
!   p := NIL; q := NIL;
CONST 0
STGW tGC4.p
CONST 0
STGW tGC4.q
!   x := 42;
CONST 42
LDLW 12
STOREW
!   GC.Collect;
GLOBAL GC.Collect
CALL 0
!   Out.Int(x, 0); Out.Ln
CONST 0
LDLW 12
LOADW
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

PROC tGC4.bop 0 4 0x00100001
! PROCEDURE bop(CONST x: qux);
!   r := NIL;
CONST 0
STGW tGC4.r
!   GC.Collect;
GLOBAL GC.Collect
CALL 0
!   Out.Int(x[0], 0); Out.Ln
CONST 0
LDLW 12
LOADW
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

PROC tGC4.%main 0 6 0
!   NEW(p);
CONST 8
GLOBAL tGC4.%1
GLOBAL tGC4.p
GLOBAL NEW
CALL 3
!   baz(p.b);
LDGW tGC4.p
NCHECK 38
CONST 4
PLUSA
GLOBAL tGC4.baz
CALL 1
!   NEW(q, 20000);
CONST 20000
CONST 1
CONST 4
CONST 0
GLOBAL tGC4.q
GLOBAL NEWFLEX
CALL 5
!   baz(q[19500]);
LDGW tGC4.q
NCHECK 40
CONST 19500
DUP 1
LDNW -4
LDNW 4
BOUND 40
INDEXW
GLOBAL tGC4.baz
CALL 1
!   NEW(r); r[0] := 53;
CONST 4
CONST 0
GLOBAL tGC4.r
GLOBAL NEW
CALL 3
CONST 53
LDGW tGC4.r
NCHECK 42
STOREW
!   bop(r^);
LDGW tGC4.r
NCHECK 43
GLOBAL tGC4.bop
CALL 1
RETURN
END

! Global variables
GLOVAR tGC4.p 4
GLOVAR tGC4.q 4
GLOVAR tGC4.r 4

! Pointer map
DEFINE tGC4.%gcmap
WORD GC_BASE
WORD tGC4.p
WORD 0
WORD GC_BASE
WORD tGC4.q
WORD 0
WORD GC_BASE
WORD tGC4.r
WORD 0
WORD GC_END

! Descriptor for *anon*
DEFINE tGC4.%1
WORD 0
WORD 0
WORD tGC4.%1.%anc

DEFINE tGC4.%1.%anc
WORD tGC4.%1

! End of file
]]*)
