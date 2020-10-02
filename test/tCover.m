MODULE tCover;

(* Miscellany to improve code coverage *)

IMPORT Out, SYSTEM;

VAR l: LONGINT; x: REAL;

TYPE u = RECORD END; t = RECORD (u) y: INTEGER END;

VAR qq, rr: t; pp: POINTER TO t; aa: ARRAY 1 OF t;
  b: BOOLEAN;

PROCEDURE proc1(VAR r: t);
BEGIN
  Out.Int(r.y, 0); Out.Ln
END proc1;

PROCEDURE proc2(VAR r: u);
BEGIN
  proc1(r(t))
END proc2;

PROCEDURE print(CONST x: LONGINT);
BEGIN
  Out.LongInt(x, 0); Out.Ln
END print;

PROCEDURE proc9*;
  TYPE bigrec = RECORD a: ARRAY 100 OF INTEGER; p: POINTER TO t; END;
  VAR p: POINTER TO ARRAY OF bigrec;
    q: POINTER TO ARRAY 1 OF bigrec;
BEGIN
  NEW(p, 20); NEW(q)
END proc9;

BEGIN
  l := 1122334455667788;
  x := l; x := ABS(x); x := SHORT(ABS(LONG(x)));
  Out.Real(x); Out.Ln;
  DEC(l); print(l);
  l := ASR(SHORT(l), 1); Out.LongInt(l, 0); Out.Ln;
  l := ASH(SHORT(l), 1); Out.LongInt(l, 0); Out.Ln;
  l := SYSTEM.VAL(INTEGER, Out.Real);

  rr.y := 2314; NEW(pp); pp^ := rr;
  aa[0] := pp^; qq := aa[0];
  proc2(qq);

  IF pp^ IS t THEN Out.String("OK1"); Out.Ln END;

  l := -1;
  b := ~((l < 0) & (pp^.y < 4000));
  IF ~b THEN Out.String("OK2"); Out.Ln END;

  b := (l > 0) OR (pp^.y > 4000);
  IF ~b THEN Out.String("OK3"); Out.Ln END  
END tCover.

(*<<
1.12233E+15
1122334455667787
-314159579
-628319158
2314
OK1
OK2
OK3
>>*)

(*[[
!! (SYMFILE #tCover STAMP #tCover.%main 1 #tCover.m)
!! (PROCEDURE #proc9* 29 #tCover.proc9 !1 (PROC 0 VOID))
!! (CHKSUM STAMP)
!! 
MODULE tCover STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tCover.proc1 0 3 0x00100001
! PROCEDURE proc1(VAR r: t);
!   Out.Int(r.y, 0); Out.Ln
CONST 0
LDLW 12
LOADW
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

PROC tCover.proc2 0 4 0x00100001
! PROCEDURE proc2(VAR r: u);
!   proc1(r(t))
GLOBAL tCover.t
LDLW 16
DUP 0
LDNW 4
CONST 1
JGEQ L8
POP 1
JUMP L7
LABEL L8
LDNW 8
LDNW 4
GLOBAL tCover.t
JEQ L6
LABEL L7
ERROR E_CAST 21
LABEL L6
LDLW 12
GLOBAL tCover.proc1
CALL 2
RETURN
END

PROC tCover.print 0 4 0
! PROCEDURE print(CONST x: LONGINT);
!   Out.LongInt(x, 0); Out.Ln
CONST 0
LDLQ 12
GLOBAL Out.LongInt
CALL 3
GLOBAL Out.Ln
CALL 0
RETURN
END

PROC tCover.proc9 8 5 0x00018001
! PROCEDURE proc9*;
!   NEW(p, 20); NEW(q)
CONST 20
CONST 1
CONST 404
GLOBAL tCover.%4.%map
GLOBAL NEWFLEX
CALLW 4
STLW -4
CONST 404
GLOBAL tCover.%5
GLOBAL NEW
CALLW 2
STLW -8
RETURN
END

PROC tCover.%main 0 4 0
!   l := 1122334455667788;
QCONST 1122334455667788
STGQ tCover.l
!   x := l; x := ABS(x); x := SHORT(ABS(LONG(x)));
LDGQ tCover.l
CONVQD
CONVDF
STGF tCover.x
LDGF tCover.x
GLOBAL ABSFLOAT
CALLF 1
STGF tCover.x
LDGF tCover.x
CONVFD
GLOBAL ABSDOUBLE
CALLD 2
CONVDF
STGF tCover.x
!   Out.Real(x); Out.Ln;
LDGF tCover.x
GLOBAL Out.Real
CALL 1
GLOBAL Out.Ln
CALL 0
!   DEC(l); print(l);
CONST 1
CONVNQ
GLOBAL tCover.l
GLOBAL DECLONG
CALL 3
LDGQ tCover.l
GLOBAL tCover.print
CALL 2
!   l := ASR(SHORT(l), 1); Out.LongInt(l, 0); Out.Ln;
LDGQ tCover.l
CONVQN
CONST 1
ASR
CONVNQ
STGQ tCover.l
CONST 0
LDGQ tCover.l
GLOBAL Out.LongInt
CALL 3
GLOBAL Out.Ln
CALL 0
!   l := ASH(SHORT(l), 1); Out.LongInt(l, 0); Out.Ln;
LDGQ tCover.l
CONVQN
CONST 1
LSL
CONVNQ
STGQ tCover.l
CONST 0
LDGQ tCover.l
GLOBAL Out.LongInt
CALL 3
GLOBAL Out.Ln
CALL 0
!   l := SYSTEM.VAL(INTEGER, Out.Real);
GLOBAL Out.Real
CONVNQ
STGQ tCover.l
!   rr.y := 2314; NEW(pp); pp^ := rr;
CONST 2314
STGW tCover.rr
CONST 4
GLOBAL tCover.t
GLOBAL NEW
CALLW 2
STGW tCover.pp
LDGW tCover.pp
NCHECK 46
DUP 0
LDNW -4
GLOBAL tCover.t
JEQ L9
ERROR E_ASSIGN 46
LABEL L9
GLOBAL tCover.rr
CONST 4
FIXCOPY
!   aa[0] := pp^; qq := aa[0];
GLOBAL tCover.aa
LDGW tCover.pp
NCHECK 47
CONST 4
FIXCOPY
GLOBAL tCover.qq
GLOBAL tCover.aa
CONST 4
FIXCOPY
!   proc2(qq);
GLOBAL tCover.t
GLOBAL tCover.qq
GLOBAL tCover.proc2
CALL 2
!   IF pp^ IS t THEN Out.String("OK1"); Out.Ln END;
LDGW tCover.pp
NCHECK 50
LDNW -4
DUP 0
LDNW 4
CONST 1
JGEQ L13
POP 1
JUMP L12
LABEL L13
LDNW 8
LDNW 4
GLOBAL tCover.t
JNEQ L12
CONST 4
GLOBAL tCover.%1
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
LABEL L12
!   l := -1;
CONST -1
CONVNQ
STGQ tCover.l
!   b := ~((l < 0) & (pp^.y < 4000));
LDGQ tCover.l
CONST 0
CONVNQ
QJLT L15
CONST 1
JUMP L16
LABEL L15
LDGW tCover.pp
NCHECK 53
LOADW
CONST 4000
GEQ
LABEL L16
STGC tCover.b
!   IF ~b THEN Out.String("OK2"); Out.Ln END;
LDGC tCover.b
JNEQZ L19
CONST 4
GLOBAL tCover.%2
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
LABEL L19
!   b := (l > 0) OR (pp^.y > 4000);
LDGQ tCover.l
CONST 0
CONVNQ
QJLEQ L21
CONST 1
JUMP L22
LABEL L21
LDGW tCover.pp
NCHECK 56
LOADW
CONST 4000
GT
LABEL L22
STGC tCover.b
!   IF ~b THEN Out.String("OK3"); Out.Ln END  
LDGC tCover.b
JNEQZ L25
CONST 4
GLOBAL tCover.%3
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
LABEL L25
RETURN
END

! Global variables
GLOVAR tCover.l 8
GLOVAR tCover.x 4
GLOVAR tCover.qq 4
GLOVAR tCover.rr 4
GLOVAR tCover.pp 4
GLOVAR tCover.aa 4
GLOVAR tCover.b 1

! Global pointer map
DEFINE tCover.%gcmap
WORD GC_POINTER
WORD tCover.pp
WORD GC_END

! String "OK1"
DEFINE tCover.%1
STRING 4F4B3100

! String "OK2"
DEFINE tCover.%2
STRING 4F4B3200

! String "OK3"
DEFINE tCover.%3
STRING 4F4B3300

! Descriptor for u
DEFINE tCover.u
WORD 0
WORD 0
WORD tCover.u.%anc

DEFINE tCover.u.%anc
WORD tCover.u

! Descriptor for t
DEFINE tCover.t
WORD 0
WORD 1
WORD tCover.t.%anc

DEFINE tCover.t.%anc
WORD tCover.u
WORD tCover.t

! Descriptor for bigrec
DEFINE tCover.%4
WORD tCover.%4.%map
WORD 0
WORD tCover.%4.%anc

DEFINE tCover.%4.%anc
WORD tCover.%4

! Descriptor for *anon*
DEFINE tCover.%5
WORD tCover.%5.%map

! Pointer maps
DEFINE tCover.%4.%map
WORD 400
WORD GC_END

DEFINE tCover.%5.%map
WORD 400
WORD GC_END

! End of file
]]*)
