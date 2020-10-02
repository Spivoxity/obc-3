MODULE tIter07;

IMPORT Out;

(* Iterators over lists *)

TYPE List = POINTER TO Cell;
  Cell = RECORD head: INTEGER; tail: List END;

PROCEDURE MakeList(s: ARRAY OF CHAR): List;
  VAR i: INTEGER; p, q: List;
BEGIN
  p := NIL;
  FOR i := LEN(s)-2 TO 0 BY -1 DO
    NEW(q); 
    q.head := ORD(s[i]) - ORD('0');
    q.tail := p; p := q
  END;
  RETURN p
END MakeList;

TYPE Visitor = POINTER TO VRec;
  VRec = RECORD visit: PROCEDURE (self: Visitor; n: INTEGER) END;

PROCEDURE Accept(p: List; v: Visitor);
  VAR q: List;
BEGIN
  q := p;
  WHILE q # NIL DO v.visit(v, q.head); q := q.tail END
END Accept;

TYPE Counter = POINTER TO CRec;
  CRec = RECORD (VRec) count: INTEGER END;

PROCEDURE CVisit(self0: Visitor; n: INTEGER);
  VAR self: Counter;
BEGIN
  self := self0(Counter); (* Type cast!!! *)
  self.count := self.count+1
END CVisit;

PROCEDURE Length(p: List): INTEGER;
  VAR c: Counter;
BEGIN
  NEW(c); c.visit := CVisit; c.count := 0;
  Accept(p, c);
  RETURN c.count
END Length;

TYPE Reverser = POINTER TO RRec;
  RRec = RECORD (VRec) list: List END;

PROCEDURE RVisit(self0: Visitor; n: INTEGER);
  VAR self: Reverser; p: List;
BEGIN
  self := self0(Reverser); (* Type cast!!! *)
  NEW(p); p.head := n; p.tail := self.list; self.list := p
END RVisit;

PROCEDURE Reverse(p: List): List;
  VAR r: Reverser;
BEGIN
  NEW(r); r.visit := RVisit; r.list := NIL;
  Accept(p, r);
  RETURN r.list
END Reverse;

PROCEDURE PVisit(self: Visitor; n: INTEGER);
BEGIN
  Out.Int(n, 0)
END PVisit;

PROCEDURE Print(p: List);
  VAR v: Visitor;
BEGIN
  NEW(v); v.visit := PVisit;
  Accept(p, v)
END Print;

PROCEDURE Main;
  VAR p: List;
BEGIN
  p := MakeList("31415926");
  Out.Int(Length(p), 0); Out.Ln;
  Print(Reverse(p)); Out.Ln
END Main;

BEGIN
  Main
END tIter07.

(*<<
8
62951413
>>*)

(*[[
!! (SYMFILE #tIter07 STAMP #tIter07.%main 1 #tIter07.m)
!! (CHKSUM STAMP)
!! 
MODULE tIter07 STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tIter07.MakeList 12 3 0x0010c001
! PROCEDURE MakeList(s: ARRAY OF CHAR): List;
!   p := NIL;
CONST 0
STLW -8
!   FOR i := LEN(s)-2 TO 0 BY -1 DO
LDLW 16
CONST 2
MINUS
STLW -4
LABEL L2
LDLW -4
JLTZ L3
!     NEW(q); 
CONST 8
GLOBAL tIter07.Cell
GLOBAL NEW
CALLW 2
STLW -12
!     q.head := ORD(s[i]) - ORD('0');
LDLW 12
LDLW -4
LDLW 16
BOUND 16
LDIC
CONST 48
MINUS
LDLW -12
NCHECK 16
STOREW
!     q.tail := p; p := q
LDLW -8
LDLW -12
NCHECK 17
STNW 4
LDLW -12
STLW -8
!   FOR i := LEN(s)-2 TO 0 BY -1 DO
DECL -4
JUMP L2
LABEL L3
!   RETURN p
LDLW -8
RETURN
END

PROC tIter07.Accept 4 4 0x00310001
! PROCEDURE Accept(p: List; v: Visitor);
!   q := p;
LDLW 12
STLW -4
LABEL L4
!   WHILE q # NIL DO v.visit(v, q.head); q := q.tail END
LDLW -4
JEQZ L6
LDLW -4
NCHECK 29
LOADW
LDLW 16
LDLW 16
NCHECK 29
LOADW
NCHECK 29
CALL 2
LDLW -4
NCHECK 29
LDNW 4
STLW -4
JUMP L4
LABEL L6
RETURN
END

PROC tIter07.CVisit 4 4 0x00110001
! PROCEDURE CVisit(self0: Visitor; n: INTEGER);
!   self := self0(Counter); (* Type cast!!! *)
LDLW 12
DUP 0
NCHECK 38
LDNW -4
DUP 0
LDNW 4
CONST 1
JGEQ L9
POP 1
JUMP L8
LABEL L9
LDNW 8
LDNW 4
GLOBAL tIter07.CRec
JEQ L7
LABEL L8
ERROR E_CAST 38
LABEL L7
STLW -4
!   self.count := self.count+1
LDLW -4
NCHECK 39
LDNW 4
INC
LDLW -4
NCHECK 39
STNW 4
RETURN
END

PROC tIter07.Length 4 3 0x00110001
! PROCEDURE Length(p: List): INTEGER;
!   NEW(c); c.visit := CVisit; c.count := 0;
CONST 8
GLOBAL tIter07.CRec
GLOBAL NEW
CALLW 2
STLW -4
GLOBAL tIter07.CVisit
LDLW -4
NCHECK 45
STOREW
CONST 0
LDLW -4
NCHECK 45
STNW 4
!   Accept(p, c);
LDLW -4
LDLW 12
GLOBAL tIter07.Accept
CALL 2
!   RETURN c.count
LDLW -4
NCHECK 47
LDNW 4
RETURN
END

PROC tIter07.RVisit 8 4 0x00118001
! PROCEDURE RVisit(self0: Visitor; n: INTEGER);
!   self := self0(Reverser); (* Type cast!!! *)
LDLW 12
DUP 0
NCHECK 56
LDNW -4
DUP 0
LDNW 4
CONST 1
JGEQ L12
POP 1
JUMP L11
LABEL L12
LDNW 8
LDNW 4
GLOBAL tIter07.RRec
JEQ L10
LABEL L11
ERROR E_CAST 56
LABEL L10
STLW -4
!   NEW(p); p.head := n; p.tail := self.list; self.list := p
CONST 8
GLOBAL tIter07.Cell
GLOBAL NEW
CALLW 2
STLW -8
LDLW 16
LDLW -8
NCHECK 57
STOREW
LDLW -4
NCHECK 57
LDNW 4
LDLW -8
NCHECK 57
STNW 4
LDLW -8
LDLW -4
NCHECK 57
STNW 4
RETURN
END

PROC tIter07.Reverse 4 3 0x00110001
! PROCEDURE Reverse(p: List): List;
!   NEW(r); r.visit := RVisit; r.list := NIL;
CONST 8
GLOBAL tIter07.RRec
GLOBAL NEW
CALLW 2
STLW -4
GLOBAL tIter07.RVisit
LDLW -4
NCHECK 63
STOREW
CONST 0
LDLW -4
NCHECK 63
STNW 4
!   Accept(p, r);
LDLW -4
LDLW 12
GLOBAL tIter07.Accept
CALL 2
!   RETURN r.list
LDLW -4
NCHECK 65
LDNW 4
RETURN
END

PROC tIter07.PVisit 0 3 0x00100001
! PROCEDURE PVisit(self: Visitor; n: INTEGER);
!   Out.Int(n, 0)
CONST 0
LDLW 16
GLOBAL Out.Int
CALL 2
RETURN
END

PROC tIter07.Print 4 3 0x00110001
! PROCEDURE Print(p: List);
!   NEW(v); v.visit := PVisit;
CONST 4
GLOBAL tIter07.VRec
GLOBAL NEW
CALLW 2
STLW -4
GLOBAL tIter07.PVisit
LDLW -4
NCHECK 76
STOREW
!   Accept(p, v)
LDLW -4
LDLW 12
GLOBAL tIter07.Accept
CALL 2
RETURN
END

PROC tIter07.Main 4 3 0x00010001
! PROCEDURE Main;
!   p := MakeList("31415926");
CONST 9
GLOBAL tIter07.%1
GLOBAL tIter07.MakeList
CALLW 2
STLW -4
!   Out.Int(Length(p), 0); Out.Ln;
CONST 0
LDLW -4
GLOBAL tIter07.Length
CALLW 1
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
!   Print(Reverse(p)); Out.Ln
LDLW -4
GLOBAL tIter07.Reverse
CALLW 1
GLOBAL tIter07.Print
CALL 1
GLOBAL Out.Ln
CALL 0
RETURN
END

PROC tIter07.%main 0 1 0
!   Main
GLOBAL tIter07.Main
CALL 0
RETURN
END

! String "31415926"
DEFINE tIter07.%1
STRING 333134313539323600

! Descriptor for Cell
DEFINE tIter07.Cell
WORD 0x00000005
WORD 0
WORD tIter07.Cell.%anc

DEFINE tIter07.Cell.%anc
WORD tIter07.Cell

! Descriptor for VRec
DEFINE tIter07.VRec
WORD 0
WORD 0
WORD tIter07.VRec.%anc

DEFINE tIter07.VRec.%anc
WORD tIter07.VRec

! Descriptor for CRec
DEFINE tIter07.CRec
WORD 0
WORD 1
WORD tIter07.CRec.%anc

DEFINE tIter07.CRec.%anc
WORD tIter07.VRec
WORD tIter07.CRec

! Descriptor for RRec
DEFINE tIter07.RRec
WORD 0x00000005
WORD 1
WORD tIter07.RRec.%anc

DEFINE tIter07.RRec.%anc
WORD tIter07.VRec
WORD tIter07.RRec

! End of file
]]*)
