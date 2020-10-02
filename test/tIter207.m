MODULE tIter207;

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
  VRec = RECORD END;

PROCEDURE (self: Visitor) Visit(n: INTEGER);
BEGIN
  Out.Int(n, 0)
END Visit;

PROCEDURE Print(p: List);
  VAR v: Visitor;
BEGIN
  NEW(v);
  Accept(p, v)
END Print;

PROCEDURE Accept(p: List; v: Visitor);
  VAR q: List;
BEGIN
  q := p;
  WHILE q # NIL DO v.Visit(q.head); q := q.tail END
END Accept;

TYPE Counter = POINTER TO CRec;
  CRec = RECORD (VRec) count: INTEGER END;

PROCEDURE (self: Counter) Visit(n: INTEGER);
BEGIN
  self.count := self.count+1
END Visit;

PROCEDURE Length(p: List): INTEGER;
  VAR c: Counter;
BEGIN
  NEW(c); c.count := 0;
  Accept(p, c);
  RETURN c.count
END Length;

TYPE Reverser = POINTER TO RRec;
  RRec = RECORD (VRec) list: List END;

PROCEDURE (self: Reverser) Visit(n: INTEGER);
  VAR p: List;
BEGIN
  NEW(p); p.head := n; p.tail := self.list; self.list := p
END Visit;

PROCEDURE Reverse(p: List): List;
  VAR r: Reverser;
BEGIN
  NEW(r); r.list := NIL;
  Accept(p, r);
  RETURN r.list
END Reverse;

PROCEDURE Main;
  VAR p: List;
BEGIN
  p := MakeList("31415926");
  Out.Int(Length(p), 0); Out.Ln;
  Print(Reverse(p)); Out.Ln
END Main;

BEGIN
  Main
END tIter207.

(*<<
8
62951413
>>*)

(*[[
!! (SYMFILE #tIter207 STAMP #tIter207.%main 1 #tIter207.m)
!! (CHKSUM STAMP)
!! 
MODULE tIter207 STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tIter207.MakeList 12 3 0x0010c001
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
GLOBAL tIter207.Cell
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
RETURNW
END

PROC tIter207.VRec.Visit 0 3 0x00100001
! PROCEDURE (self: Visitor) Visit(n: INTEGER);
!   Out.Int(n, 0)
CONST 0
LDLW 16
GLOBAL Out.Int
CALL 2
RETURN
END

PROC tIter207.Print 4 3 0x00110001
! PROCEDURE Print(p: List);
!   NEW(v);
CONST 0
GLOBAL tIter207.VRec
GLOBAL NEW
CALLW 2
STLW -4
!   Accept(p, v)
LDLW -4
LDLW 12
GLOBAL tIter207.Accept
CALL 2
RETURN
END

PROC tIter207.Accept 4 4 0x00310001
! PROCEDURE Accept(p: List; v: Visitor);
!   q := p;
LDLW 12
STLW -4
LABEL L4
!   WHILE q # NIL DO v.Visit(q.head); q := q.tail END
LDLW -4
JEQZ L6
LDLW -4
NCHECK 41
LOADW
LDLW 16
NCHECK 41
DUP 0
LDNW -4
LDNW 12
CALL 2
LDLW -4
NCHECK 41
LDNW 4
STLW -4
JUMP L4
LABEL L6
RETURN
END

PROC tIter207.CRec.Visit 0 3 0x00100001
! PROCEDURE (self: Counter) Visit(n: INTEGER);
!   self.count := self.count+1
LDLW 12
NCHECK 49
LOADW
INC
LDLW 12
NCHECK 49
STOREW
RETURN
END

PROC tIter207.Length 4 3 0x00110001
! PROCEDURE Length(p: List): INTEGER;
!   NEW(c); c.count := 0;
CONST 4
GLOBAL tIter207.CRec
GLOBAL NEW
CALLW 2
STLW -4
CONST 0
LDLW -4
NCHECK 55
STOREW
!   Accept(p, c);
LDLW -4
LDLW 12
GLOBAL tIter207.Accept
CALL 2
!   RETURN c.count
LDLW -4
NCHECK 57
LOADW
RETURNW
END

PROC tIter207.RRec.Visit 4 3 0x00110001
! PROCEDURE (self: Reverser) Visit(n: INTEGER);
!   NEW(p); p.head := n; p.tail := self.list; self.list := p
CONST 8
GLOBAL tIter207.Cell
GLOBAL NEW
CALLW 2
STLW -4
LDLW 16
LDLW -4
NCHECK 66
STOREW
LDLW 12
NCHECK 66
LOADW
LDLW -4
NCHECK 66
STNW 4
LDLW -4
LDLW 12
NCHECK 66
STOREW
RETURN
END

PROC tIter207.Reverse 4 3 0x00110001
! PROCEDURE Reverse(p: List): List;
!   NEW(r); r.list := NIL;
CONST 4
GLOBAL tIter207.RRec
GLOBAL NEW
CALLW 2
STLW -4
CONST 0
LDLW -4
NCHECK 72
STOREW
!   Accept(p, r);
LDLW -4
LDLW 12
GLOBAL tIter207.Accept
CALL 2
!   RETURN r.list
LDLW -4
NCHECK 74
LOADW
RETURNW
END

PROC tIter207.Main 4 3 0x00010001
! PROCEDURE Main;
!   p := MakeList("31415926");
CONST 9
GLOBAL tIter207.%1
GLOBAL tIter207.MakeList
CALLW 2
STLW -4
!   Out.Int(Length(p), 0); Out.Ln;
CONST 0
LDLW -4
GLOBAL tIter207.Length
CALLW 1
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
!   Print(Reverse(p)); Out.Ln
LDLW -4
GLOBAL tIter207.Reverse
CALLW 1
GLOBAL tIter207.Print
CALL 1
GLOBAL Out.Ln
CALL 0
RETURN
END

PROC tIter207.%main 0 1 0
!   Main
GLOBAL tIter207.Main
CALL 0
RETURN
END

! String "31415926"
DEFINE tIter207.%1
STRING 333134313539323600

! Descriptor for Cell
DEFINE tIter207.Cell
WORD 0x00000005
WORD 0
WORD tIter207.Cell.%anc

DEFINE tIter207.Cell.%anc
WORD tIter207.Cell

! Descriptor for VRec
DEFINE tIter207.VRec
WORD 0
WORD 0
WORD tIter207.VRec.%anc
WORD tIter207.VRec.Visit

DEFINE tIter207.VRec.%anc
WORD tIter207.VRec

! Descriptor for CRec
DEFINE tIter207.CRec
WORD 0
WORD 1
WORD tIter207.CRec.%anc
WORD tIter207.CRec.Visit

DEFINE tIter207.CRec.%anc
WORD tIter207.VRec
WORD tIter207.CRec

! Descriptor for RRec
DEFINE tIter207.RRec
WORD 0x00000003
WORD 1
WORD tIter207.RRec.%anc
WORD tIter207.RRec.Visit

DEFINE tIter207.RRec.%anc
WORD tIter207.VRec
WORD tIter207.RRec

! End of file
]]*)
