MODULE tSort;

IMPORT Out;

TYPE List = POINTER TO Cell;
  Cell = RECORD data: INTEGER; prev, next: List END;

PROCEDURE FindMin(xs: List): List;
  VAR p, q: List;
BEGIN
  p := xs.next;
  ASSERT(p # xs);
  q := p.next;
  WHILE q # xs DO
    IF q.data < p.data THEN p := q END;
    q := q.next
  END;
  RETURN p
END FindMin;

PROCEDURE Sort(xs: List; VAR ys: List);
  VAR p: List;
BEGIN
  NEW(ys);
  ys.prev := ys; ys.next := ys;

  WHILE xs.next # xs DO
    p := FindMin(xs);
    p.prev.next := p.next; p.next.prev := p.prev;
    p.prev := ys.prev; p.next := ys;
    ys.prev.next := p; ys.prev := p
  END
END Sort;

PROCEDURE Append(xs: List; d: INTEGER);
  VAR p: List;
BEGIN
  NEW(p); p.data := d;
  p.prev := xs.prev; p.next := xs;
  xs.prev.next := p; xs.prev := p
END Append;

PROCEDURE Test;
  VAR m, k: INTEGER; p, xs, ys: List;
BEGIN
  NEW(xs);
  xs.prev := xs; xs.next := xs;

  (* This is not very random ... *)
  m := 37;
  FOR k := 0 TO 24 DO 
    Out.Int(m, 3);
    Append(xs, m);
    m := (m + 61) MOD 100
  END;
  Out.Ln;

  Sort(xs, ys);

  p := ys.next;
  WHILE p # ys DO
    Out.Int(p.data, 3);
    p := p.next
  END;
  Out.Ln
END Test;

BEGIN 
  Test 
END tSort.

(*<<
 37 98 59 20 81 42  3 64 25 86 47  8 69 30 91 52 13 74 35 96 57 18 79 40  1
  1  3  8 13 18 20 25 30 35 37 40 42 47 52 57 59 64 69 74 79 81 86 91 96 98
>>*)

(*[[
!! SYMFILE #tSort STAMP #tSort.%main 1
!! END STAMP
!! 
MODULE tSort STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tSort.FindMin 2 12 0x00118001
! PROCEDURE FindMin(xs: List): List;
!   p := xs.next;
LDLW 12
NCHECK 11
LDNW 8
STLW -4
!   ASSERT(p # xs);
LDLW -4
LDLW 12
JNEQ 1
CONST 0
EASSERT 12
LABEL 1
!   q := p.next;
LDLW -4
NCHECK 13
LDNW 8
STLW -8
JUMP 3
LABEL 2
!     IF q.data < p.data THEN p := q END;
LDLW -8
NCHECK 15
LOADW
LDLW -4
NCHECK 15
LOADW
JGEQ 5
LDLW -8
STLW -4
LABEL 5
!     q := q.next
LDLW -8
NCHECK 16
LDNW 8
STLW -8
LABEL 3
!   WHILE q # xs DO
LDLW -8
LDLW 12
JNEQ 2
!   RETURN p
LDLW -4
RETURNW
END

PROC tSort.Sort 1 16 0x00310001
! PROCEDURE Sort(xs: List; VAR ys: List);
!   NEW(ys);
CONST 12
CONST tSort.Cell
LDLW 16
CONST NEW
CALL 3
!   ys.prev := ys; ys.next := ys;
LDLW 16
LOADW
LDLW 16
LOADW
NCHECK 25
STNW 4
LDLW 16
LOADW
LDLW 16
LOADW
NCHECK 25
STNW 8
JUMP 7
LABEL 6
!     p := FindMin(xs);
LDLW 12
CONST tSort.FindMin
CALLW 1
STLW -4
!     p.prev.next := p.next; p.next.prev := p.prev;
LDLW -4
NCHECK 29
LDNW 8
LDLW -4
NCHECK 29
LDNW 4
NCHECK 29
STNW 8
LDLW -4
NCHECK 29
LDNW 4
LDLW -4
NCHECK 29
LDNW 8
NCHECK 29
STNW 4
!     p.prev := ys.prev; p.next := ys;
LDLW 16
LOADW
NCHECK 30
LDNW 4
LDLW -4
NCHECK 30
STNW 4
LDLW 16
LOADW
LDLW -4
NCHECK 30
STNW 8
!     ys.prev.next := p; ys.prev := p
LDLW -4
LDLW 16
LOADW
NCHECK 31
LDNW 4
NCHECK 31
STNW 8
LDLW -4
LDLW 16
LOADW
NCHECK 31
STNW 4
LABEL 7
!   WHILE xs.next # xs DO
LDLW 12
NCHECK 27
LDNW 8
LDLW 12
JNEQ 6
RETURN
END

PROC tSort.Append 1 16 0x00110001
! PROCEDURE Append(xs: List; d: INTEGER);
!   NEW(p); p.data := d;
CONST 12
CONST tSort.Cell
LOCAL -4
CONST NEW
CALL 3
LDLW 16
LDLW -4
NCHECK 38
STOREW
!   p.prev := xs.prev; p.next := xs;
LDLW 12
NCHECK 39
LDNW 4
LDLW -4
NCHECK 39
STNW 4
LDLW 12
LDLW -4
NCHECK 39
STNW 8
!   xs.prev.next := p; xs.prev := p
LDLW -4
LDLW 12
NCHECK 40
LDNW 4
NCHECK 40
STNW 8
LDLW -4
LDLW 12
NCHECK 40
STNW 4
RETURN
END

PROC tSort.Test 5 16 0x00007001
! PROCEDURE Test;
!   NEW(xs);
CONST 12
CONST tSort.Cell
LOCAL -16
CONST NEW
CALL 3
!   xs.prev := xs; xs.next := xs;
LDLW -16
LDLW -16
NCHECK 47
STNW 4
LDLW -16
LDLW -16
NCHECK 47
STNW 8
!   m := 37;
CONST 37
STLW -4
!   FOR k := 0 TO 24 DO 
CONST 0
STLW -8
JUMP 9
LABEL 8
!     Out.Int(m, 3);
CONST 3
LDLW -4
CONST Out.Int
CALL 2
!     Append(xs, m);
LDLW -4
LDLW -16
CONST tSort.Append
CALL 2
!     m := (m + 61) MOD 100
LDLW -4
CONST 61
PLUS
CONST 100
MOD
STLW -4
!   FOR k := 0 TO 24 DO 
INCL -8
LABEL 9
LDLW -8
CONST 24
JLEQ 8
!   Out.Ln;
CONST Out.Ln
CALL 0
!   Sort(xs, ys);
LOCAL -20
LDLW -16
CONST tSort.Sort
CALL 2
!   p := ys.next;
LDLW -20
NCHECK 60
LDNW 8
STLW -12
JUMP 11
LABEL 10
!     Out.Int(p.data, 3);
CONST 3
LDLW -12
NCHECK 62
LOADW
CONST Out.Int
CALL 2
!     p := p.next
LDLW -12
NCHECK 63
LDNW 8
STLW -12
LABEL 11
!   WHILE p # ys DO
LDLW -12
LDLW -20
JNEQ 10
!   Out.Ln
CONST Out.Ln
CALL 0
RETURN
END

PROC tSort.%main 0 16 0
!   Test 
CONST tSort.Test
CALL 0
RETURN
END

! Descriptor for Cell
DEFINE tSort.Cell
WORD 0x0000000d
WORD 0
WORD tSort.Cell.%anc

DEFINE tSort.Cell.%anc
WORD tSort.Cell

! End of file
]]*)
