MODULE tTreeCopy;

IMPORT Random, Out;

TYPE Tree = POINTER TO Node;
  Node = RECORD data: INTEGER; left, right: Tree END;

PROCEDURE NewNode(x: INTEGER): Tree;
  VAR p: Tree;
BEGIN
  NEW(p); p.data := x; RETURN p
END NewNode;

PROCEDURE RandTree(d: INTEGER): Tree;
  VAR p: Tree;
BEGIN
  IF Random.Roll(10) < d THEN
    RETURN NIL
  ELSE
    p := NewNode(Random.Roll(10));
    p.left := RandTree(d+1);
    p.right := RandTree(d+1);
    RETURN p
  END
END RandTree;

PROCEDURE PrintTree(t: Tree);
BEGIN
  IF t = NIL THEN
    Out.Char('.')
  ELSE
    Out.Int(t.data, 0); PrintTree(t.left); PrintTree(t.right)
  END
END PrintTree;

PROCEDURE Copy(t: Tree): Tree;
  VAR p, q, r, u: Tree; sp: INTEGER;
    astack, bstack: ARRAY 1000 OF Tree;
BEGIN
  IF t = NIL THEN
    RETURN NIL
  ELSE
    sp := 0; 

    u := NewNode(t.data);
    astack[sp] := t.right; bstack[sp] := u; sp := sp+1;
    p := t.left; q := u;

    (* To do: copy p into q.left; then copy astack[i] into 
       bstack[i].right for each i. *)

    WHILE (p # NIL) OR (sp > 0) DO
      WHILE p # NIL DO
	r := NewNode(p.data);
	q.left := r;
  	astack[sp] := p.right; bstack[sp] := r; sp := sp+1;
	p := p.left; q := r
      END;

      sp := sp-1; p := astack[sp]; q := bstack[sp]; 
      IF p # NIL THEN
        r := NewNode(p.data);
        q.right := r;
  	astack[sp] := p.right; bstack[sp] := r; sp := sp+1;
        p := p.left; q := r
      END
    END
  END;

  RETURN u
END Copy;

PROCEDURE FlatPrint(t: Tree);
BEGIN
  IF t # NIL THEN
    FlatPrint(t.left);
    Out.Int(t.data, 0);
    FlatPrint(t.right)
  END
END FlatPrint;

PROCEDURE PrintFlat(t: Tree);
  VAR p: Tree;
BEGIN
  WHILE t # NIL DO
    IF t.left = NIL THEN
      Out.Int(t.data, 0);
      t := t.right
    ELSE
      p := t.left;
      t.left := p.right;
      p.right := t;
      t := p
    END
  END
END PrintFlat;

PROCEDURE Test;
  VAR t, u: Tree;
BEGIN
  t := RandTree(0);
  PrintTree(t); Out.Ln;
  FlatPrint(t); Out.Ln;
  u := Copy(t);
  PrintTree(u); Out.Ln;
  PrintFlat(u); Out.Ln
END Test;

BEGIN
  Test
END tTreeCopy.

(*<<
19801.9.65....60.75.44....28...471....9.2..
195608054476829174192
19801.9.65....60.75.44....28...471....9.2..
195608054476829174192
>>*)

(*[[
!! (SYMFILE #tTreeCopy STAMP #tTreeCopy.%main 1 #tTreeCopy.m)
!! (CHKSUM STAMP)
!! 
MODULE tTreeCopy STAMP 0
IMPORT Random STAMP
IMPORT Out STAMP
ENDHDR

PROC tTreeCopy.NewNode 4 3 0x00010001
! PROCEDURE NewNode(x: INTEGER): Tree;
!   NEW(p); p.data := x; RETURN p
CONST 12
GLOBAL tTreeCopy.Node
GLOBAL NEW
CALLW 2
STLW -4
LDLW 12
LDLW -4
NCHECK 11
STOREW
LDLW -4
RETURN
END

PROC tTreeCopy.RandTree 4 3 0x00010001
! PROCEDURE RandTree(d: INTEGER): Tree;
!   IF Random.Roll(10) < d THEN
CONST 10
GLOBAL Random.Roll
CALLW 1
LDLW 12
JGEQ L3
!     RETURN NIL
CONST 0
RETURN
LABEL L3
!     p := NewNode(Random.Roll(10));
CONST 10
GLOBAL Random.Roll
CALLW 1
GLOBAL tTreeCopy.NewNode
CALLW 1
STLW -4
!     p.left := RandTree(d+1);
LDLW 12
INC
GLOBAL tTreeCopy.RandTree
CALLW 1
LDLW -4
NCHECK 21
STNW 4
!     p.right := RandTree(d+1);
LDLW 12
INC
GLOBAL tTreeCopy.RandTree
CALLW 1
LDLW -4
NCHECK 22
STNW 8
!     RETURN p
LDLW -4
RETURN
END

PROC tTreeCopy.PrintTree 0 3 0x00100001
! PROCEDURE PrintTree(t: Tree);
!   IF t = NIL THEN
LDLW 12
JNEQZ L6
!     Out.Char('.')
CONST 46
ALIGNC
GLOBAL Out.Char
CALL 1
RETURN
LABEL L6
!     Out.Int(t.data, 0); PrintTree(t.left); PrintTree(t.right)
CONST 0
LDLW 12
NCHECK 32
LOADW
GLOBAL Out.Int
CALL 2
LDLW 12
NCHECK 32
LDNW 4
GLOBAL tTreeCopy.PrintTree
CALL 1
LDLW 12
NCHECK 32
LDNW 8
GLOBAL tTreeCopy.PrintTree
CALL 1
RETURN
END

PROC tTreeCopy.Copy 8020 4 tTreeCopy.Copy.%map
! PROCEDURE Copy(t: Tree): Tree;
!   IF t = NIL THEN
LDLW 12
JNEQZ L19
!     RETURN NIL
CONST 0
RETURN
LABEL L19
!     sp := 0; 
CONST 0
STLW -20
!     u := NewNode(t.data);
LDLW 12
NCHECK 45
LOADW
GLOBAL tTreeCopy.NewNode
CALLW 1
STLW -16
!     astack[sp] := t.right; bstack[sp] := u; sp := sp+1;
LDLW 12
NCHECK 46
LDNW 8
LOCAL -4020
LDLW -20
CONST 1000
BOUND 46
STIW
LDLW -16
LOCAL -8020
LDLW -20
CONST 1000
BOUND 46
STIW
INCL -20
!     p := t.left; q := u;
LDLW 12
NCHECK 47
LDNW 4
STLW -4
LDLW -16
STLW -8
LABEL L8
!     WHILE (p # NIL) OR (sp > 0) DO
LDLW -4
JNEQZ L9
LDLW -20
JLEQZ L10
LABEL L9
!       WHILE p # NIL DO
LDLW -4
JEQZ L13
! 	r := NewNode(p.data);
LDLW -4
NCHECK 54
LOADW
GLOBAL tTreeCopy.NewNode
CALLW 1
STLW -12
! 	q.left := r;
LDLW -12
LDLW -8
NCHECK 55
STNW 4
!   	astack[sp] := p.right; bstack[sp] := r; sp := sp+1;
LDLW -4
NCHECK 56
LDNW 8
LOCAL -4020
LDLW -20
CONST 1000
BOUND 56
STIW
LDLW -12
LOCAL -8020
LDLW -20
CONST 1000
BOUND 56
STIW
INCL -20
! 	p := p.left; q := r
LDLW -4
NCHECK 57
LDNW 4
STLW -4
LDLW -12
STLW -8
JUMP L9
LABEL L13
!       sp := sp-1; p := astack[sp]; q := bstack[sp]; 
DECL -20
LOCAL -4020
LDLW -20
CONST 1000
BOUND 60
LDIW
STLW -4
LOCAL -8020
LDLW -20
CONST 1000
BOUND 60
LDIW
STLW -8
!       IF p # NIL THEN
LDLW -4
JEQZ L8
!         r := NewNode(p.data);
LDLW -4
NCHECK 62
LOADW
GLOBAL tTreeCopy.NewNode
CALLW 1
STLW -12
!         q.right := r;
LDLW -12
LDLW -8
NCHECK 63
STNW 8
!   	astack[sp] := p.right; bstack[sp] := r; sp := sp+1;
LDLW -4
NCHECK 64
LDNW 8
LOCAL -4020
LDLW -20
CONST 1000
BOUND 64
STIW
LDLW -12
LOCAL -8020
LDLW -20
CONST 1000
BOUND 64
STIW
INCL -20
!         p := p.left; q := r
LDLW -4
NCHECK 65
LDNW 4
STLW -4
LDLW -12
STLW -8
JUMP L8
LABEL L10
!   RETURN u
LDLW -16
RETURN
END

PROC tTreeCopy.FlatPrint 0 3 0x00100001
! PROCEDURE FlatPrint(t: Tree);
!   IF t # NIL THEN
LDLW 12
JEQZ L22
!     FlatPrint(t.left);
LDLW 12
NCHECK 76
LDNW 4
GLOBAL tTreeCopy.FlatPrint
CALL 1
!     Out.Int(t.data, 0);
CONST 0
LDLW 12
NCHECK 77
LOADW
GLOBAL Out.Int
CALL 2
!     FlatPrint(t.right)
LDLW 12
NCHECK 78
LDNW 8
GLOBAL tTreeCopy.FlatPrint
CALL 1
LABEL L22
RETURN
END

PROC tTreeCopy.PrintFlat 4 3 0x00110001
! PROCEDURE PrintFlat(t: Tree);
LABEL L23
!   WHILE t # NIL DO
LDLW 12
JEQZ L25
!     IF t.left = NIL THEN
LDLW 12
NCHECK 86
LDNW 4
JNEQZ L28
!       Out.Int(t.data, 0);
CONST 0
LDLW 12
NCHECK 87
LOADW
GLOBAL Out.Int
CALL 2
!       t := t.right
LDLW 12
NCHECK 88
LDNW 8
STLW 12
JUMP L23
LABEL L28
!       p := t.left;
LDLW 12
NCHECK 90
LDNW 4
STLW -4
!       t.left := p.right;
LDLW -4
NCHECK 91
LDNW 8
LDLW 12
NCHECK 91
STNW 4
!       p.right := t;
LDLW 12
LDLW -4
NCHECK 92
STNW 8
!       t := p
LDLW -4
STLW 12
JUMP L23
LABEL L25
RETURN
END

PROC tTreeCopy.Test 8 2 0x00018001
! PROCEDURE Test;
!   t := RandTree(0);
CONST 0
GLOBAL tTreeCopy.RandTree
CALLW 1
STLW -4
!   PrintTree(t); Out.Ln;
LDLW -4
GLOBAL tTreeCopy.PrintTree
CALL 1
GLOBAL Out.Ln
CALL 0
!   FlatPrint(t); Out.Ln;
LDLW -4
GLOBAL tTreeCopy.FlatPrint
CALL 1
GLOBAL Out.Ln
CALL 0
!   u := Copy(t);
LDLW -4
GLOBAL tTreeCopy.Copy
CALLW 1
STLW -8
!   PrintTree(u); Out.Ln;
LDLW -8
GLOBAL tTreeCopy.PrintTree
CALL 1
GLOBAL Out.Ln
CALL 0
!   PrintFlat(u); Out.Ln
LDLW -8
GLOBAL tTreeCopy.PrintFlat
CALL 1
GLOBAL Out.Ln
CALL 0
RETURN
END

PROC tTreeCopy.%main 0 1 0
!   Test
GLOBAL tTreeCopy.Test
CALL 0
RETURN
END

! Descriptor for Node
DEFINE tTreeCopy.Node
WORD 0x0000000d
WORD 0
WORD tTreeCopy.Node.%anc

DEFINE tTreeCopy.Node.%anc
WORD tTreeCopy.Node

! Pointer maps
DEFINE tTreeCopy.Copy.%map
WORD 12
WORD -4
WORD -8
WORD -12
WORD -16
WORD GC_BLOCK
WORD -4020
WORD 1000
WORD GC_BLOCK
WORD -8020
WORD 1000
WORD GC_END

! End of file
]]*)
