MODULE tLocalType;

(*<<
(((.(..))((..)(.(..))))(((..)(.(..)))((.(..))((..)(.(..))))))
((..(...))(.(...)(..(...)))((...)(..(...))(.(...)(..(...)))))
>>*)

IMPORT Out;

PROCEDURE First;

TYPE 
  tree = POINTER TO node;
  node = RECORD left, right: tree END;

PROCEDURE Build(n: INTEGER): tree;
  VAR t: tree;
BEGIN
  IF n <= 1 THEN
    RETURN NIL
  ELSE
    NEW(t);
    t.left := Build(n-2);
    t.right := Build(n-1);
    RETURN t
  END
END Build;

PROCEDURE Print(t:tree);
BEGIN
  IF NIL = t THEN
    Out.Char('.')
  ELSE
    Out.Char('(');
    Print(t.left);
    Print(t.right);
    Out.Char(')')
  END
END Print;

VAR p: tree;

BEGIN 
  p := Build(7);
  Print(p); Out.Ln();
END First;

PROCEDURE Second;

TYPE 
  tree = POINTER TO node;
  node = RECORD left, mid, right: tree END;

PROCEDURE Build(n: INTEGER): tree;
  VAR t: tree;
BEGIN
  IF n <= 2 THEN
    RETURN NIL
  ELSE
    NEW(t);
    t.left := Build(n-3);
    t.mid := Build(n-2);
    t.right := Build(n-1);
    RETURN t
  END
END Build;

PROCEDURE Print(t:tree);
BEGIN
  IF NIL = t THEN
    Out.Char('.')
  ELSE
    Out.Char('(');
    Print(t.left);
    Print(t.mid);
    Print(t.right);
    Out.Char(')')
  END
END Print;

VAR p: tree;

BEGIN 
  p := Build(7);
  Print(p); Out.Ln();
END Second;

BEGIN
  First;
  Second
END tLocalType.

(*[[
!! (SYMFILE #tLocalType STAMP #tLocalType.%main 1 #tLocalType.m)
!! (CHKSUM STAMP)
!! 
MODULE tLocalType STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tLocalType.%2.Build 8 3 0x00008001
! PROCEDURE Build(n: INTEGER): tree;
SAVELINK
!   IF n <= 1 THEN
LDLW 12
CONST 1
JGT L9
!     RETURN NIL
CONST 0
RETURNW
LABEL L9
!     NEW(t);
CONST 8
GLOBAL tLocalType.%1
GLOBAL NEW
CALLW 2
STLW -8
!     t.left := Build(n-2);
LDLW 12
CONST 2
MINUS
LDLW -4
LINK
GLOBAL tLocalType.%2.Build
CALLW 1
LDLW -8
NCHECK 23
STOREW
!     t.right := Build(n-1);
LDLW 12
DEC
LDLW -4
LINK
GLOBAL tLocalType.%2.Build
CALLW 1
LDLW -8
NCHECK 24
STNW 4
!     RETURN t
LDLW -8
RETURNW
END

PROC tLocalType.%3.Print 4 3 0x00100001
! PROCEDURE Print(t:tree);
SAVELINK
!   IF NIL = t THEN
LDLW 12
JNEQZ L12
!     Out.Char('.')
CONST 46
ALIGNC
GLOBAL Out.Char
CALL 1
RETURN
LABEL L12
!     Out.Char('(');
CONST 40
ALIGNC
GLOBAL Out.Char
CALL 1
!     Print(t.left);
LDLW 12
NCHECK 35
LOADW
LDLW -4
LINK
GLOBAL tLocalType.%3.Print
CALL 1
!     Print(t.right);
LDLW 12
NCHECK 36
LDNW 4
LDLW -4
LINK
GLOBAL tLocalType.%3.Print
CALL 1
!     Out.Char(')')
CONST 41
ALIGNC
GLOBAL Out.Char
CALL 1
RETURN
END

PROC tLocalType.First 4 2 0x00010001
! PROCEDURE First;
!   p := Build(7);
CONST 7
LOCAL 0
LINK
GLOBAL tLocalType.%2.Build
CALLW 1
STLW -4
!   Print(p); Out.Ln();
LDLW -4
LOCAL 0
LINK
GLOBAL tLocalType.%3.Print
CALL 1
GLOBAL Out.Ln
CALL 0
RETURN
END

PROC tLocalType.%5.Build 8 3 0x00008001
! PROCEDURE Build(n: INTEGER): tree;
SAVELINK
!   IF n <= 2 THEN
LDLW 12
CONST 2
JGT L15
!     RETURN NIL
CONST 0
RETURNW
LABEL L15
!     NEW(t);
CONST 12
GLOBAL tLocalType.%4
GLOBAL NEW
CALLW 2
STLW -8
!     t.left := Build(n-3);
LDLW 12
CONST 3
MINUS
LDLW -4
LINK
GLOBAL tLocalType.%5.Build
CALLW 1
LDLW -8
NCHECK 61
STOREW
!     t.mid := Build(n-2);
LDLW 12
CONST 2
MINUS
LDLW -4
LINK
GLOBAL tLocalType.%5.Build
CALLW 1
LDLW -8
NCHECK 62
STNW 4
!     t.right := Build(n-1);
LDLW 12
DEC
LDLW -4
LINK
GLOBAL tLocalType.%5.Build
CALLW 1
LDLW -8
NCHECK 63
STNW 8
!     RETURN t
LDLW -8
RETURNW
END

PROC tLocalType.%6.Print 4 3 0x00100001
! PROCEDURE Print(t:tree);
SAVELINK
!   IF NIL = t THEN
LDLW 12
JNEQZ L18
!     Out.Char('.')
CONST 46
ALIGNC
GLOBAL Out.Char
CALL 1
RETURN
LABEL L18
!     Out.Char('(');
CONST 40
ALIGNC
GLOBAL Out.Char
CALL 1
!     Print(t.left);
LDLW 12
NCHECK 74
LOADW
LDLW -4
LINK
GLOBAL tLocalType.%6.Print
CALL 1
!     Print(t.mid);
LDLW 12
NCHECK 75
LDNW 4
LDLW -4
LINK
GLOBAL tLocalType.%6.Print
CALL 1
!     Print(t.right);
LDLW 12
NCHECK 76
LDNW 8
LDLW -4
LINK
GLOBAL tLocalType.%6.Print
CALL 1
!     Out.Char(')')
CONST 41
ALIGNC
GLOBAL Out.Char
CALL 1
RETURN
END

PROC tLocalType.Second 4 2 0x00010001
! PROCEDURE Second;
!   p := Build(7);
CONST 7
LOCAL 0
LINK
GLOBAL tLocalType.%5.Build
CALLW 1
STLW -4
!   Print(p); Out.Ln();
LDLW -4
LOCAL 0
LINK
GLOBAL tLocalType.%6.Print
CALL 1
GLOBAL Out.Ln
CALL 0
RETURN
END

PROC tLocalType.%main 0 1 0
!   First;
GLOBAL tLocalType.First
CALL 0
!   Second
GLOBAL tLocalType.Second
CALL 0
RETURN
END

! Descriptor for node
DEFINE tLocalType.%1
WORD 0x00000007
WORD 0
WORD tLocalType.%1.%anc

DEFINE tLocalType.%1.%anc
WORD tLocalType.%1

! Descriptor for node
DEFINE tLocalType.%4
WORD 0x0000000f
WORD 0
WORD tLocalType.%4.%anc

DEFINE tLocalType.%4.%anc
WORD tLocalType.%4

! End of file
]]*)
