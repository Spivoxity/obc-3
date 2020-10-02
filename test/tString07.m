MODULE tString07;

IMPORT Out, Strings, SYSTEM;

VAR i, j: INTEGER; s: POINTER TO ARRAY OF CHAR; t: ARRAY 20 OF CHAR;

PROCEDURE StackPtr(): INTEGER;
  VAR x: INTEGER;
BEGIN
  RETURN SYSTEM.VAL(INTEGER, SYSTEM.ADR(x))
END StackPtr;

BEGIN
  i := StackPtr();
  NEW(s, 3);
  j := StackPtr();
  ASSERT(i=j);
  s[0] := 'a';
  s[1] := 'b';
  s[2] := 0X;
  Out.String(s^);
  s[2] := 'c';
  Out.String(s^);
  Out.Ln;

  FOR i := 0 TO Strings.Length(s^)-2 DO
    s[i] := s[i+1]
  END;
  s[Strings.Length(s^)-1] := 0X;
  Out.String(s^);
  Out.Ln;

  FOR i := 0 TO 10 DO
    Strings.Extract("abcdefghij", 0, i, t);
    Out.Int(Strings.Length(t), 0); Out.Char(' ');
    Strings.Insert(t, 2, t);
    Strings.Insert('[', 0, t);
    Strings.Append(']', t);
    Strings.Cap(t);
    Out.String(t); Out.Ln
  END;

  Out.Int(Strings.Pos('', '', 0), 0);
  Out.Char(' '); Out.Int(Strings.Pos('abc', '', 0), 0);
  FOR i := 0 TO 3 DO
    Out.Char(' '); Out.Int(Strings.Pos('', 'abc', i), 0);
    Out.Char(' '); Out.Int(Strings.Pos('b', 'abc', i), 0)
  END;
  Out.Ln;

  t := 'abcdefabcdeabfabc';
  i := Strings.Pos('abc', t, 0);
  Out.Int(i, 0);
  WHILE i >= 0 DO
    i := Strings.Pos('abc', t, i+1);
    Out.Char(' '); Out.Int(i, 0)
  END;
  Out.Ln;

  Out.String('"a"'); Out.String('"'); Out.String("'");
  Out.String("'b'"); Out.Ln;

  NEW(s, 0);
  Out.Int(LEN(s^), 0); Out.Ln
END tString07.

(*<<
ababc
bc
0 []
1 [A]
2 [ABAB]
3 [ABABCC]
4 [ABABCDCD]
5 [ABABCDECDE]
6 [ABABCDEFCDEF]
7 [ABABCDEFGCDEFG]
8 [ABABCDEFGHCDEFGH]
9 [ABABCDEFGHICDEFGHI
10 [ABABCDEFGHIJCDEFGH
0 -1 0 1 1 1 2 -1 3 -1
0 6 14 -1
"a""''b'
0
>>*)

(*[[
!! (SYMFILE #tString07 STAMP #tString07.%main 1 #tString07.m)
!! (CHKSUM STAMP)
!! 
MODULE tString07 STAMP 0
IMPORT Out STAMP
IMPORT Strings STAMP
ENDHDR

PROC tString07.StackPtr 4 1 0
! PROCEDURE StackPtr(): INTEGER;
!   RETURN SYSTEM.VAL(INTEGER, SYSTEM.ADR(x))
LOCAL -4
RETURN
END

PROC tString07.%main 4 7 0
!   i := StackPtr();
GLOBAL tString07.StackPtr
CALLW 0
STGW tString07.i
!   NEW(s, 3);
CONST 3
CONST 1
CONST 1
CONST 0
GLOBAL NEWFLEX
CALLW 4
STGW tString07.s
!   j := StackPtr();
GLOBAL tString07.StackPtr
CALLW 0
STGW tString07.j
!   ASSERT(i=j);
LDGW tString07.i
LDGW tString07.j
JEQ L13
CONST 0
CONST 17
GLOBAL EASSERT
CALL 2
LABEL L13
!   s[0] := 'a';
CONST 97
LDGW tString07.s
NCHECK 18
CONST 0
DUP 1
LDNW -4
LDNW 4
BOUND 18
STIC
!   s[1] := 'b';
CONST 98
LDGW tString07.s
NCHECK 19
CONST 1
DUP 1
LDNW -4
LDNW 4
BOUND 19
STIC
!   s[2] := 0X;
CONST 0
LDGW tString07.s
NCHECK 20
CONST 2
DUP 1
LDNW -4
LDNW 4
BOUND 20
STIC
!   Out.String(s^);
LDGW tString07.s
NCHECK 21
DUP 0
LDNW -4
LDNW 4
SWAP
GLOBAL Out.String
CALL 2
!   s[2] := 'c';
CONST 99
LDGW tString07.s
NCHECK 22
CONST 2
DUP 1
LDNW -4
LDNW 4
BOUND 22
STIC
!   Out.String(s^);
LDGW tString07.s
NCHECK 23
DUP 0
LDNW -4
LDNW 4
SWAP
GLOBAL Out.String
CALL 2
!   Out.Ln;
GLOBAL Out.Ln
CALL 0
!   FOR i := 0 TO Strings.Length(s^)-2 DO
LDGW tString07.s
NCHECK 26
DUP 0
LDNW -4
LDNW 4
SWAP
GLOBAL Strings.Length
CALLW 2
CONST 2
MINUS
STLW -4
CONST 0
STGW tString07.i
LABEL L14
LDGW tString07.i
LDLW -4
JGT L15
!     s[i] := s[i+1]
LDGW tString07.s
NCHECK 27
LDGW tString07.i
INC
DUP 1
LDNW -4
LDNW 4
BOUND 27
LDIC
LDGW tString07.s
NCHECK 27
LDGW tString07.i
DUP 1
LDNW -4
LDNW 4
BOUND 27
STIC
!   FOR i := 0 TO Strings.Length(s^)-2 DO
LDGW tString07.i
INC
STGW tString07.i
JUMP L14
LABEL L15
!   s[Strings.Length(s^)-1] := 0X;
CONST 0
LDGW tString07.s
NCHECK 29
LDGW tString07.s
NCHECK 29
DUP 0
LDNW -4
LDNW 4
SWAP
GLOBAL Strings.Length
STKMAP 0x00000009
CALLW 2
DEC
DUP 1
LDNW -4
LDNW 4
BOUND 29
STIC
!   Out.String(s^);
LDGW tString07.s
NCHECK 30
DUP 0
LDNW -4
LDNW 4
SWAP
GLOBAL Out.String
CALL 2
!   Out.Ln;
GLOBAL Out.Ln
CALL 0
!   FOR i := 0 TO 10 DO
CONST 0
STGW tString07.i
LABEL L16
LDGW tString07.i
CONST 10
JGT L17
!     Strings.Extract("abcdefghij", 0, i, t);
CONST 20
GLOBAL tString07.t
LDGW tString07.i
CONST 0
CONST 11
GLOBAL tString07.%1
GLOBAL Strings.Extract
CALL 6
!     Out.Int(Strings.Length(t), 0); Out.Char(' ');
CONST 0
CONST 20
GLOBAL tString07.t
GLOBAL Strings.Length
CALLW 2
GLOBAL Out.Int
CALL 2
CONST 32
ALIGNC
GLOBAL Out.Char
CALL 1
!     Strings.Insert(t, 2, t);
CONST 20
GLOBAL tString07.t
CONST 2
CONST 20
GLOBAL tString07.t
GLOBAL Strings.Insert
CALL 5
!     Strings.Insert('[', 0, t);
CONST 20
GLOBAL tString07.t
CONST 0
CONST 2
GLOBAL tString07.%7
GLOBAL Strings.Insert
CALL 5
!     Strings.Append(']', t);
CONST 20
GLOBAL tString07.t
CONST 2
GLOBAL tString07.%8
GLOBAL Strings.Append
CALL 4
!     Strings.Cap(t);
CONST 20
GLOBAL tString07.t
GLOBAL Strings.Cap
CALL 2
!     Out.String(t); Out.Ln
CONST 20
GLOBAL tString07.t
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
!   FOR i := 0 TO 10 DO
LDGW tString07.i
INC
STGW tString07.i
JUMP L16
LABEL L17
!   Out.Int(Strings.Pos('', '', 0), 0);
CONST 0
CONST 0
CONST 1
GLOBAL tString07.%2
CONST 1
GLOBAL tString07.%2
GLOBAL Strings.Pos
CALLW 5
GLOBAL Out.Int
CALL 2
!   Out.Char(' '); Out.Int(Strings.Pos('abc', '', 0), 0);
CONST 32
ALIGNC
GLOBAL Out.Char
CALL 1
CONST 0
CONST 0
CONST 1
GLOBAL tString07.%2
CONST 4
GLOBAL tString07.%3
GLOBAL Strings.Pos
CALLW 5
GLOBAL Out.Int
CALL 2
!   FOR i := 0 TO 3 DO
CONST 0
STGW tString07.i
LABEL L18
LDGW tString07.i
CONST 3
JGT L19
!     Out.Char(' '); Out.Int(Strings.Pos('', 'abc', i), 0);
CONST 32
ALIGNC
GLOBAL Out.Char
CALL 1
CONST 0
LDGW tString07.i
CONST 4
GLOBAL tString07.%3
CONST 1
GLOBAL tString07.%2
GLOBAL Strings.Pos
CALLW 5
GLOBAL Out.Int
CALL 2
!     Out.Char(' '); Out.Int(Strings.Pos('b', 'abc', i), 0)
CONST 32
ALIGNC
GLOBAL Out.Char
CALL 1
CONST 0
LDGW tString07.i
CONST 4
GLOBAL tString07.%3
CONST 2
GLOBAL tString07.%9
GLOBAL Strings.Pos
CALLW 5
GLOBAL Out.Int
CALL 2
!   FOR i := 0 TO 3 DO
LDGW tString07.i
INC
STGW tString07.i
JUMP L18
LABEL L19
!   Out.Ln;
GLOBAL Out.Ln
CALL 0
!   t := 'abcdefabcdeabfabc';
CONST 20
GLOBAL tString07.t
CONST 18
GLOBAL tString07.%4
GLOBAL COPY
CALL 4
!   i := Strings.Pos('abc', t, 0);
CONST 0
CONST 20
GLOBAL tString07.t
CONST 4
GLOBAL tString07.%3
GLOBAL Strings.Pos
CALLW 5
STGW tString07.i
!   Out.Int(i, 0);
CONST 0
LDGW tString07.i
GLOBAL Out.Int
CALL 2
LABEL L20
!   WHILE i >= 0 DO
LDGW tString07.i
JLTZ L22
!     i := Strings.Pos('abc', t, i+1);
LDGW tString07.i
INC
CONST 20
GLOBAL tString07.t
CONST 4
GLOBAL tString07.%3
GLOBAL Strings.Pos
CALLW 5
STGW tString07.i
!     Out.Char(' '); Out.Int(i, 0)
CONST 32
ALIGNC
GLOBAL Out.Char
CALL 1
CONST 0
LDGW tString07.i
GLOBAL Out.Int
CALL 2
JUMP L20
LABEL L22
!   Out.Ln;
GLOBAL Out.Ln
CALL 0
!   Out.String('"a"'); Out.String('"'); Out.String("'");
CONST 4
GLOBAL tString07.%5
GLOBAL Out.String
CALL 2
CONST 2
GLOBAL tString07.%10
GLOBAL Out.String
CALL 2
CONST 2
GLOBAL tString07.%11
GLOBAL Out.String
CALL 2
!   Out.String("'b'"); Out.Ln;
CONST 4
GLOBAL tString07.%6
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
!   NEW(s, 0);
CONST 0
CONST 1
CONST 1
CONST 0
GLOBAL NEWFLEX
CALLW 4
STGW tString07.s
!   Out.Int(LEN(s^), 0); Out.Ln
CONST 0
LDGW tString07.s
NCHECK 64
LDNW -4
LDNW 4
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tString07.i 4
GLOVAR tString07.j 4
GLOVAR tString07.s 4
GLOVAR tString07.t 20

! Global pointer map
DEFINE tString07.%gcmap
WORD GC_POINTER
WORD tString07.s
WORD GC_END

! String "abcdefghij"
DEFINE tString07.%1
STRING 6162636465666768696A00

! String ""
DEFINE tString07.%2
STRING 00

! String "abc"
DEFINE tString07.%3
STRING 61626300

! String "abcdefabcdeabfabc"
DEFINE tString07.%4
STRING 616263646566616263646561626661626300

! String "\"a\""
DEFINE tString07.%5
STRING 22612200

! String "'b'"
DEFINE tString07.%6
STRING 27622700

! String "["
DEFINE tString07.%7
STRING 5B00

! String "]"
DEFINE tString07.%8
STRING 5D00

! String "b"
DEFINE tString07.%9
STRING 6200

! String "\""
DEFINE tString07.%10
STRING 2200

! String "'"
DEFINE tString07.%11
STRING 2700

! End of file
]]*)
