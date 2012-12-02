MODULE tString;

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

  COPY('abcdefabcdeabfabc', t);
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
END tString.

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
!! SYMFILE #tString STAMP #tString.%main 1
!! END STAMP
!! 
MODULE tString STAMP 0
IMPORT Out STAMP
IMPORT Strings STAMP
ENDHDR

PROC tString.StackPtr 1 4 0
! PROCEDURE StackPtr(): INTEGER;
!   RETURN SYSTEM.VAL(INTEGER, SYSTEM.ADR(x))
LOCAL -4
RETURNW
END

PROC tString.%main 1 28 0
!   i := StackPtr();
CONST tString.StackPtr
CALLW 0
STGW tString.i
!   NEW(s, 3);
CONST 3
CONST 1
CONST 1
CONST 0
CONST tString.s
CONST NEWFLEX
CALL 5
!   j := StackPtr();
CONST tString.StackPtr
CALLW 0
STGW tString.j
!   ASSERT(i=j);
LDGW tString.i
LDGW tString.j
JEQ 12
CONST 0
EASSERT 17
LABEL 12
!   s[0] := 'a';
CONST 97
LDGW tString.s
NCHECK 18
CONST 0
DUP 1
LDNW -4
LDNW 4
BOUND 18
STIC
!   s[1] := 'b';
CONST 98
LDGW tString.s
NCHECK 19
CONST 1
DUP 1
LDNW -4
LDNW 4
BOUND 19
STIC
!   s[2] := 0X;
CONST 0
LDGW tString.s
NCHECK 20
CONST 2
DUP 1
LDNW -4
LDNW 4
BOUND 20
STIC
!   Out.String(s^);
LDGW tString.s
NCHECK 21
DUP 0
LDNW -4
LDNW 4
SWAP
CONST Out.String
CALL 2
!   s[2] := 'c';
CONST 99
LDGW tString.s
NCHECK 22
CONST 2
DUP 1
LDNW -4
LDNW 4
BOUND 22
STIC
!   Out.String(s^);
LDGW tString.s
NCHECK 23
DUP 0
LDNW -4
LDNW 4
SWAP
CONST Out.String
CALL 2
!   Out.Ln;
CONST Out.Ln
CALL 0
!   FOR i := 0 TO Strings.Length(s^)-2 DO
LDGW tString.s
NCHECK 26
DUP 0
LDNW -4
LDNW 4
SWAP
CONST Strings.Length
CALLW 2
CONST 2
MINUS
STLW -4
CONST 0
STGW tString.i
JUMP 14
LABEL 13
!     s[i] := s[i+1]
LDGW tString.s
NCHECK 27
LDGW tString.i
INC
DUP 1
LDNW -4
LDNW 4
BOUND 27
LDIC
LDGW tString.s
NCHECK 27
LDGW tString.i
DUP 1
LDNW -4
LDNW 4
BOUND 27
STIC
!   FOR i := 0 TO Strings.Length(s^)-2 DO
LDGW tString.i
INC
STGW tString.i
LABEL 14
LDGW tString.i
LDLW -4
JLEQ 13
!   s[Strings.Length(s^)-1] := 0X;
CONST 0
LDGW tString.s
NCHECK 29
LDGW tString.s
NCHECK 29
DUP 0
LDNW -4
LDNW 4
SWAP
CONST Strings.Length
STKMAP 0x00000009
CALLW 2
DEC
DUP 1
LDNW -4
LDNW 4
BOUND 29
STIC
!   Out.String(s^);
LDGW tString.s
NCHECK 30
DUP 0
LDNW -4
LDNW 4
SWAP
CONST Out.String
CALL 2
!   Out.Ln;
CONST Out.Ln
CALL 0
!   FOR i := 0 TO 10 DO
CONST 0
STGW tString.i
JUMP 17
LABEL 16
!     Strings.Extract("abcdefghij", 0, i, t);
CONST 20
CONST tString.t
LDGW tString.i
CONST 0
CONST 11
CONST tString.%1
CONST Strings.Extract
CALL 6
!     Out.Int(Strings.Length(t), 0); Out.Char(' ');
CONST 0
CONST 20
CONST tString.t
CONST Strings.Length
CALLW 2
CONST Out.Int
CALL 2
CONST 32
ALIGNC
CONST Out.Char
CALL 1
!     Strings.Insert(t, 2, t);
CONST 20
CONST tString.t
CONST 2
CONST 20
CONST tString.t
CONST Strings.Insert
CALL 5
!     Strings.Insert('[', 0, t);
CONST 20
CONST tString.t
CONST 0
CONST 2
CONST tString.%7
CONST Strings.Insert
CALL 5
!     Strings.Append(']', t);
CONST 20
CONST tString.t
CONST 2
CONST tString.%8
CONST Strings.Append
CALL 4
!     Strings.Cap(t);
CONST 20
CONST tString.t
CONST Strings.Cap
CALL 2
!     Out.String(t); Out.Ln
CONST 20
CONST tString.t
CONST Out.String
CALL 2
CONST Out.Ln
CALL 0
!   FOR i := 0 TO 10 DO
LDGW tString.i
INC
STGW tString.i
LABEL 17
LDGW tString.i
CONST 10
JLEQ 16
!   Out.Int(Strings.Pos('', '', 0), 0);
CONST 0
CONST 0
CONST 1
CONST tString.%2
CONST 1
CONST tString.%2
CONST Strings.Pos
CALLW 5
CONST Out.Int
CALL 2
!   Out.Char(' '); Out.Int(Strings.Pos('abc', '', 0), 0);
CONST 32
ALIGNC
CONST Out.Char
CALL 1
CONST 0
CONST 0
CONST 1
CONST tString.%2
CONST 4
CONST tString.%3
CONST Strings.Pos
CALLW 5
CONST Out.Int
CALL 2
!   FOR i := 0 TO 3 DO
CONST 0
STGW tString.i
JUMP 19
LABEL 18
!     Out.Char(' '); Out.Int(Strings.Pos('', 'abc', i), 0);
CONST 32
ALIGNC
CONST Out.Char
CALL 1
CONST 0
LDGW tString.i
CONST 4
CONST tString.%3
CONST 1
CONST tString.%2
CONST Strings.Pos
CALLW 5
CONST Out.Int
CALL 2
!     Out.Char(' '); Out.Int(Strings.Pos('b', 'abc', i), 0)
CONST 32
ALIGNC
CONST Out.Char
CALL 1
CONST 0
LDGW tString.i
CONST 4
CONST tString.%3
CONST 2
CONST tString.%9
CONST Strings.Pos
CALLW 5
CONST Out.Int
CALL 2
!   FOR i := 0 TO 3 DO
LDGW tString.i
INC
STGW tString.i
LABEL 19
LDGW tString.i
CONST 3
JLEQ 18
!   Out.Ln;
CONST Out.Ln
CALL 0
!   COPY('abcdefabcdeabfabc', t);
CONST 20
CONST tString.t
CONST 18
CONST tString.%4
CONST COPY
CALL 4
!   i := Strings.Pos('abc', t, 0);
CONST 0
CONST 20
CONST tString.t
CONST 4
CONST tString.%3
CONST Strings.Pos
CALLW 5
STGW tString.i
!   Out.Int(i, 0);
CONST 0
LDGW tString.i
CONST Out.Int
CALL 2
JUMP 21
LABEL 20
!     i := Strings.Pos('abc', t, i+1);
LDGW tString.i
INC
CONST 20
CONST tString.t
CONST 4
CONST tString.%3
CONST Strings.Pos
CALLW 5
STGW tString.i
!     Out.Char(' '); Out.Int(i, 0)
CONST 32
ALIGNC
CONST Out.Char
CALL 1
CONST 0
LDGW tString.i
CONST Out.Int
CALL 2
LABEL 21
!   WHILE i >= 0 DO
LDGW tString.i
JGEQZ 20
!   Out.Ln;
CONST Out.Ln
CALL 0
!   Out.String('"a"'); Out.String('"'); Out.String("'");
CONST 4
CONST tString.%5
CONST Out.String
CALL 2
CONST 2
CONST tString.%10
CONST Out.String
CALL 2
CONST 2
CONST tString.%11
CONST Out.String
CALL 2
!   Out.String("'b'"); Out.Ln;
CONST 4
CONST tString.%6
CONST Out.String
CALL 2
CONST Out.Ln
CALL 0
!   NEW(s, 0);
CONST 0
CONST 1
CONST 1
CONST 0
CONST tString.s
CONST NEWFLEX
CALL 5
!   Out.Int(LEN(s^), 0); Out.Ln
CONST 0
LDGW tString.s
NCHECK 64
LDNW -4
LDNW 4
CONST Out.Int
CALL 2
CONST Out.Ln
CALL 0
RETURN
END

! Global variables
GLOBAL tString.i 4
GLOBAL tString.j 4
GLOBAL tString.s 4
GLOBAL tString.t 20

! Pointer map
DEFINE tString.%gcmap
WORD GC_BASE
WORD tString.s
WORD 0
WORD GC_END

! String "abcdefghij"
DEFINE tString.%1
STRING 6162636465666768696A00

! String ""
DEFINE tString.%2
STRING 00

! String "abc"
DEFINE tString.%3
STRING 61626300

! String "abcdefabcdeabfabc"
DEFINE tString.%4
STRING 616263646566616263646561626661626300

! String "\"a\""
DEFINE tString.%5
STRING 22612200

! String "'b'"
DEFINE tString.%6
STRING 27622700

! String "["
DEFINE tString.%7
STRING 5B00

! String "]"
DEFINE tString.%8
STRING 5D00

! String "b"
DEFINE tString.%9
STRING 6200

! String "\""
DEFINE tString.%10
STRING 2200

! String "'"
DEFINE tString.%11
STRING 2700

! End of file
]]*)
