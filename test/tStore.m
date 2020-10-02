MODULE tStore;

(*<<
   -1   -1
 1234   -1
 4321 2345
 4567
abel = 2345
john = 4567
mike = 4321
zeke = 3456
>>*)

IMPORT Out;

CONST 
  MAXSTR = 20;
  MAXVAL = 100;

TYPE string = ARRAY MAXSTR OF CHAR;

VAR N: INTEGER;
  name: ARRAY MAXVAL OF string;
  value: ARRAY MAXVAL OF INTEGER;

PROCEDURE Find(s: string): INTEGER;
  VAR a, b, m: INTEGER;
BEGIN
  a := -1; b := N;
  WHILE a+1 # b DO
    m := (a+b) DIV 2;
    IF name[m] <= s THEN
      a := m
    ELSE
      b := m
    END
  END;
  RETURN a
END Find;

PROCEDURE Store(s: string; v: INTEGER);
  VAR i, j: INTEGER;
BEGIN
  i := Find(s);
  IF (i >= 0) & (i < N) & (name[i] = s) THEN
    value[i] := v
  ELSE
    j := N; N := N+1;
    WHILE j > i+1 DO
      name[j] := name[j-1];
      value[j] := value[j-1];
      j := j-1
    END;
    name[j] := s;
    value[j] := v
  END
END Store;

PROCEDURE Recall(s: string): INTEGER;
  VAR i: INTEGER;
BEGIN
  i := Find(s);
  IF (i >= 0) & (i < N) & (name[i] = s) THEN
    RETURN value[i]
  ELSE
    RETURN -1
  END
END Recall;

PROCEDURE Test;
  VAR i: INTEGER;
BEGIN
  Out.Int(Recall("mike"), 5); Out.Int(Recall("abel"), 5); Out.Ln;
  Store("mike", 1234);
  Out.Int(Recall("mike"), 5); Out.Int(Recall("abel"), 5); Out.Ln; 
  Store("abel", 2345); Store("mike", 4321);
  Out.Int(Recall("mike"), 5); Out.Int(Recall("abel"), 5); Out.Ln; 
  Store("zeke", 3456); Store("john", 4567);
  Out.Int(Recall("john"), 5); Out.Ln;

  FOR i := 0 TO N-1 DO
    Out.String(name[i]); Out.String(" = "); 
    Out.Int(value[i], 0); Out.Ln
  END
END Test;

BEGIN
  Test
END tStore.

(*[[
!! (SYMFILE #tStore STAMP #tStore.%main 1 #tStore.m)
!! (CHKSUM STAMP)
!! 
MODULE tStore STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tStore.Find 32 6 0
! PROCEDURE Find(s: string): INTEGER;
LOCAL -32
LDLW 12
CONST 20
FIXCOPY
!   a := -1; b := N;
CONST -1
STLW -4
LDGW tStore.N
STLW -8
LABEL L6
!   WHILE a+1 # b DO
LDLW -4
INC
LDLW -8
JEQ L8
!     m := (a+b) DIV 2;
LDLW -4
LDLW -8
PLUS
CONST 2
DIV
STLW -12
!     IF name[m] <= s THEN
CONST 20
LOCAL -32
CONST 20
GLOBAL tStore.name
LDLW -12
CONST 100
BOUND 32
CONST 20
TIMES
OFFSET
GLOBAL COMPARE
CALLW 4
JGTZ L11
!       a := m
LDLW -12
STLW -4
JUMP L6
LABEL L11
!       b := m
LDLW -12
STLW -8
JUMP L6
LABEL L8
!   RETURN a
LDLW -4
RETURN
END

PROC tStore.Store 28 6 0
! PROCEDURE Store(s: string; v: INTEGER);
LOCAL -28
LDLW 12
CONST 20
FIXCOPY
!   i := Find(s);
LOCAL -28
GLOBAL tStore.Find
CALLW 1
STLW -4
!   IF (i >= 0) & (i < N) & (name[i] = s) THEN
LDLW -4
JLTZ L17
LDLW -4
LDGW tStore.N
JGEQ L17
CONST 20
LOCAL -28
CONST 20
GLOBAL tStore.name
LDLW -4
CONST 100
BOUND 45
CONST 20
TIMES
OFFSET
GLOBAL COMPARE
CALLW 4
JNEQZ L17
!     value[i] := v
LDLW 16
GLOBAL tStore.value
LDLW -4
CONST 100
BOUND 46
STIW
RETURN
LABEL L17
!     j := N; N := N+1;
LDGW tStore.N
STLW -8
LDGW tStore.N
INC
STGW tStore.N
LABEL L13
!     WHILE j > i+1 DO
LDLW -8
LDLW -4
INC
JLEQ L15
!       name[j] := name[j-1];
GLOBAL tStore.name
LDLW -8
CONST 100
BOUND 50
CONST 20
TIMES
OFFSET
GLOBAL tStore.name
LDLW -8
DEC
CONST 100
BOUND 50
CONST 20
TIMES
OFFSET
CONST 20
FIXCOPY
!       value[j] := value[j-1];
GLOBAL tStore.value
LDLW -8
DEC
CONST 100
BOUND 51
LDIW
GLOBAL tStore.value
LDLW -8
CONST 100
BOUND 51
STIW
!       j := j-1
DECL -8
JUMP L13
LABEL L15
!     name[j] := s;
GLOBAL tStore.name
LDLW -8
CONST 100
BOUND 54
CONST 20
TIMES
OFFSET
LOCAL -28
CONST 20
FIXCOPY
!     value[j] := v
LDLW 16
GLOBAL tStore.value
LDLW -8
CONST 100
BOUND 55
STIW
RETURN
END

PROC tStore.Recall 24 6 0
! PROCEDURE Recall(s: string): INTEGER;
LOCAL -24
LDLW 12
CONST 20
FIXCOPY
!   i := Find(s);
LOCAL -24
GLOBAL tStore.Find
CALLW 1
STLW -4
!   IF (i >= 0) & (i < N) & (name[i] = s) THEN
LDLW -4
JLTZ L22
LDLW -4
LDGW tStore.N
JGEQ L22
CONST 20
LOCAL -24
CONST 20
GLOBAL tStore.name
LDLW -4
CONST 100
BOUND 63
CONST 20
TIMES
OFFSET
GLOBAL COMPARE
CALLW 4
JNEQZ L22
!     RETURN value[i]
GLOBAL tStore.value
LDLW -4
CONST 100
BOUND 64
LDIW
RETURN
LABEL L22
!     RETURN -1
CONST -1
RETURN
END

PROC tStore.Test 8 4 0
! PROCEDURE Test;
!   Out.Int(Recall("mike"), 5); Out.Int(Recall("abel"), 5); Out.Ln;
CONST 5
GLOBAL tStore.%1
GLOBAL tStore.Recall
CALLW 1
GLOBAL Out.Int
CALL 2
CONST 5
GLOBAL tStore.%2
GLOBAL tStore.Recall
CALLW 1
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
!   Store("mike", 1234);
CONST 1234
GLOBAL tStore.%1
GLOBAL tStore.Store
CALL 2
!   Out.Int(Recall("mike"), 5); Out.Int(Recall("abel"), 5); Out.Ln; 
CONST 5
GLOBAL tStore.%1
GLOBAL tStore.Recall
CALLW 1
GLOBAL Out.Int
CALL 2
CONST 5
GLOBAL tStore.%2
GLOBAL tStore.Recall
CALLW 1
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
!   Store("abel", 2345); Store("mike", 4321);
CONST 2345
GLOBAL tStore.%2
GLOBAL tStore.Store
CALL 2
CONST 4321
GLOBAL tStore.%1
GLOBAL tStore.Store
CALL 2
!   Out.Int(Recall("mike"), 5); Out.Int(Recall("abel"), 5); Out.Ln; 
CONST 5
GLOBAL tStore.%1
GLOBAL tStore.Recall
CALLW 1
GLOBAL Out.Int
CALL 2
CONST 5
GLOBAL tStore.%2
GLOBAL tStore.Recall
CALLW 1
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
!   Store("zeke", 3456); Store("john", 4567);
CONST 3456
GLOBAL tStore.%3
GLOBAL tStore.Store
CALL 2
CONST 4567
GLOBAL tStore.%4
GLOBAL tStore.Store
CALL 2
!   Out.Int(Recall("john"), 5); Out.Ln;
CONST 5
GLOBAL tStore.%4
GLOBAL tStore.Recall
CALLW 1
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
!   FOR i := 0 TO N-1 DO
LDGW tStore.N
DEC
STLW -8
CONST 0
STLW -4
LABEL L25
LDLW -4
LDLW -8
JGT L26
!     Out.String(name[i]); Out.String(" = "); 
CONST 20
GLOBAL tStore.name
LDLW -4
CONST 100
BOUND 82
CONST 20
TIMES
OFFSET
GLOBAL Out.String
CALL 2
CONST 4
GLOBAL tStore.%5
GLOBAL Out.String
CALL 2
!     Out.Int(value[i], 0); Out.Ln
CONST 0
GLOBAL tStore.value
LDLW -4
CONST 100
BOUND 83
LDIW
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
!   FOR i := 0 TO N-1 DO
INCL -4
JUMP L25
LABEL L26
RETURN
END

PROC tStore.%main 0 1 0
!   Test
GLOBAL tStore.Test
CALL 0
RETURN
END

! Global variables
GLOVAR tStore.N 4
GLOVAR tStore.name 2000
GLOVAR tStore.value 400

! String "mike"
DEFINE tStore.%1
STRING 6D696B6500

! String "abel"
DEFINE tStore.%2
STRING 6162656C00

! String "zeke"
DEFINE tStore.%3
STRING 7A656B6500

! String "john"
DEFINE tStore.%4
STRING 6A6F686E00

! String " = "
DEFINE tStore.%5
STRING 203D2000

! End of file
]]*)
