MODULE tSet;

IMPORT Out;

VAR s: SET; n: INTEGER;

PROCEDURE Print;
  VAR i: INTEGER; b: BOOLEAN;
BEGIN
  FOR i := 0 TO 15 DO
    IF i IN s THEN Out.Int(i, 3) END
  END;
  FOR i := 16 TO 31 DO
    b := (i IN s);
    IF b THEN Out.Int(i, 3) END
  END;
  Out.Ln
END Print;

BEGIN
  n := MAX(SET);
  s := {0..-1}; IF s # {} THEN Out.String("Fail 1"); Out.Ln END;
  s := {0..30, 31}; 
  IF s # {MIN(SET) .. n} THEN Out.String("Fail 2"); Out.Ln END;

  n := 5;
  s := {1, 2, n..n+2, 10..12, 13..n+7};	(* 1 2 5 6 7 10 11 12 *)
  Print;
  s := s * {3..15} + {14..n+12};	(* 5 6 7 10 11 12 14 15 16 17 *)
  Print;
  INCL(s, 4); EXCL(s, 16);		(* 4 5 6 7 10 11 12 14 15 17 *)
  Print;
  s := s / {12..14};			(* 4 5 6 7 10 11 13 15 17 *)
  Print;
  s := (- s) * ({19..MAX(SET)}/(- {})); (* 0 1 2 3 8 9 12 14 16 18 *)
  Print;
  s := s - {1..2};			(* 0 3 8 9 12 14 16 18 *)
  Print
END tSet.

(*<<
  1  2  5  6  7 10 11 12
  5  6  7 10 11 12 14 15 16 17
  4  5  6  7 10 11 12 14 15 17
  4  5  6  7 10 11 13 15 17
  0  1  2  3  8  9 12 14 16 18
  0  3  8  9 12 14 16 18
>>*)

(*[[
!! SYMFILE #tSet STAMP #tSet.%main 1
!! END STAMP
!! 
MODULE tSet STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tSet.Print 2 3 0
! PROCEDURE Print;
!   FOR i := 0 TO 15 DO
CONST 0
STLW -4
JUMP 4
LABEL 3
!     IF i IN s THEN Out.Int(i, 3) END
LDLW -4
CONST 32
BOUND 11
BIT
LDGW tSet.s
BITAND
JEQZ 6
CONST 3
LDLW -4
GLOBAL Out.Int
CALL 2
LABEL 6
!   FOR i := 0 TO 15 DO
INCL -4
LABEL 4
LDLW -4
CONST 15
JLEQ 3
!   FOR i := 16 TO 31 DO
CONST 16
STLW -4
JUMP 8
LABEL 7
!     b := (i IN s);
LDLW -4
CONST 32
BOUND 14
BIT
LDGW tSet.s
BITAND
CONST 0
NEQ
STLC -5
!     IF b THEN Out.Int(i, 3) END
LDLC -5
JUMPF 10
CONST 3
LDLW -4
GLOBAL Out.Int
CALL 2
LABEL 10
!   FOR i := 16 TO 31 DO
INCL -4
LABEL 8
LDLW -4
CONST 31
JLEQ 7
!   Out.Ln
GLOBAL Out.Ln
CALL 0
RETURN
END

PROC tSet.%main 0 4 0
!   n := MAX(SET);
CONST 31
STGW tSet.n
!   s := {0..-1}; IF s # {} THEN Out.String("Fail 1"); Out.Ln END;
CONST 0
STGW tSet.s
LDGW tSet.s
JEQZ 12
CONST 7
GLOBAL tSet.%1
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
LABEL 12
!   s := {0..30, 31}; 
CONST -1
STGW tSet.s
!   IF s # {MIN(SET) .. n} THEN Out.String("Fail 2"); Out.Ln END;
LDGW tSet.s
LDGW tSet.n
INC
CONST 33
BOUND 24
BIT
DEC
JEQ 14
CONST 7
GLOBAL tSet.%2
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
LABEL 14
!   n := 5;
CONST 5
STGW tSet.n
!   s := {1, 2, n..n+2, 10..12, 13..n+7};	(* 1 2 5 6 7 10 11 12 *)
CONST 6
LDGW tSet.n
CONST 3
PLUS
CONST 33
BOUND 27
BIT
DEC
LDGW tSet.n
CONST 33
BOUND 27
BIT
DEC
BITNOT
BITAND
BITOR
CONST 7168
BITOR
LDGW tSet.n
CONST 8
PLUS
CONST 33
BOUND 27
BIT
DEC
CONST -8192
BITAND
BITOR
STGW tSet.s
!   Print;
GLOBAL tSet.Print
CALL 0
!   s := s * {3..15} + {14..n+12};	(* 5 6 7 10 11 12 14 15 16 17 *)
LDGW tSet.s
CONST 65528
BITAND
LDGW tSet.n
CONST 13
PLUS
CONST 33
BOUND 29
BIT
DEC
CONST -16384
BITAND
BITOR
STGW tSet.s
!   Print;
GLOBAL tSet.Print
CALL 0
!   INCL(s, 4); EXCL(s, 16);		(* 4 5 6 7 10 11 12 14 15 17 *)
GLOBAL tSet.s
DUP 0
LOADW
CONST 16
BITOR
SWAP
STOREW
GLOBAL tSet.s
DUP 0
LOADW
CONST -65537
BITAND
SWAP
STOREW
!   Print;
GLOBAL tSet.Print
CALL 0
!   s := s / {12..14};			(* 4 5 6 7 10 11 13 15 17 *)
LDGW tSet.s
CONST 28672
BITXOR
STGW tSet.s
!   Print;
GLOBAL tSet.Print
CALL 0
!   s := (- s) * ({19..MAX(SET)}/(- {})); (* 0 1 2 3 8 9 12 14 16 18 *)
LDGW tSet.s
BITNOT
CONST 524287
BITAND
STGW tSet.s
!   Print;
GLOBAL tSet.Print
CALL 0
!   s := s - {1..2};			(* 0 3 8 9 12 14 16 18 *)
LDGW tSet.s
CONST -7
BITAND
STGW tSet.s
!   Print
GLOBAL tSet.Print
CALL 0
RETURN
END

! Global variables
GLOVAR tSet.s 4
GLOVAR tSet.n 4

! String "Fail 1"
DEFINE tSet.%1
STRING 4661696C203100

! String "Fail 2"
DEFINE tSet.%2
STRING 4661696C203200

! End of file
]]*)
