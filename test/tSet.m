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

PROC tSet.Print 8 3 0
! PROCEDURE Print;
!   FOR i := 0 TO 15 DO
CONST 0
STLW -4
LABEL 2
LDLW -4
CONST 15
JGT 3
!     IF i IN s THEN Out.Int(i, 3) END
CONST 1
LDLW -4
CONST 32
BOUND 11
LSL
LDGW tSet.s
BITAND
JEQZ 5
CONST 3
LDLW -4
GLOBAL Out.Int
CALL 2
LABEL 5
INCL -4
JUMP 2
LABEL 3
!   FOR i := 16 TO 31 DO
CONST 16
STLW -4
LABEL 6
LDLW -4
CONST 31
JGT 7
!     b := (i IN s);
CONST 1
LDLW -4
CONST 32
BOUND 14
LSL
LDGW tSet.s
BITAND
CONST 0
NEQ
STLC -5
!     IF b THEN Out.Int(i, 3) END
LDLC -5
JUMPF 9
CONST 3
LDLW -4
GLOBAL Out.Int
CALL 2
LABEL 9
INCL -4
JUMP 6
LABEL 7
!   Out.Ln
GLOBAL Out.Ln
CALL 0
RETURN
END

PROC tSet.%main 0 5 0
!   n := MAX(SET);
CONST 31
STGW tSet.n
!   s := {0..30, 31}; 
CONST -1
STGW tSet.s
!   IF s # {MIN(SET) .. n} THEN Out.String("Fail 2"); Out.Ln END;
LDGW tSet.s
CONST -1
CONST -2
LDGW tSet.n
CONST 32
BOUND 23
LSL
BITNOT
BITAND
JEQ 11
CONST 7
GLOBAL tSet.%1
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
LABEL 11
!   n := 5;
CONST 5
STGW tSet.n
!   s := {1, 2, n..n+2, 10..12, 13..n+7};	(* 1 2 5 6 7 10 11 12 *)
CONST 6
CONST -1
LDGW tSet.n
CONST 32
BOUND 26
LSL
CONST -2
LDGW tSet.n
CONST 2
PLUS
CONST 32
BOUND 26
LSL
BITNOT
BITAND
BITOR
CONST 7168
BITOR
CONST -8192
CONST -2
LDGW tSet.n
CONST 7
PLUS
CONST 32
BOUND 26
LSL
BITNOT
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
CONST -16384
CONST -2
LDGW tSet.n
CONST 12
PLUS
CONST 32
BOUND 28
LSL
BITNOT
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

! String "Fail 2"
DEFINE tSet.%1
STRING 4661696C203200

! End of file
]]*)
