MODULE tSet07;

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
  n := 31;
  s := {0..30, 31}; 
  IF s # {0 .. n} THEN Out.String("Fail 2"); Out.Ln END;

  n := 5;
  s := {1, 2, n..n+2, 10..12, 13..n+7};	(* 1 2 5 6 7 10 11 12 *)
  Print;
  s := s * {3..15} + {14..n+12};	(* 5 6 7 10 11 12 14 15 16 17 *)
  Print;
  INCL(s, 4); EXCL(s, 16);		(* 4 5 6 7 10 11 12 14 15 17 *)
  Print;
  s := s / {12..14};			(* 4 5 6 7 10 11 13 15 17 *)
  Print;
  s := (- s) * ({19..31}/(- {})); (* 0 1 2 3 8 9 12 14 16 18 *)
  Print;
  s := s - {1..2};			(* 0 3 8 9 12 14 16 18 *)
  Print
END tSet07.

(*<<
  1  2  5  6  7 10 11 12
  5  6  7 10 11 12 14 15 16 17
  4  5  6  7 10 11 12 14 15 17
  4  5  6  7 10 11 13 15 17
  0  1  2  3  8  9 12 14 16 18
  0  3  8  9 12 14 16 18
>>*)

(*[[
!! (SYMFILE #tSet07 STAMP #tSet07.%main 1 #tSet07.m)
!! (CHKSUM STAMP)
!! 
MODULE tSet07 STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tSet07.Print 8 4 0
! PROCEDURE Print;
!   FOR i := 0 TO 15 DO
CONST 0
STLW -4
LABEL L2
LDLW -4
CONST 15
JGT L3
!     IF i IN s THEN Out.Int(i, 3) END
LDGW tSet07.s
CONST 1
LDLW -4
CONST 32
BOUND 11
LSL
BITAND
JEQZ L6
CONST 3
LDLW -4
GLOBAL Out.Int
CALL 2
LABEL L6
!   FOR i := 0 TO 15 DO
INCL -4
JUMP L2
LABEL L3
!   FOR i := 16 TO 31 DO
CONST 16
STLW -4
LABEL L7
LDLW -4
CONST 31
JGT L8
!     b := (i IN s);
LDGW tSet07.s
CONST 1
LDLW -4
CONST 32
BOUND 14
LSL
BITAND
CONST 0
NEQ
STLC -5
!     IF b THEN Out.Int(i, 3) END
LDLC -5
JEQZ L11
CONST 3
LDLW -4
GLOBAL Out.Int
CALL 2
LABEL L11
!   FOR i := 16 TO 31 DO
INCL -4
JUMP L7
LABEL L8
!   Out.Ln
GLOBAL Out.Ln
CALL 0
RETURN
END

PROC tSet07.%main 0 5 0
!   n := 31;
CONST 31
STGW tSet07.n
!   s := {0..30, 31}; 
CONST -1
STGW tSet07.s
!   IF s # {0 .. n} THEN Out.String("Fail 2"); Out.Ln END;
LDGW tSet07.s
CONST -2
LDGW tSet07.n
CONST 32
BOUND 23
LSL
BITNOT
JEQ L14
CONST 7
GLOBAL tSet07.%1
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
LABEL L14
!   n := 5;
CONST 5
STGW tSet07.n
!   s := {1, 2, n..n+2, 10..12, 13..n+7};	(* 1 2 5 6 7 10 11 12 *)
CONST -1
LDGW tSet07.n
CONST 32
BOUND 26
LSL
CONST -2
LDGW tSet07.n
CONST 2
PLUS
CONST 32
BOUND 26
LSL
BITNOT
BITAND
CONST 6
BITOR
CONST 7168
BITOR
CONST -2
LDGW tSet07.n
CONST 7
PLUS
CONST 32
BOUND 26
LSL
BITNOT
CONST -8192
BITAND
BITOR
STGW tSet07.s
!   Print;
GLOBAL tSet07.Print
CALL 0
!   s := s * {3..15} + {14..n+12};	(* 5 6 7 10 11 12 14 15 16 17 *)
LDGW tSet07.s
CONST 65528
BITAND
CONST -2
LDGW tSet07.n
CONST 12
PLUS
CONST 32
BOUND 28
LSL
BITNOT
CONST -16384
BITAND
BITOR
STGW tSet07.s
!   Print;
GLOBAL tSet07.Print
CALL 0
!   INCL(s, 4); EXCL(s, 16);		(* 4 5 6 7 10 11 12 14 15 17 *)
GLOBAL tSet07.s
DUP 0
LOADW
CONST 16
BITOR
SWAP
STOREW
GLOBAL tSet07.s
DUP 0
LOADW
CONST -65537
BITAND
SWAP
STOREW
!   Print;
GLOBAL tSet07.Print
CALL 0
!   s := s / {12..14};			(* 4 5 6 7 10 11 13 15 17 *)
LDGW tSet07.s
CONST 28672
BITXOR
STGW tSet07.s
!   Print;
GLOBAL tSet07.Print
CALL 0
!   s := (- s) * ({19..31}/(- {})); (* 0 1 2 3 8 9 12 14 16 18 *)
LDGW tSet07.s
BITNOT
CONST 524287
BITAND
STGW tSet07.s
!   Print;
GLOBAL tSet07.Print
CALL 0
!   s := s - {1..2};			(* 0 3 8 9 12 14 16 18 *)
LDGW tSet07.s
CONST -7
BITAND
STGW tSet07.s
!   Print
GLOBAL tSet07.Print
CALL 0
RETURN
END

! Global variables
GLOVAR tSet07.s 4
GLOVAR tSet07.n 4

! String "Fail 2"
DEFINE tSet07.%1
STRING 4661696C203200

! End of file
]]*)
