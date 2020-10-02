MODULE tQSet;

(*<<
1 5 8 6 3 7 2 4
>>*)

IMPORT Out;

CONST N = 8;

TYPE board = ARRAY N OF INTEGER;

VAR done: BOOLEAN;

PROCEDURE queens(k: INTEGER; VAR choice: board; rows, diagdown, diagup: SET);
  VAR y: INTEGER;
(*
  VAR z: INTEGER;
*)
BEGIN
(*
  Out.String("queens("); Out.Int(k, 0); Out.String(")");
  FOR z := 0 TO 31 DO
    IF z IN diagup THEN Out.String(" "); Out.Int(z, 0) END
  END;
  Out.Ln;
*)

  IF k = N THEN
    print(choice); done := TRUE
  ELSE
    y := 0;
    WHILE (y < N) & ~done DO
      IF (y IN rows) & (k+y IN diagdown) & (y-k+7 IN diagup) THEN 
	choice[k] := y;
	queens(k+1, choice, rows - {y}, diagdown - {k+y}, diagup - {y-k+7})
      END;
      y := y+1
    END
  END
END queens;

PROCEDURE print(VAR choice: board);
  VAR x: INTEGER;
BEGIN
  x := 0;
  WHILE x < N DO
    IF x > 0 THEN Out.Char(' ') END;
    Out.Int(choice[x]+1, 0);
    x := x+1
  END;
  Out.Ln()
END print;

VAR choice: board;

BEGIN
  queens(0, choice, {0..7}, {0..14}, {0..14})
END tQSet.

(*[[
!! (SYMFILE #tQSet STAMP #tQSet.%main 1 #tQSet.m)
!! (CHKSUM STAMP)
!! 
MODULE tQSet STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tQSet.queens 4 6 0x00200001
! PROCEDURE queens(k: INTEGER; VAR choice: board; rows, diagdown, diagup: SET);
!   IF k = N THEN
LDLW 12
CONST 8
JNEQ L12
!     print(choice); done := TRUE
LDLW 16
GLOBAL tQSet.print
CALL 1
CONST 1
STGC tQSet.done
RETURN
LABEL L12
!     y := 0;
CONST 0
STLW -4
LABEL L2
!     WHILE (y < N) & ~done DO
LDLW -4
CONST 8
JGEQ L4
LDGC tQSet.done
JNEQZ L4
!       IF (y IN rows) & (k+y IN diagdown) & (y-k+7 IN diagup) THEN 
LDLW 20
CONST 1
LDLW -4
CONST 32
BOUND 34
LSL
BITAND
JEQZ L7
LDLW 24
CONST 1
LDLW 12
LDLW -4
PLUS
CONST 32
BOUND 34
LSL
BITAND
JEQZ L7
LDLW 28
CONST 1
LDLW -4
LDLW 12
MINUS
CONST 7
PLUS
CONST 32
BOUND 34
LSL
BITAND
JEQZ L7
! 	choice[k] := y;
LDLW -4
LDLW 16
LDLW 12
CONST 8
BOUND 35
STIW
! 	queens(k+1, choice, rows - {y}, diagdown - {k+y}, diagup - {y-k+7})
LDLW 28
CONST 1
LDLW -4
LDLW 12
MINUS
CONST 7
PLUS
CONST 32
BOUND 36
LSL
BITNOT
BITAND
LDLW 24
CONST 1
LDLW 12
LDLW -4
PLUS
CONST 32
BOUND 36
LSL
BITNOT
BITAND
LDLW 20
CONST 1
LDLW -4
CONST 32
BOUND 36
LSL
BITNOT
BITAND
LDLW 16
LDLW 12
INC
GLOBAL tQSet.queens
CALL 5
LABEL L7
!       y := y+1
INCL -4
JUMP L2
LABEL L4
RETURN
END

PROC tQSet.print 4 4 0x00100001
! PROCEDURE print(VAR choice: board);
!   x := 0;
CONST 0
STLW -4
LABEL L13
!   WHILE x < N DO
LDLW -4
CONST 8
JGEQ L15
!     IF x > 0 THEN Out.Char(' ') END;
LDLW -4
JLEQZ L18
CONST 32
ALIGNC
GLOBAL Out.Char
CALL 1
LABEL L18
!     Out.Int(choice[x]+1, 0);
CONST 0
LDLW 12
LDLW -4
CONST 8
BOUND 49
LDIW
INC
GLOBAL Out.Int
CALL 2
!     x := x+1
INCL -4
JUMP L13
LABEL L15
!   Out.Ln()
GLOBAL Out.Ln
CALL 0
RETURN
END

PROC tQSet.%main 0 6 0
!   queens(0, choice, {0..7}, {0..14}, {0..14})
CONST 32767
CONST 32767
CONST 255
GLOBAL tQSet.choice
CONST 0
GLOBAL tQSet.queens
CALL 5
RETURN
END

! Global variables
GLOVAR tQSet.done 1
GLOVAR tQSet.choice 32

! End of file
]]*)
