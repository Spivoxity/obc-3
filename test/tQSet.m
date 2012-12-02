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
!! SYMFILE #tQSet STAMP #tQSet.%main 1
!! END STAMP
!! 
MODULE tQSet STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tQSet.queens 1 24 0x00200001
! PROCEDURE queens(k: INTEGER; VAR choice: board; rows, diagdown, diagup: SET);
!   IF k = N THEN
LDLW 12
CONST 8
JNEQ 2
!     print(choice); done := TRUE
LDLW 16
CONST tQSet.print
CALL 1
CONST 1
STGC tQSet.done
RETURN
LABEL 2
!     y := 0;
CONST 0
STLW -4
JUMP 4
LABEL 3
!       IF (y IN rows) & (k+y IN diagdown) & (y-k+7 IN diagup) THEN 
LDLW -4
CONST 32
BOUND 34
BIT
LDLW 20
BITAND
JEQZ 6
LDLW 12
LDLW -4
PLUS
CONST 32
BOUND 34
BIT
LDLW 24
BITAND
JEQZ 6
LDLW -4
LDLW 12
MINUS
CONST 7
PLUS
CONST 32
BOUND 34
BIT
LDLW 28
BITAND
JEQZ 6
! 	choice[k] := y;
LDLW -4
LDLW 16
LDLW 12
CONST 8
BOUND 35
STIW
! 	queens(k+1, choice, rows - {y}, diagdown - {k+y}, diagup - {y-k+7})
LDLW 28
LDLW -4
LDLW 12
MINUS
CONST 7
PLUS
CONST 32
BOUND 36
BIT
BITNOT
BITAND
LDLW 24
LDLW 12
LDLW -4
PLUS
CONST 32
BOUND 36
BIT
BITNOT
BITAND
LDLW 20
LDLW -4
CONST 32
BOUND 36
BIT
BITNOT
BITAND
LDLW 16
LDLW 12
INC
CONST tQSet.queens
CALL 5
LABEL 6
!       y := y+1
INCL -4
LABEL 4
!     WHILE (y < N) & ~done DO
LDLW -4
CONST 8
JGEQ 7
LDGC tQSet.done
JUMPF 3
LABEL 7
RETURN
END

PROC tQSet.print 1 24 0x00100001
! PROCEDURE print(VAR choice: board);
!   x := 0;
CONST 0
STLW -4
JUMP 9
LABEL 8
!     IF x > 0 THEN Out.Char(' ') END;
LDLW -4
JLEQZ 11
CONST 32
ALIGNC
CONST Out.Char
CALL 1
LABEL 11
!     Out.Int(choice[x]+1, 0);
CONST 0
LDLW 12
LDLW -4
CONST 8
BOUND 49
LDIW
INC
CONST Out.Int
CALL 2
!     x := x+1
INCL -4
LABEL 9
!   WHILE x < N DO
LDLW -4
CONST 8
JLT 8
!   Out.Ln()
CONST Out.Ln
CALL 0
RETURN
END

PROC tQSet.%main 0 24 0
!   queens(0, choice, {0..7}, {0..14}, {0..14})
CONST 32767
CONST 32767
CONST 255
CONST tQSet.choice
CONST 0
CONST tQSet.queens
CALL 5
RETURN
END

! Global variables
GLOBAL tQSet.done 1
GLOBAL tQSet.choice 32

! End of file
]]*)
