MODULE tQueens;

(*<<
1 5 8 6 3 7 2 4
>>*)

IMPORT Out;

CONST N = 8;

TYPE board = ARRAY N OF SHORTINT;

VAR done: BOOLEAN;

PROCEDURE queens(k: INTEGER; VAR choice: board);
  VAR y, j, q: INTEGER; ok: BOOLEAN;
BEGIN
  IF k = N THEN
    print(choice); done := TRUE
  ELSE
    y := 0;
    WHILE (y < N) & ~done DO
      j := 0; ok := TRUE;
      WHILE ok & (j < k) DO
        q := choice[j];
        ok := (q # y) & (q+j # y+k) & (q-j # y-k);
        j := j+1
      END;
      IF ok THEN 
	choice[k] := SHORT(y);
	queens(k+1, choice)
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
  queens(0, choice)
END tQueens.

(*[[
!! (SYMFILE #tQueens STAMP #tQueens.%main 1 #tQueens.m)
!! (CHKSUM STAMP)
!! 
MODULE tQueens STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tQueens.queens 16 4 0x00200001
! PROCEDURE queens(k: INTEGER; VAR choice: board);
!   IF k = N THEN
LDLW 12
CONST 8
JNEQ L14
!     print(choice); done := TRUE
LDLW 16
GLOBAL tQueens.print
CALL 1
CONST 1
STGC tQueens.done
RETURN
LABEL L14
!     y := 0;
CONST 0
STLW -4
LABEL L2
!     WHILE (y < N) & ~done DO
LDLW -4
CONST 8
JGEQ L4
LDGC tQueens.done
JNEQZ L4
!       j := 0; ok := TRUE;
CONST 0
STLW -8
CONST 1
STLC -13
LABEL L5
!       WHILE ok & (j < k) DO
LDLC -13
JEQZ L7
LDLW -8
LDLW 12
JGEQ L7
!         q := choice[j];
LDLW 16
LDLW -8
CONST 8
BOUND 25
LDIS
STLW -12
!         ok := (q # y) & (q+j # y+k) & (q-j # y-k);
LDLW -12
LDLW -4
NEQ
LDLW -12
LDLW -8
PLUS
LDLW -4
LDLW 12
PLUS
NEQ
AND
LDLW -12
LDLW -8
MINUS
LDLW -4
LDLW 12
MINUS
NEQ
AND
STLC -13
!         j := j+1
INCL -8
JUMP L5
LABEL L7
!       IF ok THEN 
LDLC -13
JEQZ L11
! 	choice[k] := SHORT(y);
LDLW -4
CONVNS
LDLW 16
LDLW 12
CONST 8
BOUND 30
STIS
! 	queens(k+1, choice)
LDLW 16
LDLW 12
INC
GLOBAL tQueens.queens
CALL 2
LABEL L11
!       y := y+1
INCL -4
JUMP L2
LABEL L4
RETURN
END

PROC tQueens.print 4 4 0x00100001
! PROCEDURE print(VAR choice: board);
!   x := 0;
CONST 0
STLW -4
LABEL L15
!   WHILE x < N DO
LDLW -4
CONST 8
JGEQ L17
!     IF x > 0 THEN Out.Char(' ') END;
LDLW -4
JLEQZ L20
CONST 32
ALIGNC
GLOBAL Out.Char
CALL 1
LABEL L20
!     Out.Int(choice[x]+1, 0);
CONST 0
LDLW 12
LDLW -4
CONST 8
BOUND 44
LDIS
INC
GLOBAL Out.Int
CALL 2
!     x := x+1
INCL -4
JUMP L15
LABEL L17
!   Out.Ln()
GLOBAL Out.Ln
CALL 0
RETURN
END

PROC tQueens.%main 0 3 0
!   queens(0, choice)
GLOBAL tQueens.choice
CONST 0
GLOBAL tQueens.queens
CALL 2
RETURN
END

! Global variables
GLOVAR tQueens.done 1
GLOVAR tQueens.choice 16

! End of file
]]*)
