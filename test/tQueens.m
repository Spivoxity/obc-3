MODULE tQueens;

(*<<
1 5 8 6 3 7 2 4
>>*)

IMPORT Out;

CONST N = 8;

TYPE board = ARRAY N OF SHORTINT;

VAR done: BOOLEAN;

PROCEDURE queens(k: INTEGER; VAR choice: board);
  VAR y: INTEGER; ok: BOOLEAN;
BEGIN
  IF k = N THEN
    print(choice); done := TRUE
  ELSE
    y := 0;
    WHILE (y < N) & ~done DO
      VAR j: INTEGER; BEGIN
	j := 0; ok := TRUE;
	WHILE ok & (j < k) DO
	  VAR q: INTEGER; BEGIN
	    q := choice[j];
	    ok := (q # y) & (q+j # y+k) & (q-j # y-k);
	    j := j+1
	  END
	END
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

BEGIN
  VAR choice: board; BEGIN
    queens(0, choice)
  END
END tQueens.

(*[[
!! SYMFILE #tQueens STAMP #tQueens.%main 1
!! END STAMP
!! 
MODULE tQueens STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tQueens.queens 4 16 0x00200001
! PROCEDURE queens(k: INTEGER; VAR choice: board);
!   IF k = N THEN
LDLW 12
CONST 8
JNEQ 2
!     print(choice); done := TRUE
LDLW 16
CONST tQueens.print
CALL 1
CONST 1
STGC tQueens.done
RETURN
LABEL 2
!     y := 0;
CONST 0
STLW -4
JUMP 4
LABEL 3
! 	j := 0; ok := TRUE;
CONST 0
STLW -12
CONST 1
STLC -5
JUMP 6
LABEL 5
! 	    q := choice[j];
LDLW 16
LDLW -12
CONST 8
BOUND 27
LDIS
STLW -16
! 	    ok := (q # y) & (q+j # y+k) & (q-j # y-k);
LDLW -16
LDLW -4
NEQ
LDLW -16
LDLW -12
PLUS
LDLW -4
LDLW 12
PLUS
NEQ
AND
LDLW -16
LDLW -12
MINUS
LDLW -4
LDLW 12
MINUS
NEQ
AND
STLC -5
! 	    j := j+1
INCL -12
LABEL 6
! 	WHILE ok & (j < k) DO
LDLC -5
JUMPF 7
LDLW -12
LDLW 12
JLT 5
LABEL 7
!       IF ok THEN 
LDLC -5
JUMPF 9
! 	choice[k] := SHORT(y);
LDLW -4
CONVNS
LDLW 16
LDLW 12
CONST 8
BOUND 34
STIS
! 	queens(k+1, choice)
LDLW 16
LDLW 12
INC
CONST tQueens.queens
CALL 2
LABEL 9
!       y := y+1
INCL -4
LABEL 4
!     WHILE (y < N) & ~done DO
LDLW -4
CONST 8
JGEQ 10
LDGC tQueens.done
JUMPF 3
LABEL 10
RETURN
END

PROC tQueens.print 1 16 0x00100001
! PROCEDURE print(VAR choice: board);
!   x := 0;
CONST 0
STLW -4
JUMP 12
LABEL 11
!     IF x > 0 THEN Out.Char(' ') END;
LDLW -4
JLEQZ 14
CONST 32
ALIGNC
CONST Out.Char
CALL 1
LABEL 14
!     Out.Int(choice[x]+1, 0);
CONST 0
LDLW 12
LDLW -4
CONST 8
BOUND 48
LDIS
INC
CONST Out.Int
CALL 2
!     x := x+1
INCL -4
LABEL 12
!   WHILE x < N DO
LDLW -4
CONST 8
JLT 11
!   Out.Ln()
CONST Out.Ln
CALL 0
RETURN
END

PROC tQueens.%main 4 16 0
!     queens(0, choice)
LOCAL -16
CONST 0
CONST tQueens.queens
CALL 2
RETURN
END

! Global variables
GLOBAL tQueens.done 1

! End of file
]]*)
