MODULE tQueens07;

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
	choice[k] := y;
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
END tQueens07.

(*[[
!! SYMFILE #tQueens07 STAMP #tQueens07.%main 1
!! END STAMP
!! 
MODULE tQueens07 STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tQueens07.queens 16 4 0x00200001
! PROCEDURE queens(k: INTEGER; VAR choice: board);
!   IF k = N THEN
LDLW 12
CONST 8
JNEQ 14
!     print(choice); done := TRUE
LDLW 16
GLOBAL tQueens07.print
CALL 1
CONST 1
STGC tQueens07.done
RETURN
LABEL 14
!     y := 0;
CONST 0
STLW -4
LABEL 2
!     WHILE (y < N) & ~done DO
LDLW -4
CONST 8
JGEQ 4
LDGC tQueens07.done
JUMPT 4
! 	j := 0; ok := TRUE;
CONST 0
STLW -12
CONST 1
STLC -5
LABEL 5
! 	WHILE ok & (j < k) DO
LDLC -5
JUMPF 7
LDLW -12
LDLW 12
JGEQ 7
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
JUMP 5
LABEL 7
!       IF ok THEN 
LDLC -5
JUMPF 11
! 	choice[k] := y;
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
GLOBAL tQueens07.queens
CALL 2
LABEL 11
!       y := y+1
INCL -4
JUMP 2
LABEL 4
RETURN
END

PROC tQueens07.print 4 4 0x00100001
! PROCEDURE print(VAR choice: board);
!   x := 0;
CONST 0
STLW -4
LABEL 15
!   WHILE x < N DO
LDLW -4
CONST 8
JGEQ 17
!     IF x > 0 THEN Out.Char(' ') END;
LDLW -4
JLEQZ 20
CONST 32
ALIGNC
GLOBAL Out.Char
CALL 1
LABEL 20
!     Out.Int(choice[x]+1, 0);
CONST 0
LDLW 12
LDLW -4
CONST 8
BOUND 48
LDIS
INC
GLOBAL Out.Int
CALL 2
!     x := x+1
INCL -4
JUMP 15
LABEL 17
!   Out.Ln()
GLOBAL Out.Ln
CALL 0
RETURN
END

PROC tQueens07.%main 16 4 0
!     queens(0, choice)
LOCAL -16
CONST 0
GLOBAL tQueens07.queens
CALL 2
RETURN
END

! Global variables
GLOVAR tQueens07.done 1

! End of file
]]*)
