MODULE tEqual;

(* Check machine-code implementation of equality *)

IMPORT Out;

PROCEDURE Plot(f: PROCEDURE (i, j: INTEGER): BOOLEAN);
  VAR i, j: INTEGER;
BEGIN
  FOR i := -4 TO 4 DO
    FOR j := -4 TO 4 DO
      IF f(i, j) THEN Out.String(" T") ELSE Out.String(" F") END
    END;
    Out.Ln
  END
END Plot;

PROCEDURE Equal(i, j: INTEGER): BOOLEAN; BEGIN RETURN i = j END Equal;

PROCEDURE Unequal(i, j: INTEGER): BOOLEAN; BEGIN RETURN i # j END Unequal;

BEGIN
  Plot(Equal);
  Out.Ln;
  Plot(Unequal)
END tEqual.

(*<<
 T F F F F F F F F
 F T F F F F F F F
 F F T F F F F F F
 F F F T F F F F F
 F F F F T F F F F
 F F F F F T F F F
 F F F F F F T F F
 F F F F F F F T F
 F F F F F F F F T

 F T T T T T T T T
 T F T T T T T T T
 T T F T T T T T T
 T T T F T T T T T
 T T T T F T T T T
 T T T T T F T T T
 T T T T T T F T T
 T T T T T T T F T
 T T T T T T T T F
>>*)

(*[[
!! (SYMFILE #tEqual STAMP #tEqual.%main 1 #tEqual.m)
!! (CHKSUM STAMP)
!! 
MODULE tEqual STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tEqual.Plot 8 3 0
! PROCEDURE Plot(f: PROCEDURE (i, j: INTEGER): BOOLEAN);
!   FOR i := -4 TO 4 DO
CONST -4
STLW -4
LABEL L3
LDLW -4
CONST 4
JGT L4
!     FOR j := -4 TO 4 DO
CONST -4
STLW -8
LABEL L5
LDLW -8
CONST 4
JGT L6
!       IF f(i, j) THEN Out.String(" T") ELSE Out.String(" F") END
LDLW -8
LDLW -4
LDLW 16
STATLINK
LDLW 12
NCHECK 12
CALLW 2
JEQZ L9
CONST 3
GLOBAL tEqual.%1
GLOBAL Out.String
CALL 2
JUMP L7
LABEL L9
CONST 3
GLOBAL tEqual.%2
GLOBAL Out.String
CALL 2
LABEL L7
!     FOR j := -4 TO 4 DO
INCL -8
JUMP L5
LABEL L6
!     Out.Ln
GLOBAL Out.Ln
CALL 0
!   FOR i := -4 TO 4 DO
INCL -4
JUMP L3
LABEL L4
RETURN
END

PROC tEqual.Equal 0 2 0
! PROCEDURE Equal(i, j: INTEGER): BOOLEAN; BEGIN RETURN i = j END Equal;
LDLW 12
LDLW 16
EQ
RETURN
END

PROC tEqual.Unequal 0 2 0
! PROCEDURE Unequal(i, j: INTEGER): BOOLEAN; BEGIN RETURN i # j END Unequal;
LDLW 12
LDLW 16
NEQ
RETURN
END

PROC tEqual.%main 0 3 0
!   Plot(Equal);
CONST 0
GLOBAL tEqual.Equal
GLOBAL tEqual.Plot
CALL 2
!   Out.Ln;
GLOBAL Out.Ln
CALL 0
!   Plot(Unequal)
CONST 0
GLOBAL tEqual.Unequal
GLOBAL tEqual.Plot
CALL 2
RETURN
END

! String " T"
DEFINE tEqual.%1
STRING 205400

! String " F"
DEFINE tEqual.%2
STRING 204600

! End of file
]]*)
