MODULE tOpen;

(*<<
15
>>*)

IMPORT Out;

PROCEDURE Sum(a: ARRAY OF INTEGER): INTEGER;
  VAR i, s: INTEGER;
BEGIN
  s := 0;
  FOR i := 0 TO LEN(a)-1 DO s := s + a[i] END;
  RETURN s
END Sum;

VAR b: ARRAY 5 OF INTEGER;

BEGIN
  VAR j: INTEGER; BEGIN
    FOR j := 0 TO LEN(b)-1 DO b[j] := j+1 END
  END;
  Out.Int(Sum(b), 0); Out.Ln
END tOpen.

(*[[
!! SYMFILE #tOpen STAMP #tOpen.%main 1
!! END STAMP
!! 
MODULE tOpen STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tOpen.Sum 3 4 0
! PROCEDURE Sum(a: ARRAY OF INTEGER): INTEGER;
LOCAL 12
LDLW 16
CONST 4
TIMES
FLEXCOPY
!   s := 0;
CONST 0
STLW -8
!   FOR i := 0 TO LEN(a)-1 DO s := s + a[i] END;
LDLW 16
DEC
STLW -12
CONST 0
STLW -4
JUMP 2
LABEL 1
LDLW -8
LDLW 12
LDLW -4
LDLW 16
BOUND 13
LDIW
PLUS
STLW -8
INCL -4
LABEL 2
LDLW -4
LDLW -12
JLEQ 1
!   RETURN s
LDLW -8
RETURNW
END

PROC tOpen.%main 1 4 0
!     FOR j := 0 TO LEN(b)-1 DO b[j] := j+1 END
CONST 0
STLW -4
JUMP 4
LABEL 3
LDLW -4
INC
GLOBAL tOpen.b
LDLW -4
CONST 5
BOUND 21
STIW
INCL -4
LABEL 4
LDLW -4
CONST 4
JLEQ 3
!   Out.Int(Sum(b), 0); Out.Ln
CONST 0
CONST 5
GLOBAL tOpen.b
GLOBAL tOpen.Sum
CALLW 2
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tOpen.b 20

! End of file
]]*)