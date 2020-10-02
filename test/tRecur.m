MODULE tRecur;

IMPORT Out;

PROCEDURE Recip(N: INTEGER);
  VAR 
    i, j, k, r: INTEGER;
    d, p: ARRAY 1000 OF INTEGER;
BEGIN
  FOR i := 0 TO N-1 DO p[i] := -1 END;

  r := 1; k := 0;
  WHILE p[r] < 0 DO
    p[r] := k; r := 10 * r;
    d[k] := r DIV N; r := r MOD N;
    k := k+1
  END;
  j := p[r];

  Out.String("1/"); Out.Int(N, 0); Out.String(" = 0.");
  FOR i := 0 TO j-1 DO Out.Int(d[i], 0) END;
  Out.String("|");
  FOR i := j TO k-1 DO Out.Int(d[i], 0) END;
  Out.String("|"); Out.Ln
END Recip;

BEGIN
  Recip(2);
  Recip(3);
  Recip(12);
  Recip(14);
  Recip(33)
END tRecur.

(*<<
1/2 = 0.5|0|
1/3 = 0.|3|
1/12 = 0.08|3|
1/14 = 0.0|714285|
1/33 = 0.|03|
>>*)

(*[[
!! (SYMFILE #tRecur STAMP #tRecur.%main 1 #tRecur.m)
!! (CHKSUM STAMP)
!! 
MODULE tRecur STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tRecur.Recip 8028 4 0
! PROCEDURE Recip(N: INTEGER);
!   FOR i := 0 TO N-1 DO p[i] := -1 END;
LDLW 12
DEC
STLW -8020
CONST 0
STLW -4
LABEL L4
LDLW -4
LDLW -8020
JGT L5
CONST -1
LOCAL -8016
LDLW -4
CONST 1000
BOUND 10
STIW
INCL -4
JUMP L4
LABEL L5
!   r := 1; k := 0;
CONST 1
STLW -16
CONST 0
STLW -12
LABEL L6
!   WHILE p[r] < 0 DO
LOCAL -8016
LDLW -16
CONST 1000
BOUND 13
LDIW
JGEQZ L8
!     p[r] := k; r := 10 * r;
LDLW -12
LOCAL -8016
LDLW -16
CONST 1000
BOUND 14
STIW
LDLW -16
CONST 10
TIMES
STLW -16
!     d[k] := r DIV N; r := r MOD N;
LDLW -16
LDLW 12
ZCHECK 15
DIV
LOCAL -4016
LDLW -12
CONST 1000
BOUND 15
STIW
LDLW -16
LDLW 12
ZCHECK 15
MOD
STLW -16
!     k := k+1
INCL -12
JUMP L6
LABEL L8
!   j := p[r];
LOCAL -8016
LDLW -16
CONST 1000
BOUND 18
LDIW
STLW -8
!   Out.String("1/"); Out.Int(N, 0); Out.String(" = 0.");
CONST 3
GLOBAL tRecur.%1
GLOBAL Out.String
CALL 2
CONST 0
LDLW 12
GLOBAL Out.Int
CALL 2
CONST 6
GLOBAL tRecur.%2
GLOBAL Out.String
CALL 2
!   FOR i := 0 TO j-1 DO Out.Int(d[i], 0) END;
LDLW -8
DEC
STLW -8024
CONST 0
STLW -4
LABEL L9
LDLW -4
LDLW -8024
JGT L10
CONST 0
LOCAL -4016
LDLW -4
CONST 1000
BOUND 21
LDIW
GLOBAL Out.Int
CALL 2
INCL -4
JUMP L9
LABEL L10
!   Out.String("|");
CONST 2
GLOBAL tRecur.%3
GLOBAL Out.String
CALL 2
!   FOR i := j TO k-1 DO Out.Int(d[i], 0) END;
LDLW -12
DEC
STLW -8028
LDLW -8
STLW -4
LABEL L11
LDLW -4
LDLW -8028
JGT L12
CONST 0
LOCAL -4016
LDLW -4
CONST 1000
BOUND 23
LDIW
GLOBAL Out.Int
CALL 2
INCL -4
JUMP L11
LABEL L12
!   Out.String("|"); Out.Ln
CONST 2
GLOBAL tRecur.%3
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

PROC tRecur.%main 0 2 0
!   Recip(2);
CONST 2
GLOBAL tRecur.Recip
CALL 1
!   Recip(3);
CONST 3
GLOBAL tRecur.Recip
CALL 1
!   Recip(12);
CONST 12
GLOBAL tRecur.Recip
CALL 1
!   Recip(14);
CONST 14
GLOBAL tRecur.Recip
CALL 1
!   Recip(33)
CONST 33
GLOBAL tRecur.Recip
CALL 1
RETURN
END

! String "1/"
DEFINE tRecur.%1
STRING 312F00

! String " = 0."
DEFINE tRecur.%2
STRING 203D20302E00

! String "|"
DEFINE tRecur.%3
STRING 7C00

! End of file
]]*)
