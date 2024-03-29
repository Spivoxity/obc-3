MODULE tNestFunc;

IMPORT Out;

PROCEDURE G(x: INTEGER); 
BEGIN 
  Out.Int(x, 0); Out.Ln 
END G;

PROCEDURE P;
  VAR y: INTEGER;

  PROCEDURE Q(F: PROCEDURE (x: INTEGER)); 
  BEGIN 
    F(y) 
  END Q;

BEGIN 
  y := 3; 
  Q(G) 
END P;

PROCEDURE R(F: PROCEDURE (x: INTEGER));
  PROCEDURE S;
  BEGIN
    F(8)
  END S;
BEGIN
  S
END R;

PROCEDURE U;
  VAR y: INTEGER;

  PROCEDURE V(x: INTEGER);
  BEGIN
    y := x
  END V;
BEGIN
  y := 0;
  R(V);
  Out.Int(y, 0); Out.Ln;
END U;

BEGIN 
  P;
  U
END tNestFunc.

(*<<
3
8
>>*)

(*[[
!! (SYMFILE #tNestFunc STAMP #tNestFunc.%main 1 #tNestFunc.m)
!! (CHKSUM STAMP)
!! 
MODULE tNestFunc STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tNestFunc.G 0 3 0
! PROCEDURE G(x: INTEGER); 
!   Out.Int(x, 0); Out.Ln 
CONST 0
LDLW 12
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

PROC tNestFunc.%1.Q 4 2 0
SAVELINK
!   PROCEDURE Q(F: PROCEDURE (x: INTEGER)); 
!     F(y) 
LDLW -4
LDNW -4
LDLW 16
STATLINK
LDLW 12
NCHECK 15
CALL 1
RETURN
END

PROC tNestFunc.P 4 3 0
! PROCEDURE P;
!   y := 3; 
CONST 3
STLW -4
!   Q(G) 
CONST 0
GLOBAL tNestFunc.G
LOCAL 0
STATLINK
GLOBAL tNestFunc.%1.Q
CALL 2
RETURN
END

PROC tNestFunc.%2.S 4 2 0
SAVELINK
!   PROCEDURE S;
!     F(8)
CONST 8
LDLW -4
LDNW 16
STATLINK
LDLW -4
LDNW 12
NCHECK 26
CALL 1
RETURN
END

PROC tNestFunc.R 0 1 0
! PROCEDURE R(F: PROCEDURE (x: INTEGER));
!   S
LOCAL 0
STATLINK
GLOBAL tNestFunc.%2.S
CALL 0
RETURN
END

PROC tNestFunc.%3.V 4 2 0
SAVELINK
!   PROCEDURE V(x: INTEGER);
!     y := x
LDLW 12
LDLW -4
STNW -4
RETURN
END

PROC tNestFunc.U 4 3 0
! PROCEDURE U;
!   y := 0;
CONST 0
STLW -4
!   R(V);
LOCAL 0
GLOBAL tNestFunc.%3.V
GLOBAL tNestFunc.R
CALL 2
!   Out.Int(y, 0); Out.Ln;
CONST 0
LDLW -4
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

PROC tNestFunc.%main 0 1 0
!   P;
GLOBAL tNestFunc.P
CALL 0
!   U
GLOBAL tNestFunc.U
CALL 0
RETURN
END

! End of file
]]*)
