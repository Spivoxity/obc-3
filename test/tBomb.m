MODULE tBomb;

(*<<
Runtime error: null pointer error on line 23 in module tBomb
In procedure tBomb.p
   called from tBomb.q2
   called from tBomb.q1
   called from tBomb.q3
   called from tBomb.q2
   called from tBomb.q1
   ... 93 intervening frames omitted ...
   called from tBomb.q3
   called from tBomb.q2
   called from tBomb.q1
   called from tBomb.%main
   called from MAIN
>>*)

PROCEDURE p;
  VAR z: POINTER TO RECORD a: INTEGER END;
BEGIN
  z := NIL;
  z.a := 3
END p;

PROCEDURE q1(n: INTEGER);
BEGIN 
  IF n = 0 THEN p ELSE q2(n-1) END
END q1;

PROCEDURE q2(n: INTEGER);
BEGIN 
  IF n = 0 THEN p ELSE q3(n-1) END
END q2;

PROCEDURE q3(n: INTEGER);
BEGIN 
  IF n = 0 THEN p ELSE q1(n-1) END
END q3;

BEGIN
  q1(100)
END tBomb.
  

(*[[
!! (SYMFILE #tBomb STAMP #tBomb.%main 1 #tBomb.m)
!! (CHKSUM STAMP)
!! 
MODULE tBomb STAMP 0
ENDHDR

PROC tBomb.p 4 3 0x00010001
! PROCEDURE p;
!   z := NIL;
CONST 0
STLW -4
!   z.a := 3
CONST 3
LDLW -4
NCHECK 23
STOREW
RETURN
END

PROC tBomb.q1 0 2 0
! PROCEDURE q1(n: INTEGER);
!   IF n = 0 THEN p ELSE q2(n-1) END
LDLW 12
JNEQZ L4
GLOBAL tBomb.p
CALL 0
RETURN
LABEL L4
LDLW 12
DEC
GLOBAL tBomb.q2
CALL 1
RETURN
END

PROC tBomb.q2 0 2 0
! PROCEDURE q2(n: INTEGER);
!   IF n = 0 THEN p ELSE q3(n-1) END
LDLW 12
JNEQZ L7
GLOBAL tBomb.p
CALL 0
RETURN
LABEL L7
LDLW 12
DEC
GLOBAL tBomb.q3
CALL 1
RETURN
END

PROC tBomb.q3 0 2 0
! PROCEDURE q3(n: INTEGER);
!   IF n = 0 THEN p ELSE q1(n-1) END
LDLW 12
JNEQZ L10
GLOBAL tBomb.p
CALL 0
RETURN
LABEL L10
LDLW 12
DEC
GLOBAL tBomb.q1
CALL 1
RETURN
END

PROC tBomb.%main 0 2 0
!   q1(100)
CONST 100
GLOBAL tBomb.q1
CALL 1
RETURN
END

! Descriptor for *anon*
DEFINE tBomb.%1
WORD 0
WORD 0
WORD tBomb.%1.%anc

DEFINE tBomb.%1.%anc
WORD tBomb.%1

! End of file
]]*)
