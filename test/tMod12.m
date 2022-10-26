(* Modulo-12 counter implemented with JK flip-flops for Jack *)

MODULE tMod12;

IMPORT Out;

VAR q: ARRAY 4 OF BOOLEAN;

PROCEDURE Step;
  VAR i: INTEGER; r: BOOLEAN; j, k: ARRAY 4 OF BOOLEAN;
BEGIN
  r := q[2] & q[3];
  j[0] := ~r;                    k[0] := TRUE;
  j[1] := q[0] & ~r;             k[1] := q[0] OR r;
  j[2] := q[0] & q[1] & ~q[3];   k[2] := q[0] & q[1] OR r;
  j[3] := q[0] & q[1] & q[2];    k[3] := k[2];

  FOR i := 0 TO 3 DO
    q[i] := j[i] & ~q[i] OR ~k[i] & q[i]
  END
END Step;

VAR i, t: INTEGER;

BEGIN
  q[0] := TRUE; q[1] := FALSE; q[2] := TRUE; q[3] := TRUE;

  t := 0;
  LOOP
    FOR i := 3 TO 0 BY -1 DO Out.Int(ORD(q[i]), 0) END;
    Out.Ln;
    IF t = 15 THEN EXIT END;
    Step; INC(t)
  END
END tMod12.

(*<<
1101
0000
0001
0010
0011
0100
0101
0110
0111
1000
1001
1010
1011
0000
0001
0010
>>*)

(*[[
!! (SYMFILE #tMod12 STAMP #tMod12.%main 3 #tMod12.m)
!! (CHKSUM STAMP)
!! 
MODULE tMod12 STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tMod12.Step 16 4 0
! PROCEDURE Step;
!   r := q[2] & q[3];
GLOBAL tMod12.q
LDNC 2
JNEQZ L2
CONST 0
JUMP L3
LABEL L2
GLOBAL tMod12.q
LDNC 3
LABEL L3
STLC -5
!   j[0] := ~r;                    k[0] := TRUE;
LDLC -5
NOT
STLC -9
CONST 1
STLC -13
!   j[1] := q[0] & ~r;             k[1] := q[0] OR r;
LDGC tMod12.q
LDLC -5
NOT
AND
STLC -8
LDGC tMod12.q
LDLC -5
OR
STLC -12
!   j[2] := q[0] & q[1] & ~q[3];   k[2] := q[0] & q[1] OR r;
LDGC tMod12.q
JEQZ L4
GLOBAL tMod12.q
LDNC 1
JNEQZ L5
LABEL L4
CONST 0
JUMP L6
LABEL L5
GLOBAL tMod12.q
LDNC 3
NOT
LABEL L6
STLC -7
LDGC tMod12.q
JEQZ L9
GLOBAL tMod12.q
LDNC 1
JEQZ L9
CONST 1
JUMP L10
LABEL L9
LDLC -5
LABEL L10
STLC -11
!   j[3] := q[0] & q[1] & q[2];    k[3] := k[2];
LDGC tMod12.q
JEQZ L12
GLOBAL tMod12.q
LDNC 1
JNEQZ L13
LABEL L12
CONST 0
JUMP L14
LABEL L13
GLOBAL tMod12.q
LDNC 2
LABEL L14
STLC -6
LDLC -11
STLC -10
!   FOR i := 0 TO 3 DO
CONST 0
STLW -4
LABEL L16
LDLW -4
CONST 3
JGT L17
!     q[i] := j[i] & ~q[i] OR ~k[i] & q[i]
LOCAL -9
LDLW -4
CONST 4
BOUND 19
LDIC
JEQZ L21
GLOBAL tMod12.q
LDLW -4
CONST 4
BOUND 19
LDIC
JEQZ L18
LABEL L21
LOCAL -13
LDLW -4
CONST 4
BOUND 19
LDIC
JNEQZ L19
GLOBAL tMod12.q
LDLW -4
CONST 4
BOUND 19
LDIC
JEQZ L19
LABEL L18
CONST 1
JUMP L20
LABEL L19
CONST 0
LABEL L20
GLOBAL tMod12.q
LDLW -4
CONST 4
BOUND 19
STIC
!   FOR i := 0 TO 3 DO
INCL -4
JUMP L16
LABEL L17
RETURN
END

PROC tMod12.%main 0 4 0
!   q[0] := TRUE; q[1] := FALSE; q[2] := TRUE; q[3] := TRUE;
CONST 1
STGC tMod12.q
CONST 0
GLOBAL tMod12.q
STNC 1
CONST 1
GLOBAL tMod12.q
STNC 2
CONST 1
GLOBAL tMod12.q
STNC 3
!   t := 0;
CONST 0
STGW tMod12.t
LABEL L24
!     FOR i := 3 TO 0 BY -1 DO Out.Int(ORD(q[i]), 0) END;
CONST 3
STGW tMod12.i
LABEL L26
LDGW tMod12.i
JLTZ L27
CONST 0
GLOBAL tMod12.q
LDGW tMod12.i
CONST 4
BOUND 30
LDIC
GLOBAL Out.Int
CALL 2
LDGW tMod12.i
DEC
STGW tMod12.i
JUMP L26
LABEL L27
!     Out.Ln;
GLOBAL Out.Ln
CALL 0
!     IF t = 15 THEN EXIT END;
LDGW tMod12.t
CONST 15
JEQ L25
!     Step; INC(t)
GLOBAL tMod12.Step
CALL 0
LDGW tMod12.t
INC
STGW tMod12.t
JUMP L24
LABEL L25
RETURN
END

! Global variables
GLOVAR tMod12.q 4
GLOVAR tMod12.i 4
GLOVAR tMod12.t 4

! End of file
]]*)
