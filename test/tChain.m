MODULE tChain;

(*<<
2415
2415
2415
2415
2415
2415
2415
2415
2415
2415
2415
>>*)

IMPORT Out;

CONST N = 100000; P = 10007;

TYPE list = POINTER TO cell;
  cell = RECORD head: INTEGER; tail1, tail2: list END;

(* The marker can mark a linear list in constant space, but the double
links here cause stack overflow which must be handled gracefully. *)

PROCEDURE Cons(hd: INTEGER; tl: list): list;
  VAR p: list;
BEGIN
  NEW(p);
  p.head := hd;
  p.tail1 := tl;
  p.tail2 := tl;
  RETURN p
END Cons;

PROCEDURE Test;
  VAR n, s: INTEGER; xs: list;
BEGIN
  xs := NIL;
  FOR n := N TO 1 BY -1 DO 
    xs := Cons(3, xs);		    (* Create some garbage *)
    xs := Cons(n, xs.tail1) 
  END;

  s := 0;
  WHILE xs # NIL DO
    s := (s + xs.head) MOD P;
    IF xs.tail1 # xs.tail2 THEN 
      Out.String("Fail 1"); Out.Ln; RETURN 
    END;
    xs := xs.tail1
  END;

  Out.Int(s, 0); Out.Ln;
END Test;

VAR i: INTEGER;

BEGIN
  FOR i := 1 TO 10 DO Test END;
  Out.Int((((((P+1) DIV 2) * (N MOD P)) MOD P) 
			* ((N+1) MOD P)) MOD P, 0); Out.Ln;
END tChain.

(*[[
!! (SYMFILE #tChain STAMP #tChain.%main 1 #tChain.m)
!! (CHKSUM STAMP)
!! 
MODULE tChain STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tChain.Cons 4 3 0x00210001
! PROCEDURE Cons(hd: INTEGER; tl: list): list;
!   NEW(p);
CONST 12
GLOBAL tChain.cell
GLOBAL NEW
CALLW 2
STLW -4
!   p.head := hd;
LDLW 12
LDLW -4
NCHECK 31
STOREW
!   p.tail1 := tl;
LDLW 16
LDLW -4
NCHECK 32
STNW 4
!   p.tail2 := tl;
LDLW 16
LDLW -4
NCHECK 33
STNW 8
!   RETURN p
LDLW -4
RETURN
END

PROC tChain.Test 12 3 0x00004001
! PROCEDURE Test;
!   xs := NIL;
CONST 0
STLW -12
!   FOR n := N TO 1 BY -1 DO 
CONST 100000
STLW -4
LABEL L2
LDLW -4
CONST 1
JLT L3
!     xs := Cons(3, xs);		    (* Create some garbage *)
LDLW -12
CONST 3
GLOBAL tChain.Cons
CALLW 2
STLW -12
!     xs := Cons(n, xs.tail1) 
LDLW -12
NCHECK 43
LDNW 4
LDLW -4
GLOBAL tChain.Cons
CALLW 2
STLW -12
!   FOR n := N TO 1 BY -1 DO 
DECL -4
JUMP L2
LABEL L3
!   s := 0;
CONST 0
STLW -8
LABEL L4
!   WHILE xs # NIL DO
LDLW -12
JEQZ L6
!     s := (s + xs.head) MOD P;
LDLW -8
LDLW -12
NCHECK 48
LOADW
PLUS
CONST 10007
MOD
STLW -8
!     IF xs.tail1 # xs.tail2 THEN 
LDLW -12
NCHECK 49
LDNW 4
LDLW -12
NCHECK 49
LDNW 8
JEQ L9
!       Out.String("Fail 1"); Out.Ln; RETURN 
CONST 7
GLOBAL tChain.%1
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
LABEL L9
!     xs := xs.tail1
LDLW -12
NCHECK 52
LDNW 4
STLW -12
JUMP L4
LABEL L6
!   Out.Int(s, 0); Out.Ln;
CONST 0
LDLW -8
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

PROC tChain.%main 0 3 0
!   FOR i := 1 TO 10 DO Test END;
CONST 1
STGW tChain.i
LABEL L10
LDGW tChain.i
CONST 10
JGT L11
GLOBAL tChain.Test
CALL 0
LDGW tChain.i
INC
STGW tChain.i
JUMP L10
LABEL L11
!   Out.Int((((((P+1) DIV 2) * (N MOD P)) MOD P) 
CONST 0
CONST 2415
GLOBAL Out.Int
CALL 2
! 			* ((N+1) MOD P)) MOD P, 0); Out.Ln;
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tChain.i 4

! String "Fail 1"
DEFINE tChain.%1
STRING 4661696C203100

! Descriptor for cell
DEFINE tChain.cell
WORD 0x0000000d
WORD 0
WORD tChain.cell.%anc

DEFINE tChain.cell.%anc
WORD tChain.cell

! End of file
]]*)
