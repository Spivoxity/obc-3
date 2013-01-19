MODULE tChain;

(*<<
[ex][gc[ex][ex]][gc]2415
[gc]2415
[gc]2415
[gc]2415
[gc]2415
[gc][gc[ex]]2415
[gc]2415
[gc]2415
[gc]2415
[gc]2415
2415
>>*)

IMPORT GC, Out;

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
  GC.Debug('gs');
  FOR i := 1 TO 10 DO Test END;
  Out.Int((((((P+1) DIV 2) * (N MOD P)) MOD P) 
			* ((N+1) MOD P)) MOD P, 0); Out.Ln;
  (* GC.Dump *)
END tChain.

(*[[
!! SYMFILE #tChain STAMP #tChain.%main 1
!! END STAMP
!! 
MODULE tChain STAMP 0
IMPORT GC STAMP
IMPORT Out STAMP
ENDHDR

PROC tChain.Cons 4 4 0x00210001
! PROCEDURE Cons(hd: INTEGER; tl: list): list;
!   NEW(p);
CONST 12
GLOBAL tChain.cell
LOCAL -4
GLOBAL NEW
CALL 3
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
RETURNW
END

PROC tChain.Test 12 4 0x00004001
! PROCEDURE Test;
!   xs := NIL;
CONST 0
STLW -12
!   FOR n := N TO 1 BY -1 DO 
CONST 100000
STLW -4
LABEL 3
LDLW -4
CONST 1
JLT 4
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
DECL -4
JUMP 3
LABEL 4
!   s := 0;
CONST 0
STLW -8
LABEL 5
!   WHILE xs # NIL DO
LDLW -12
JEQZ 6
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
JEQ 8
!       Out.String("Fail 1"); Out.Ln; RETURN 
CONST 7
GLOBAL tChain.%1
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
LABEL 8
!     xs := xs.tail1
LDLW -12
NCHECK 52
LDNW 4
STLW -12
JUMP 5
LABEL 6
!   Out.Int(s, 0); Out.Ln;
CONST 0
LDLW -8
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

PROC tChain.%main 0 4 0
!   GC.Debug('gs');
CONST 3
GLOBAL tChain.%2
GLOBAL GC.Debug
CALL 2
!   FOR i := 1 TO 10 DO Test END;
CONST 1
STGW tChain.i
LABEL 9
LDGW tChain.i
CONST 10
JGT 10
GLOBAL tChain.Test
CALL 0
LDGW tChain.i
INC
STGW tChain.i
JUMP 9
LABEL 10
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

! String "gs"
DEFINE tChain.%2
STRING 677300

! Descriptor for cell
DEFINE tChain.cell
WORD 0x0000000d
WORD 0
WORD tChain.cell.%anc

DEFINE tChain.cell.%anc
WORD tChain.cell

! End of file
]]*)
