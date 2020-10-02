MODULE tPower;

IMPORT Out;

TYPE STRING = ARRAY OF CHAR;

PROCEDURE var(n : INTEGER);
BEGIN
  Out.String("x"); IF n # 1 THEN Out.Int(n,0) END
END var;

PROCEDURE assign(a, b, c : INTEGER);
BEGIN
  var(a); Out.String(":="); var(b); Out.String("*"); var(c); Out.String("; ")
END assign;

PROCEDURE power(n : INTEGER);
BEGIN
  IF n # 1 THEN
    IF n MOD 2 = 0 THEN
      power(n DIV 2);
      assign(n, n DIV 2, n DIV 2);
    ELSE
      power(n-1);
      assign(n, n-1, 1);
    END
  END
END power;

TYPE setofset = POINTER TO node;
     node = RECORD set : SET;
                   cost : INTEGER;
                   n, i, j : INTEGER;
		   left, right : setofset;
                   next : setofset
            END;

PROCEDURE dynamic(n : INTEGER);

  VAR cost : INTEGER;
      s : SET;
      i, j : INTEGER;
      ss, si, sj : setofset;

  PROCEDURE add(newset : SET; newcost, i, j : INTEGER; left, right: setofset);
    VAR p : setofset;
  BEGIN
    IF newcost <= cost THEN
      p := ss;
      WHILE (p # NIL) & (p.set # newset) DO p := p.next END;
      IF p = NIL THEN
        NEW(p);
        p.set := newset;
        p.cost := MAX(INTEGER);
        p.next := ss;
        ss := p
      END;
      IF newcost < p.cost THEN
        p.cost := newcost;
        p.i := i;
        p.j := j;
        p.left := left;
        p.right := right;
        s := s + {i+j}
      END;
    END
  END add;

  PROCEDURE print;
    PROCEDURE out(p : setofset);
    BEGIN
      IF (p # NIL) & (p.cost > 0) THEN
        out(p.left);
	out(p.right);
	assign(p.i+p.j, p.i, p.j)
      END
    END out;
    VAR p : setofset;
  BEGIN
    p := ss;
    WHILE ~(n IN p.set) DO p := p.next END;
    out(p)
  END print;

BEGIN
  cost := 0;
  s := {1};
  NEW(ss);
  ss.set := {1};
  ss.cost := 0;
  ss.next := NIL;
  WHILE ~(n IN s) DO
    INC(cost);
    si := ss;
    WHILE si # NIL DO
      FOR i := 1 TO n DO 
        IF i IN si.set THEN
          sj := ss;
	  WHILE sj # NIL DO
	    FOR j := 1 TO n-i DO 
	      IF j IN sj.set THEN
                IF ~((i + j) IN (si.set + sj.set)) THEN
		  IF i = j THEN
		    add(si.set + {i+j}, si.cost + 1, i, j, si, NIL);
		    add(sj.set + {i+j}, sj.cost + 1, i, j, NIL, sj)
		  ELSE
		    IF si = sj THEN
		      add(si.set + sj.set + {i+j},
		          si.cost + 1, i, j, si, NIL);
		    ELSE 
		      add(si.set + sj.set + {i+j}, 
		          si.cost + sj.cost + 1, 
		          i, j, si, sj)
	            END
                  END
		END
              END 
            END;
	    sj := sj.next
          END
        END 
      END;
      si := si.next
    END
  END;
  print
END dynamic;

VAR n : INTEGER;

BEGIN 
    n := 15;
    power(n); Out.Ln;
    dynamic(n); Out.Ln
END tPower.

(*<<
x2:=x*x; x3:=x2*x; x6:=x3*x3; x7:=x6*x; x14:=x7*x7; x15:=x14*x; 
x2:=x*x; x3:=x*x2; x6:=x3*x3; x9:=x3*x6; x15:=x6*x9; 
>>*)

(*[[
!! (SYMFILE #tPower STAMP #tPower.%main 1 #tPower.m)
!! (CHKSUM STAMP)
!! 
MODULE tPower STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tPower.var 0 3 0
! PROCEDURE var(n : INTEGER);
!   Out.String("x"); IF n # 1 THEN Out.Int(n,0) END
CONST 2
GLOBAL tPower.%3
GLOBAL Out.String
CALL 2
LDLW 12
CONST 1
JEQ L10
CONST 0
LDLW 12
GLOBAL Out.Int
CALL 2
LABEL L10
RETURN
END

PROC tPower.assign 0 3 0
! PROCEDURE assign(a, b, c : INTEGER);
!   var(a); Out.String(":="); var(b); Out.String("*"); var(c); Out.String("; ")
LDLW 12
GLOBAL tPower.var
CALL 1
CONST 3
GLOBAL tPower.%1
GLOBAL Out.String
CALL 2
LDLW 16
GLOBAL tPower.var
CALL 1
CONST 2
GLOBAL tPower.%4
GLOBAL Out.String
CALL 2
LDLW 20
GLOBAL tPower.var
CALL 1
CONST 3
GLOBAL tPower.%2
GLOBAL Out.String
CALL 2
RETURN
END

PROC tPower.power 0 4 0
! PROCEDURE power(n : INTEGER);
!   IF n # 1 THEN
LDLW 12
CONST 1
JEQ L13
!     IF n MOD 2 = 0 THEN
LDLW 12
CONST 2
MOD
JNEQZ L16
!       power(n DIV 2);
LDLW 12
CONST 2
DIV
GLOBAL tPower.power
CALL 1
!       assign(n, n DIV 2, n DIV 2);
LDLW 12
CONST 2
DIV
LDLW 12
CONST 2
DIV
LDLW 12
GLOBAL tPower.assign
CALL 3
RETURN
LABEL L16
!       power(n-1);
LDLW 12
DEC
GLOBAL tPower.power
CALL 1
!       assign(n, n-1, 1);
CONST 1
LDLW 12
DEC
LDLW 12
GLOBAL tPower.assign
CALL 3
LABEL L13
RETURN
END

PROC tPower.%5.add 8 4 0x03008001
SAVELINK
!   PROCEDURE add(newset : SET; newcost, i, j : INTEGER; left, right: setofset);
!     IF newcost <= cost THEN
LDLW 16
LDLW -4
LDNW -4
JGT L19
!       p := ss;
LDLW -4
LDNW -20
STLW -8
LABEL L20
!       WHILE (p # NIL) & (p.set # newset) DO p := p.next END;
LDLW -8
JEQZ L22
LDLW -8
NCHECK 50
LOADW
LDLW 12
JEQ L22
LDLW -8
NCHECK 50
LDNW 28
STLW -8
JUMP L20
LABEL L22
!       IF p = NIL THEN
LDLW -8
JNEQZ L26
!         NEW(p);
CONST 32
GLOBAL tPower.node
GLOBAL NEW
CALLW 2
STLW -8
!         p.set := newset;
LDLW 12
LDLW -8
NCHECK 53
STOREW
!         p.cost := MAX(INTEGER);
CONST 2147483647
LDLW -8
NCHECK 54
STNW 4
!         p.next := ss;
LDLW -4
LDNW -20
LDLW -8
NCHECK 55
STNW 28
!         ss := p
LDLW -8
LDLW -4
STNW -20
LABEL L26
!       IF newcost < p.cost THEN
LDLW 16
LDLW -8
NCHECK 58
LDNW 4
JGEQ L19
!         p.cost := newcost;
LDLW 16
LDLW -8
NCHECK 59
STNW 4
!         p.i := i;
LDLW 20
LDLW -8
NCHECK 60
STNW 12
!         p.j := j;
LDLW 24
LDLW -8
NCHECK 61
STNW 16
!         p.left := left;
LDLW 28
LDLW -8
NCHECK 62
STNW 20
!         p.right := right;
LDLW 32
LDLW -8
NCHECK 63
STNW 24
!         s := s + {i+j}
LDLW -4
LDNW -8
CONST 1
LDLW 20
LDLW 24
PLUS
CONST 32
BOUND 64
LSL
BITOR
LDLW -4
STNW -8
LABEL L19
RETURN
END

PROC tPower.%7.out 4 5 0x00100001
SAVELINK
!     PROCEDURE out(p : setofset);
!       IF (p # NIL) & (p.cost > 0) THEN
LDLW 12
JEQZ L32
LDLW 12
NCHECK 72
LDNW 4
JLEQZ L32
!         out(p.left);
LDLW 12
NCHECK 73
LDNW 20
LDLW -4
STATLINK
GLOBAL tPower.%7.out
CALL 1
! 	out(p.right);
LDLW 12
NCHECK 74
LDNW 24
LDLW -4
STATLINK
GLOBAL tPower.%7.out
CALL 1
! 	assign(p.i+p.j, p.i, p.j)
LDLW 12
NCHECK 75
LDNW 16
LDLW 12
NCHECK 75
LDNW 12
LDLW 12
NCHECK 75
LDNW 12
LDLW 12
NCHECK 75
LDNW 16
PLUS
GLOBAL tPower.assign
CALL 3
LABEL L32
RETURN
END

PROC tPower.%6.print 8 4 0x00008001
SAVELINK
!   PROCEDURE print;
!     p := ss;
LDLW -4
LDNW -20
STLW -8
LABEL L34
!     WHILE ~(n IN p.set) DO p := p.next END;
LDLW -8
NCHECK 81
LOADW
CONST 1
LDLW -4
LDNW 12
CONST 32
BOUND 81
LSL
BITAND
JNEQZ L36
LDLW -8
NCHECK 81
LDNW 28
STLW -8
JUMP L34
LABEL L36
!     out(p)
LDLW -8
LOCAL 0
STATLINK
GLOBAL tPower.%7.out
CALL 1
RETURN
END

PROC tPower.dynamic 36 9 0x00001c01
! PROCEDURE dynamic(n : INTEGER);
!   cost := 0;
CONST 0
STLW -4
!   s := {1};
CONST 2
STLW -8
!   NEW(ss);
CONST 32
GLOBAL tPower.node
GLOBAL NEW
CALLW 2
STLW -20
!   ss.set := {1};
CONST 2
LDLW -20
NCHECK 89
STOREW
!   ss.cost := 0;
CONST 0
LDLW -20
NCHECK 90
STNW 4
!   ss.next := NIL;
CONST 0
LDLW -20
NCHECK 91
STNW 28
LABEL L37
!   WHILE ~(n IN s) DO
LDLW -8
CONST 1
LDLW 12
CONST 32
BOUND 92
LSL
BITAND
JNEQZ L39
!     INC(cost);
INCL -4
!     si := ss;
LDLW -20
STLW -24
LABEL L40
!     WHILE si # NIL DO
LDLW -24
JEQZ L37
!       FOR i := 1 TO n DO 
LDLW 12
STLW -32
CONST 1
STLW -12
LABEL L43
LDLW -12
LDLW -32
JGT L44
!         IF i IN si.set THEN
LDLW -24
NCHECK 97
LOADW
CONST 1
LDLW -12
CONST 32
BOUND 97
LSL
BITAND
JEQZ L47
!           sj := ss;
LDLW -20
STLW -28
LABEL L48
! 	  WHILE sj # NIL DO
LDLW -28
JEQZ L47
! 	    FOR j := 1 TO n-i DO 
LDLW 12
LDLW -12
MINUS
STLW -36
CONST 1
STLW -16
LABEL L51
LDLW -16
LDLW -36
JGT L52
! 	      IF j IN sj.set THEN
LDLW -28
NCHECK 101
LOADW
CONST 1
LDLW -16
CONST 32
BOUND 101
LSL
BITAND
JEQZ L55
!                 IF ~((i + j) IN (si.set + sj.set)) THEN
LDLW -24
NCHECK 102
LOADW
LDLW -28
NCHECK 102
LOADW
BITOR
CONST 1
LDLW -12
LDLW -16
PLUS
CONST 32
BOUND 102
LSL
BITAND
JNEQZ L55
! 		  IF i = j THEN
LDLW -12
LDLW -16
JNEQ L64
! 		    add(si.set + {i+j}, si.cost + 1, i, j, si, NIL);
CONST 0
LDLW -24
LDLW -16
LDLW -12
LDLW -24
NCHECK 104
LDNW 4
INC
LDLW -24
NCHECK 104
LOADW
CONST 1
LDLW -12
LDLW -16
PLUS
CONST 32
BOUND 104
LSL
BITOR
LOCAL 0
STATLINK
GLOBAL tPower.%5.add
CALL 6
! 		    add(sj.set + {i+j}, sj.cost + 1, i, j, NIL, sj)
LDLW -28
CONST 0
LDLW -16
LDLW -12
LDLW -28
NCHECK 105
LDNW 4
INC
LDLW -28
NCHECK 105
LOADW
CONST 1
LDLW -12
LDLW -16
PLUS
CONST 32
BOUND 105
LSL
BITOR
LOCAL 0
STATLINK
GLOBAL tPower.%5.add
CALL 6
JUMP L55
LABEL L64
! 		    IF si = sj THEN
LDLW -24
LDLW -28
JNEQ L62
! 		      add(si.set + sj.set + {i+j},
CONST 0
LDLW -24
LDLW -16
LDLW -12
LDLW -24
NCHECK 109
LDNW 4
INC
LDLW -24
NCHECK 108
LOADW
LDLW -28
NCHECK 108
LOADW
BITOR
CONST 1
LDLW -12
LDLW -16
PLUS
CONST 32
BOUND 108
LSL
BITOR
LOCAL 0
STATLINK
GLOBAL tPower.%5.add
CALL 6
JUMP L55
LABEL L62
! 		      add(si.set + sj.set + {i+j}, 
LDLW -28
LDLW -24
LDLW -16
LDLW -12
LDLW -24
NCHECK 112
LDNW 4
LDLW -28
NCHECK 112
LDNW 4
PLUS
INC
LDLW -24
NCHECK 111
LOADW
LDLW -28
NCHECK 111
LOADW
BITOR
CONST 1
LDLW -12
LDLW -16
PLUS
CONST 32
BOUND 111
LSL
BITOR
LOCAL 0
STATLINK
GLOBAL tPower.%5.add
CALL 6
LABEL L55
! 	    FOR j := 1 TO n-i DO 
INCL -16
JUMP L51
LABEL L52
! 	    sj := sj.next
LDLW -28
NCHECK 119
LDNW 28
STLW -28
JUMP L48
LABEL L47
!       FOR i := 1 TO n DO 
INCL -12
JUMP L43
LABEL L44
!       si := si.next
LDLW -24
NCHECK 123
LDNW 28
STLW -24
JUMP L40
LABEL L39
!   print
LOCAL 0
STATLINK
GLOBAL tPower.%6.print
CALL 0
RETURN
END

PROC tPower.%main 0 2 0
!     n := 15;
CONST 15
STGW tPower.n
!     power(n); Out.Ln;
LDGW tPower.n
GLOBAL tPower.power
CALL 1
GLOBAL Out.Ln
CALL 0
!     dynamic(n); Out.Ln
LDGW tPower.n
GLOBAL tPower.dynamic
CALL 1
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tPower.n 4

! String ":="
DEFINE tPower.%1
STRING 3A3D00

! String "; "
DEFINE tPower.%2
STRING 3B2000

! String "x"
DEFINE tPower.%3
STRING 7800

! String "*"
DEFINE tPower.%4
STRING 2A00

! Descriptor for node
DEFINE tPower.node
WORD 0x000001c1
WORD 0
WORD tPower.node.%anc

DEFINE tPower.node.%anc
WORD tPower.node

! End of file
]]*)
