MODULE tGraeme;

IMPORT Out;

TYPE Object = POINTER TO Record;
  Record = RECORD END;

PROCEDURE (self: Object) print;
BEGIN Out.String("anObject") END print;

TYPE Integer = POINTER TO RECORD(Record) val: INTEGER END;

PROCEDURE newInteger(val: INTEGER): Integer;
  VAR p: Integer;
BEGIN
  NEW(p); p.val := val; RETURN p
END newInteger;

PROCEDURE (self: Integer) add(other: Integer): Integer;
BEGIN RETURN newInteger(self.val + other.val) END add;

PROCEDURE (self: Integer) print;
BEGIN Out.Int(self.val, 0) END print;

TYPE Car = POINTER TO RECORD(Record) dist: Integer END;

PROCEDURE newCar(dist: Integer): Car;
  VAR p: Car;
BEGIN
  NEW(p); p.dist := dist; RETURN p
END newCar;

PROCEDURE (self: Car) drive(x: Integer);
BEGIN self.dist := self.dist.add(x) END drive;

PROCEDURE (self: Car) print;
BEGIN
  Out.String("aCar("); self.dist.print; Out.String(")")
END print;

(* Assume that the initial action is to call the 'main' method of
   an instance of class 'Main' *)

TYPE Main = POINTER TO RECORD(Record) END;

PROCEDURE newMain(): Main;
  VAR p: Main;
BEGIN
  NEW(p); RETURN p
END newMain;

PROCEDURE (self: Main) main;
  VAR c: Car;
BEGIN
  c := newCar(newInteger(10));
  c.drive(newInteger(5));
  c.print; Out.Ln
END main;

PROCEDURE init;
  VAR m: Main;
BEGIN
  m := newMain();
  m.main
END init;

BEGIN
  init
END tGraeme.

(*<<
aCar(15)
>>*)

(*[[
!! (SYMFILE #tGraeme STAMP #tGraeme.%main 1 #tGraeme.m)
!! (CHKSUM STAMP)
!! 
MODULE tGraeme STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tGraeme.Record.print 0 3 0x00100001
! PROCEDURE (self: Object) print;
! BEGIN Out.String("anObject") END print;
CONST 9
GLOBAL tGraeme.%1
GLOBAL Out.String
CALL 2
RETURN
END

PROC tGraeme.newInteger 4 3 0x00010001
! PROCEDURE newInteger(val: INTEGER): Integer;
!   NEW(p); p.val := val; RETURN p
CONST 4
GLOBAL tGraeme.%3
GLOBAL NEW
CALLW 2
STLW -4
LDLW 12
LDLW -4
NCHECK 16
STOREW
LDLW -4
RETURN
END

PROC tGraeme.%3.add 0 3 0x00300001
! PROCEDURE (self: Integer) add(other: Integer): Integer;
! BEGIN RETURN newInteger(self.val + other.val) END add;
LDLW 12
NCHECK 20
LOADW
LDLW 16
NCHECK 20
LOADW
PLUS
GLOBAL tGraeme.newInteger
CALLW 1
RETURN
END

PROC tGraeme.%3.print 0 3 0x00100001
! PROCEDURE (self: Integer) print;
! BEGIN Out.Int(self.val, 0) END print;
CONST 0
LDLW 12
NCHECK 23
LOADW
GLOBAL Out.Int
CALL 2
RETURN
END

PROC tGraeme.newCar 4 3 0x00110001
! PROCEDURE newCar(dist: Integer): Car;
!   NEW(p); p.dist := dist; RETURN p
CONST 4
GLOBAL tGraeme.%4
GLOBAL NEW
CALLW 2
STLW -4
LDLW 12
LDLW -4
NCHECK 30
STOREW
LDLW -4
RETURN
END

PROC tGraeme.%4.drive 0 4 0x00300001
! PROCEDURE (self: Car) drive(x: Integer);
! BEGIN self.dist := self.dist.add(x) END drive;
LDLW 16
LDLW 12
NCHECK 34
LOADW
NCHECK 34
DUP 0
LDNW -4
LDNW 16
CALLW 2
LDLW 12
NCHECK 34
STOREW
RETURN
END

PROC tGraeme.%4.print 0 3 0x00100001
! PROCEDURE (self: Car) print;
!   Out.String("aCar("); self.dist.print; Out.String(")")
CONST 6
GLOBAL tGraeme.%2
GLOBAL Out.String
CALL 2
LDLW 12
NCHECK 38
LOADW
NCHECK 38
DUP 0
LDNW -4
LDNW 12
CALL 1
CONST 2
GLOBAL tGraeme.%6
GLOBAL Out.String
CALL 2
RETURN
END

PROC tGraeme.newMain 4 3 0x00010001
! PROCEDURE newMain(): Main;
!   NEW(p); RETURN p
CONST 0
GLOBAL tGraeme.%5
GLOBAL NEW
CALLW 2
STLW -4
LDLW -4
RETURN
END

PROC tGraeme.%5.main 4 4 0x00110001
! PROCEDURE (self: Main) main;
!   c := newCar(newInteger(10));
CONST 10
GLOBAL tGraeme.newInteger
CALLW 1
GLOBAL tGraeme.newCar
CALLW 1
STLW -4
!   c.drive(newInteger(5));
CONST 5
GLOBAL tGraeme.newInteger
CALLW 1
LDLW -4
NCHECK 56
DUP 0
LDNW -4
LDNW 16
CALL 2
!   c.print; Out.Ln
LDLW -4
NCHECK 57
DUP 0
LDNW -4
LDNW 12
CALL 1
GLOBAL Out.Ln
CALL 0
RETURN
END

PROC tGraeme.init 4 3 0x00010001
! PROCEDURE init;
!   m := newMain();
GLOBAL tGraeme.newMain
CALLW 0
STLW -4
!   m.main
LDLW -4
NCHECK 64
DUP 0
LDNW -4
LDNW 16
CALL 1
RETURN
END

PROC tGraeme.%main 0 1 0
!   init
GLOBAL tGraeme.init
CALL 0
RETURN
END

! String "anObject"
DEFINE tGraeme.%1
STRING 616E4F626A65637400

! String "aCar("
DEFINE tGraeme.%2
STRING 614361722800

! String ")"
DEFINE tGraeme.%6
STRING 2900

! Descriptor for Record
DEFINE tGraeme.Record
WORD 0
WORD 0
WORD tGraeme.Record.%anc
WORD tGraeme.Record.print

DEFINE tGraeme.Record.%anc
WORD tGraeme.Record

! Descriptor for *anon*
DEFINE tGraeme.%3
WORD 0
WORD 1
WORD tGraeme.%3.%anc
WORD tGraeme.%3.print
WORD tGraeme.%3.add

DEFINE tGraeme.%3.%anc
WORD tGraeme.Record
WORD tGraeme.%3

! Descriptor for *anon*
DEFINE tGraeme.%4
WORD 0x00000003
WORD 1
WORD tGraeme.%4.%anc
WORD tGraeme.%4.print
WORD tGraeme.%4.drive

DEFINE tGraeme.%4.%anc
WORD tGraeme.Record
WORD tGraeme.%4

! Descriptor for *anon*
DEFINE tGraeme.%5
WORD 0
WORD 1
WORD tGraeme.%5.%anc
WORD tGraeme.Record.print
WORD tGraeme.%5.main

DEFINE tGraeme.%5.%anc
WORD tGraeme.Record
WORD tGraeme.%5

! End of file
]]*)
