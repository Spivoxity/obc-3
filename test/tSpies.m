MODULE tSpies;

IMPORT M := MathL, Out;

TYPE R = LONGREAL;

TYPE Vector = POINTER TO RECORD x, y, z: R END;

PROCEDURE Unit(lat, long: R): Vector;
  VAR r: Vector; c, s: R;
BEGIN
  NEW(r);
  c := M.Cos(M.pi*lat/180);
  s := M.Sin(M.pi*lat/180);
  r.x := c * M.Cos(M.pi*long/180);
  r.y := c * M.Sin(M.pi*long/180);
  r.z := s;
  RETURN r
END Unit;

PROCEDURE Cross(a, b: Vector): Vector;
  VAR r: Vector;
BEGIN
  NEW(r);
  r.x := a.y*b.z - b.y*a.z;
  r.y := a.z*b.x - b.z*a.x;
  r.z := a.x*b.y - b.x*a.y;
  RETURN r
END Cross;

PROCEDURE LatLong(a: Vector; VAR lat, long: R);
  VAR u: R;
BEGIN
  u := M.Sqrt(a.x*a.x + a.y*a.y);
  lat := M.Arctan2(a.z, u) * 180/M.pi;
  long := M.Arctan2(a.y, a.x) * 180/M.pi
END LatLong;

VAR lat, long: R; v1, v2, v3, v4: Vector;

BEGIN
  v1 := Unit(50.313685,-4.223152);	(* Rame Head *)
  v2 := Unit(52.40025,-4.088076);	(* Near Aberystwyth *)
  v3 := Unit(51.487304,-0.124317);	(* MI6 HQ *)
  v4 := Unit(51.99647,-0.74287);	(* Bletchley Park *)
  LatLong(Cross(Cross(v2, v3), Cross(v1, v4)), lat, long);
  Out.LongReal(lat); Out.String(", "); Out.LongReal(long); Out.Ln
END tSpies.

(*<<
51.7643808020, -1.25163824851
>>*)

(*[[
!! (SYMFILE #tSpies STAMP #tSpies.%main 1 #tSpies.m)
!! (CHKSUM STAMP)
!! 
MODULE tSpies STAMP 0
IMPORT MathL STAMP
IMPORT Out STAMP
ENDHDR

PROC tSpies.Unit 20 6 0x00010001
! PROCEDURE Unit(lat, long: R): Vector;
!   NEW(r);
CONST 24
GLOBAL tSpies.%2
GLOBAL NEW
CALLW 2
STLW -4
!   c := M.Cos(M.pi*lat/180);
LDLD 12
DCONST 3.14159265359
DTIMES
DCONST 180.0
DDIV
GLOBAL MathL.Cos
CALLD 2
STLD -12
!   s := M.Sin(M.pi*lat/180);
LDLD 12
DCONST 3.14159265359
DTIMES
DCONST 180.0
DDIV
GLOBAL MathL.Sin
CALLD 2
STLD -20
!   r.x := c * M.Cos(M.pi*long/180);
LDLD -12
LDLD 20
DCONST 3.14159265359
DTIMES
DCONST 180.0
DDIV
GLOBAL MathL.Cos
CALLD 2
DTIMES
LDLW -4
NCHECK 15
STORED
!   r.y := c * M.Sin(M.pi*long/180);
LDLD -12
LDLD 20
DCONST 3.14159265359
DTIMES
DCONST 180.0
DDIV
GLOBAL MathL.Sin
CALLD 2
DTIMES
LDLW -4
NCHECK 16
CONST 1
STID
!   r.z := s;
LDLD -20
LDLW -4
NCHECK 17
CONST 2
STID
!   RETURN r
LDLW -4
RETURN
END

PROC tSpies.Cross 4 6 0x00310001
! PROCEDURE Cross(a, b: Vector): Vector;
!   NEW(r);
CONST 24
GLOBAL tSpies.%2
GLOBAL NEW
CALLW 2
STLW -4
!   r.x := a.y*b.z - b.y*a.z;
LDLW 12
NCHECK 25
CONST 1
LDID
LDLW 16
NCHECK 25
CONST 2
LDID
DTIMES
LDLW 16
NCHECK 25
CONST 1
LDID
LDLW 12
NCHECK 25
CONST 2
LDID
DTIMES
DMINUS
LDLW -4
NCHECK 25
STORED
!   r.y := a.z*b.x - b.z*a.x;
LDLW 12
NCHECK 26
CONST 2
LDID
LDLW 16
NCHECK 26
LOADD
DTIMES
LDLW 16
NCHECK 26
CONST 2
LDID
LDLW 12
NCHECK 26
LOADD
DTIMES
DMINUS
LDLW -4
NCHECK 26
CONST 1
STID
!   r.z := a.x*b.y - b.x*a.y;
LDLW 12
NCHECK 27
LOADD
LDLW 16
NCHECK 27
CONST 1
LDID
DTIMES
LDLW 16
NCHECK 27
LOADD
LDLW 12
NCHECK 27
CONST 1
LDID
DTIMES
DMINUS
LDLW -4
NCHECK 27
CONST 2
STID
!   RETURN r
LDLW -4
RETURN
END

PROC tSpies.LatLong 8 6 0x00700001
! PROCEDURE LatLong(a: Vector; VAR lat, long: R);
!   u := M.Sqrt(a.x*a.x + a.y*a.y);
LDLW 12
NCHECK 34
LOADD
LDLW 12
NCHECK 34
LOADD
DTIMES
LDLW 12
NCHECK 34
CONST 1
LDID
LDLW 12
NCHECK 34
CONST 1
LDID
DTIMES
DPLUS
GLOBAL MathL.Sqrt
CALLD 2
STLD -8
!   lat := M.Arctan2(a.z, u) * 180/M.pi;
LDLD -8
LDLW 12
NCHECK 35
CONST 2
LDID
GLOBAL MathL.Arctan2
CALLD 4
DCONST 180.0
DTIMES
DCONST 3.14159265359
DDIV
LDLW 16
STORED
!   long := M.Arctan2(a.y, a.x) * 180/M.pi
LDLW 12
NCHECK 36
LOADD
LDLW 12
NCHECK 36
CONST 1
LDID
GLOBAL MathL.Arctan2
CALLD 4
DCONST 180.0
DTIMES
DCONST 3.14159265359
DDIV
LDLW 20
STORED
RETURN
END

PROC tSpies.%main 0 6 0
!   v1 := Unit(50.313685,-4.223152);	(* Rame Head *)
DCONST -4.223152
DCONST 50.313685
GLOBAL tSpies.Unit
CALLW 4
STGW tSpies.v1
!   v2 := Unit(52.40025,-4.088076);	(* Near Aberystwyth *)
DCONST -4.088076
DCONST 52.40025
GLOBAL tSpies.Unit
CALLW 4
STGW tSpies.v2
!   v3 := Unit(51.487304,-0.124317);	(* MI6 HQ *)
DCONST -0.124317
DCONST 51.487304
GLOBAL tSpies.Unit
CALLW 4
STGW tSpies.v3
!   v4 := Unit(51.99647,-0.74287);	(* Bletchley Park *)
DCONST -0.74287
DCONST 51.99647
GLOBAL tSpies.Unit
CALLW 4
STGW tSpies.v4
!   LatLong(Cross(Cross(v2, v3), Cross(v1, v4)), lat, long);
GLOBAL tSpies.long
GLOBAL tSpies.lat
LDGW tSpies.v4
LDGW tSpies.v1
GLOBAL tSpies.Cross
CALLW 2
LDGW tSpies.v3
LDGW tSpies.v2
GLOBAL tSpies.Cross
STKMAP 0x00000009
CALLW 2
GLOBAL tSpies.Cross
CALLW 2
GLOBAL tSpies.LatLong
CALL 3
!   Out.LongReal(lat); Out.String(", "); Out.LongReal(long); Out.Ln
LDGD tSpies.lat
GLOBAL Out.LongReal
CALL 2
CONST 3
GLOBAL tSpies.%1
GLOBAL Out.String
CALL 2
LDGD tSpies.long
GLOBAL Out.LongReal
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tSpies.lat 8
GLOVAR tSpies.long 8
GLOVAR tSpies.v1 4
GLOVAR tSpies.v2 4
GLOVAR tSpies.v3 4
GLOVAR tSpies.v4 4

! Global pointer map
DEFINE tSpies.%gcmap
WORD GC_POINTER
WORD tSpies.v1
WORD GC_POINTER
WORD tSpies.v2
WORD GC_POINTER
WORD tSpies.v3
WORD GC_POINTER
WORD tSpies.v4
WORD GC_END

! String ", "
DEFINE tSpies.%1
STRING 2C2000

! Descriptor for *anon*
DEFINE tSpies.%2
WORD 0
WORD 0
WORD tSpies.%2.%anc

DEFINE tSpies.%2.%anc
WORD tSpies.%2

! End of file
]]*)
