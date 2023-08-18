MODULE tBox;

(* Boxing the compass

N
N by E

                     / NNE
         / t(N,NE,E) | s(N,NE,E) = NE by N, NE, NE by E
         |           \ ENE
         |
u(N,E,S) | s(N,E,S) = E by N, E, E by S
         |
         |           / ESE
         \ t(E,SE,S) | s(E,SE,S) = SE by E, SE, SE by S
                     \ SSE

s(E,S,W) = S by E, S, S by W

u(S,W,N)

N by W
N
*)

IMPORT Strings, Out;

TYPE String = POINTER TO Base;
  Base = ARRAY OF CHAR;

PROCEDURE Const(CONST s: Base): String;
  VAR p: String;
BEGIN
  NEW(p, Strings.Length(s)+1);
  p^ := s;
  RETURN p
END Const;

PROCEDURE Cat(a, b: String): String;
  VAR c: String; na, nb: INTEGER;
BEGIN
  na := Strings.Length(a^); nb := Strings.Length(b^);
  NEW(c, na+nb+2);
  Strings.Insert(a^, 0, c^);
  c^[na] := ' ';
  Strings.Insert(b^, na+1, c^);
  RETURN c
END Cat;

PROCEDURE Say(a: String);
BEGIN
  Out.String(a^); Out.Ln
END Say;

PROCEDURE SayBy(a, b: String);
BEGIN
  Out.String(a^); Out.String(" by "); Out.String(b^); Out.Ln
END SayBy;

PROCEDURE Triple(a, b, c: String);
BEGIN
  SayBy(b, a); Say(b); SayBy(b, c);
END Triple;

PROCEDURE Quin(a, b, c: String);
BEGIN
  Say(Cat(a, b)); Triple(a, b, c); Say(Cat(c, b))
END Quin;

PROCEDURE Thirteen(a, b, c: String);
BEGIN
  Quin(a, Cat(a, b), b); Triple(a, b, c); Quin(b, Cat(c, b), c)
END Thirteen;

VAR north, south, east, west: String;

BEGIN
  north := Const("North");
  south := Const("South");
  east := Const("East");
  west := Const("West");

  Say(north);
  SayBy(north, east);
  Thirteen(north, east, south);
  Triple(east, south, west);
  Thirteen(south, west, north);
  SayBy(north, west);
  Say(north)
END tBox.

(*<<
North
North by East
North North East
North East by North
North East
North East by East
East North East
East by North
East
East by South
East South East
South East by East
South East
South East by South
South South East
South by East
South
South by West
South South West
South West by South
South West
South West by West
West South West
West by South
West
West by North
West North West
North West by West
North West
North West by North
North North West
North by West
North
>>*)

(*[[
!! (SYMFILE #tBox STAMP #tBox.%main 1 #tBox.m)
!! (CHKSUM STAMP)
!! 
MODULE tBox STAMP 0
IMPORT Strings STAMP
IMPORT Out STAMP
ENDHDR

PROC tBox.Const 4 7 0x00110001
! PROCEDURE Const(CONST s: Base): String;
!   NEW(p, Strings.Length(s)+1);
LDLW 16
LDLW 12
GLOBAL Strings.Length
CALLW 2
INC
CONST 1
CONST 1
CONST 0
GLOBAL NEWFLEX
CALLW 4
STLW -4
!   p^ := s;
LDLW -4
NCHECK 35
DUP 0
LDNW -4
LDNW 4
SWAP
LDLW 16
LDLW 12
CONST 1
CONST 1
GLOBAL FLEXASSIGN
CALL 6
!   RETURN p
LDLW -4
RETURN
END

PROC tBox.Cat 12 6 0x00310001
! PROCEDURE Cat(a, b: String): String;
!   na := Strings.Length(a^); nb := Strings.Length(b^);
LDLW 12
NCHECK 42
DUP 0
LDNW -4
LDNW 4
SWAP
GLOBAL Strings.Length
CALLW 2
STLW -8
LDLW 16
NCHECK 42
DUP 0
LDNW -4
LDNW 4
SWAP
GLOBAL Strings.Length
CALLW 2
STLW -12
!   NEW(c, na+nb+2);
LDLW -8
LDLW -12
PLUS
CONST 2
PLUS
CONST 1
CONST 1
CONST 0
GLOBAL NEWFLEX
CALLW 4
STLW -4
!   Strings.Insert(a^, 0, c^);
LDLW -4
NCHECK 44
DUP 0
LDNW -4
LDNW 4
SWAP
CONST 0
LDLW 12
NCHECK 44
DUP 0
LDNW -4
LDNW 4
SWAP
GLOBAL Strings.Insert
CALL 5
!   c^[na] := ' ';
CONST 32
LDLW -4
NCHECK 45
LDLW -8
DUP 1
LDNW -4
LDNW 4
BOUND 45
STIC
!   Strings.Insert(b^, na+1, c^);
LDLW -4
NCHECK 46
DUP 0
LDNW -4
LDNW 4
SWAP
LDLW -8
INC
LDLW 16
NCHECK 46
DUP 0
LDNW -4
LDNW 4
SWAP
GLOBAL Strings.Insert
CALL 5
!   RETURN c
LDLW -4
RETURN
END

PROC tBox.Say 0 3 0x00100001
! PROCEDURE Say(a: String);
!   Out.String(a^); Out.Ln
LDLW 12
NCHECK 52
DUP 0
LDNW -4
LDNW 4
SWAP
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

PROC tBox.SayBy 0 3 0x00300001
! PROCEDURE SayBy(a, b: String);
!   Out.String(a^); Out.String(" by "); Out.String(b^); Out.Ln
LDLW 12
NCHECK 57
DUP 0
LDNW -4
LDNW 4
SWAP
GLOBAL Out.String
CALL 2
CONST 5
GLOBAL tBox.%1
GLOBAL Out.String
CALL 2
LDLW 16
NCHECK 57
DUP 0
LDNW -4
LDNW 4
SWAP
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

PROC tBox.Triple 0 3 0x00700001
! PROCEDURE Triple(a, b, c: String);
!   SayBy(b, a); Say(b); SayBy(b, c);
LDLW 12
LDLW 16
GLOBAL tBox.SayBy
CALL 2
LDLW 16
GLOBAL tBox.Say
CALL 1
LDLW 20
LDLW 16
GLOBAL tBox.SayBy
CALL 2
RETURN
END

PROC tBox.Quin 0 4 0x00700001
! PROCEDURE Quin(a, b, c: String);
!   Say(Cat(a, b)); Triple(a, b, c); Say(Cat(c, b))
LDLW 16
LDLW 12
GLOBAL tBox.Cat
CALLW 2
GLOBAL tBox.Say
CALL 1
LDLW 20
LDLW 16
LDLW 12
GLOBAL tBox.Triple
CALL 3
LDLW 16
LDLW 20
GLOBAL tBox.Cat
CALLW 2
GLOBAL tBox.Say
CALL 1
RETURN
END

PROC tBox.Thirteen 0 4 0x00700001
! PROCEDURE Thirteen(a, b, c: String);
!   Quin(a, Cat(a, b), b); Triple(a, b, c); Quin(b, Cat(c, b), c)
LDLW 16
LDLW 16
LDLW 12
GLOBAL tBox.Cat
STKMAP 0x00000009
CALLW 2
LDLW 12
GLOBAL tBox.Quin
CALL 3
LDLW 20
LDLW 16
LDLW 12
GLOBAL tBox.Triple
CALL 3
LDLW 20
LDLW 16
LDLW 20
GLOBAL tBox.Cat
STKMAP 0x00000009
CALLW 2
LDLW 16
GLOBAL tBox.Quin
CALL 3
RETURN
END

PROC tBox.%main 0 4 0
!   north := Const("North");
CONST 6
GLOBAL tBox.%2
GLOBAL tBox.Const
CALLW 2
STGW tBox.north
!   south := Const("South");
CONST 6
GLOBAL tBox.%3
GLOBAL tBox.Const
CALLW 2
STGW tBox.south
!   east := Const("East");
CONST 5
GLOBAL tBox.%4
GLOBAL tBox.Const
CALLW 2
STGW tBox.east
!   west := Const("West");
CONST 5
GLOBAL tBox.%5
GLOBAL tBox.Const
CALLW 2
STGW tBox.west
!   Say(north);
LDGW tBox.north
GLOBAL tBox.Say
CALL 1
!   SayBy(north, east);
LDGW tBox.east
LDGW tBox.north
GLOBAL tBox.SayBy
CALL 2
!   Thirteen(north, east, south);
LDGW tBox.south
LDGW tBox.east
LDGW tBox.north
GLOBAL tBox.Thirteen
CALL 3
!   Triple(east, south, west);
LDGW tBox.west
LDGW tBox.south
LDGW tBox.east
GLOBAL tBox.Triple
CALL 3
!   Thirteen(south, west, north);
LDGW tBox.north
LDGW tBox.west
LDGW tBox.south
GLOBAL tBox.Thirteen
CALL 3
!   SayBy(north, west);
LDGW tBox.west
LDGW tBox.north
GLOBAL tBox.SayBy
CALL 2
!   Say(north)
LDGW tBox.north
GLOBAL tBox.Say
CALL 1
RETURN
END

! Global variables
GLOVAR tBox.north 4
GLOVAR tBox.south 4
GLOVAR tBox.east 4
GLOVAR tBox.west 4

! Global pointer map
DEFINE tBox.%gcmap
WORD GC_POINTER
WORD tBox.north
WORD GC_POINTER
WORD tBox.south
WORD GC_POINTER
WORD tBox.east
WORD GC_POINTER
WORD tBox.west
WORD GC_END

! String " by "
DEFINE tBox.%1
STRING 2062792000

! String "North"
DEFINE tBox.%2
STRING 4E6F72746800

! String "South"
DEFINE tBox.%3
STRING 536F75746800

! String "East"
DEFINE tBox.%4
STRING 4561737400

! String "West"
DEFINE tBox.%5
STRING 5765737400

! End of file
]]*)
