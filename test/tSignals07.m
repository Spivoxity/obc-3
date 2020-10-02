MODULE tSignals07;

IMPORT Bit, Out;

CONST xxx = 0F0H; yyy = 0CCH; zzz = 0AAH;

TYPE Sig = POINTER TO Blob;
  Blob = RECORD
      op: CHAR;			(* '&', '|', '~', 'x', 'y', 'z' *)
      left, right: Sig;		(* Operands *)
      value: INTEGER;		(* Truth table *)
      id: INTEGER;		(* Index in stack *)
      marked: BOOLEAN		(* Whether used for outputs *)
    END;

VAR
  signal: ARRAY 256 OF Sig;	(* signals for each value *)
  stack: ARRAY 1024 OF Sig;     (* signals in order of creation *)
  sp: INTEGER;

PROCEDURE Pop(sp0: INTEGER);
BEGIN
  WHILE sp > sp0 DO
    DEC(sp);
    signal[stack[sp].value] := NIL
  END
END Pop;  

PROCEDURE MakeSig(op: CHAR; left, right: Sig; value: INTEGER);
  VAR p: Sig;
BEGIN
  IF signal[value] = NIL THEN
  NEW(p);
  p.op := op; p.left := left; p.right := right; p.value := value;
  p.id := sp; p.marked := FALSE;
  signal[value] := p; stack[sp] := p; INC(sp); 
END
END MakeSig;

PROCEDURE Combine(u, v: Sig);
BEGIN
  MakeSig('&', u, v, Bit.And(u.value, v.value));
  MakeSig('|', u, v, Bit.Or(u.value, v.value))
END Combine;

PROCEDURE Negate(u: Sig): BOOLEAN;
  VAR v: INTEGER;
r:BOOLEAN;
BEGIN
  v := Bit.Xor(u.value, 0FFH);
  IF signal[v] # NIL THEN r := FALSE ELSE
  MakeSig('~', NIL, u, v);
  r := TRUE
END RETURN r
END Negate;

PROCEDURE Closure(lo: INTEGER);
  VAR i, j: INTEGER;
BEGIN
  i := lo;
  WHILE i < sp DO
    j := 0;
    WHILE j < i DO
      Combine(stack[j], stack[i]);
      INC(j)
    END;
    INC(i)
  END
END Closure;

PROCEDURE OutBits(v: INTEGER);
  VAR j: INTEGER;
BEGIN
  FOR j := 0 TO 7 DO
    Out.Int(Bit.And(LSR(v, j), 1), 0)
  END
END OutBits;

PROCEDURE OutName(p: Sig);
BEGIN
  Out.Char('s'); Out.Int(p.id, 0)
END OutName;

PROCEDURE OutArc(p, q: Sig);
BEGIN
  OutName(p); Out.String(" -> "); OutName(q); Out.Ln
END OutArc;

PROCEDURE Print(p: Sig);
BEGIN
  OutName(p); Out.String('[label="'); 
  Out.Char(p.op); Out.String(" ("); OutName(p);
  Out.String(')"]'); Out.Ln;
  Out.String('# '); OutName(p); 
  Out.Char('['); OutBits(p.value); Out.String("] = ");  
  CASE p.op OF
      'x', 'y', 'z':
	Out.Char(p.op); Out.Ln
    | '&', '|':
	OutName(p.left); Out.Char(' '); Out.Char(p.op); 
	Out.Char(' '); OutName(p.right); Out.Ln;
	OutArc(p.left, p); OutArc(p.right, p)
    | '~':
	Out.Char(p.op); Out.Char(' '); OutName(p.right); Out.Ln;
	OutArc(p.right, p)
  END;
END Print;

PROCEDURE Mark(s: Sig);
BEGIN
  IF (s # NIL) & ~s.marked THEN
    s.marked := TRUE;
    Mark(s.left); Mark(s.right)
  END
END Mark;

PROCEDURE PrintMarked;
  VAR i: INTEGER;
BEGIN
  FOR i := 0 TO sp-1 DO
    IF stack[i].marked THEN
      stack[i].marked := FALSE;
      Print(stack[i])
    END
  END
END PrintMarked;      

PROCEDURE Check;
  VAR notx, noty, notz: INTEGER;
BEGIN
  notx := Bit.Xor(xxx, 0FFH); 
  noty := Bit.Xor(yyy, 0FFH);
  notz := Bit.Xor(zzz, 0FFH);
  IF (signal[notx] # NIL) & (signal[noty] # NIL) & (signal[notz] # NIL) THEN
     Mark(signal[notx]); Mark(signal[noty]); Mark(signal[notz]);
     Out.String("digraph Circuit {"); Out.Ln;
     PrintMarked;
     Out.String("}"); Out.Ln
  END
END Check;

PROCEDURE Search;
  VAR i, j, u, v: INTEGER;
BEGIN
  MakeSig('x', NIL, NIL, xxx);
  MakeSig('y', NIL, NIL, yyy);
  MakeSig('z', NIL, NIL, zzz);
  Closure(0);
  u := sp;
  FOR i := 0 TO u-1 DO
    IF Negate(stack[i]) THEN
      Closure(sp-1);
      v := sp;
      FOR j := i+1 TO v-1 DO
        IF Negate(stack[j]) THEN
          Closure(sp-1);
	  Check;
	  Pop(v)
        END
      END;
      Pop(u)
    END
  END;
END Search;

BEGIN
  sp := 0;
  Search
END tSignals07.

(*<<
digraph Circuit {
s0[label="x (s0)"]
# s0[00001111] = x
s1[label="y (s1)"]
# s1[00110011] = y
s2[label="z (s2)"]
# s2[01010101] = z
s3[label="& (s3)"]
# s3[00000011] = s0 & s1
s0 -> s3
s1 -> s3
s4[label="| (s4)"]
# s4[00111111] = s0 | s1
s0 -> s4
s1 -> s4
s5[label="& (s5)"]
# s5[00000101] = s0 & s2
s0 -> s5
s2 -> s5
s6[label="| (s6)"]
# s6[01011111] = s0 | s2
s0 -> s6
s2 -> s6
s7[label="& (s7)"]
# s7[00010001] = s1 & s2
s1 -> s7
s2 -> s7
s8[label="| (s8)"]
# s8[01110111] = s1 | s2
s1 -> s8
s2 -> s8
s9[label="& (s9)"]
# s9[00000001] = s2 & s3
s2 -> s9
s3 -> s9
s10[label="| (s10)"]
# s10[01010111] = s2 | s3
s2 -> s10
s3 -> s10
s12[label="| (s12)"]
# s12[01111111] = s2 | s4
s2 -> s12
s4 -> s12
s17[label="& (s17)"]
# s17[00010111] = s4 & s10
s4 -> s17
s10 -> s17
s18[label="~ (s18)"]
# s18[11101000] = ~ s17
s17 -> s18
s26[label="| (s26)"]
# s26[11101011] = s3 | s18
s3 -> s26
s18 -> s26
s27[label="& (s27)"]
# s27[00101000] = s4 & s18
s4 -> s27
s18 -> s27
s29[label="| (s29)"]
# s29[11101101] = s5 | s18
s5 -> s29
s18 -> s29
s30[label="& (s30)"]
# s30[01001000] = s6 & s18
s6 -> s30
s18 -> s30
s31[label="| (s31)"]
# s31[11111001] = s7 | s18
s7 -> s31
s18 -> s31
s32[label="& (s32)"]
# s32[01100000] = s8 & s18
s8 -> s32
s18 -> s32
s33[label="| (s33)"]
# s33[11101001] = s9 | s18
s9 -> s33
s18 -> s33
s80[label="& (s80)"]
# s80[01101001] = s12 & s33
s12 -> s80
s33 -> s80
s81[label="~ (s81)"]
# s81[10010110] = ~ s80
s80 -> s81
s104[label="& (s104)"]
# s104[10000010] = s26 & s81
s26 -> s104
s81 -> s104
s106[label="& (s106)"]
# s106[10000100] = s29 & s81
s29 -> s106
s81 -> s106
s108[label="& (s108)"]
# s108[10010000] = s31 & s81
s31 -> s108
s81 -> s108
s247[label="| (s247)"]
# s247[10101010] = s27 | s104
s27 -> s247
s104 -> s247
s252[label="| (s252)"]
# s252[11001100] = s30 | s106
s30 -> s252
s106 -> s252
s255[label="| (s255)"]
# s255[11110000] = s32 | s108
s32 -> s255
s108 -> s255
}
>>*)

(*[[
!! (SYMFILE #tSignals07 STAMP #tSignals07.%main 1 #tSignals07.m)
!! (CHKSUM STAMP)
!! 
MODULE tSignals07 STAMP 0
IMPORT Bit STAMP
IMPORT Out STAMP
ENDHDR

PROC tSignals07.Pop 0 5 0
! PROCEDURE Pop(sp0: INTEGER);
LABEL L9
!   WHILE sp > sp0 DO
LDGW tSignals07.sp
LDLW 12
JLEQ L11
!     DEC(sp);
LDGW tSignals07.sp
DEC
STGW tSignals07.sp
!     signal[stack[sp].value] := NIL
CONST 0
GLOBAL tSignals07.signal
GLOBAL tSignals07.stack
LDGW tSignals07.sp
CONST 1024
BOUND 25
LDIW
NCHECK 25
LDNW 12
CONST 256
BOUND 25
STIW
JUMP L9
LABEL L11
RETURN
END

PROC tSignals07.MakeSig 4 4 0x00610001
! PROCEDURE MakeSig(op: CHAR; left, right: Sig; value: INTEGER);
!   IF signal[value] = NIL THEN
GLOBAL tSignals07.signal
LDLW 24
CONST 256
BOUND 32
LDIW
JNEQZ L14
!   NEW(p);
CONST 24
GLOBAL tSignals07.Blob
GLOBAL NEW
CALLW 2
STLW -4
!   p.op := op; p.left := left; p.right := right; p.value := value;
LDLC 12
LDLW -4
NCHECK 34
STOREC
LDLW 16
LDLW -4
NCHECK 34
STNW 4
LDLW 20
LDLW -4
NCHECK 34
STNW 8
LDLW 24
LDLW -4
NCHECK 34
STNW 12
!   p.id := sp; p.marked := FALSE;
LDGW tSignals07.sp
LDLW -4
NCHECK 35
STNW 16
CONST 0
LDLW -4
NCHECK 35
CONST 20
STIC
!   signal[value] := p; stack[sp] := p; INC(sp); 
LDLW -4
GLOBAL tSignals07.signal
LDLW 24
CONST 256
BOUND 36
STIW
LDLW -4
GLOBAL tSignals07.stack
LDGW tSignals07.sp
CONST 1024
BOUND 36
STIW
LDGW tSignals07.sp
INC
STGW tSignals07.sp
LABEL L14
RETURN
END

PROC tSignals07.Combine 0 5 0x00300001
! PROCEDURE Combine(u, v: Sig);
!   MakeSig('&', u, v, Bit.And(u.value, v.value));
LDLW 16
NCHECK 42
LDNW 12
LDLW 12
NCHECK 42
LDNW 12
GLOBAL Bit.And
CALLW 2
LDLW 16
LDLW 12
CONST 38
ALIGNC
GLOBAL tSignals07.MakeSig
CALL 4
!   MakeSig('|', u, v, Bit.Or(u.value, v.value))
LDLW 16
NCHECK 43
LDNW 12
LDLW 12
NCHECK 43
LDNW 12
GLOBAL Bit.Or
CALLW 2
LDLW 16
LDLW 12
CONST 124
ALIGNC
GLOBAL tSignals07.MakeSig
CALL 4
RETURN
END

PROC tSignals07.Negate 8 5 0x00100001
! PROCEDURE Negate(u: Sig): BOOLEAN;
!   v := Bit.Xor(u.value, 0FFH);
CONST 255
LDLW 12
NCHECK 50
LDNW 12
GLOBAL Bit.Xor
CALLW 2
STLW -4
!   IF signal[v] # NIL THEN r := FALSE ELSE
GLOBAL tSignals07.signal
LDLW -4
CONST 256
BOUND 51
LDIW
JEQZ L17
CONST 0
STLC -5
JUMP L15
LABEL L17
!   MakeSig('~', NIL, u, v);
LDLW -4
LDLW 12
CONST 0
CONST 126
ALIGNC
GLOBAL tSignals07.MakeSig
CALL 4
!   r := TRUE
CONST 1
STLC -5
LABEL L15
! END RETURN r
LDLC -5
RETURN
END

PROC tSignals07.Closure 8 4 0
! PROCEDURE Closure(lo: INTEGER);
!   i := lo;
LDLW 12
STLW -4
LABEL L18
!   WHILE i < sp DO
LDLW -4
LDGW tSignals07.sp
JGEQ L20
!     j := 0;
CONST 0
STLW -8
LABEL L21
!     WHILE j < i DO
LDLW -8
LDLW -4
JGEQ L23
!       Combine(stack[j], stack[i]);
GLOBAL tSignals07.stack
LDLW -4
CONST 1024
BOUND 64
LDIW
GLOBAL tSignals07.stack
LDLW -8
CONST 1024
BOUND 64
LDIW
GLOBAL tSignals07.Combine
CALL 2
!       INC(j)
INCL -8
JUMP L21
LABEL L23
!     INC(i)
INCL -4
JUMP L18
LABEL L20
RETURN
END

PROC tSignals07.OutBits 4 4 0
! PROCEDURE OutBits(v: INTEGER);
!   FOR j := 0 TO 7 DO
CONST 0
STLW -4
LABEL L24
LDLW -4
CONST 7
JGT L25
!     Out.Int(Bit.And(LSR(v, j), 1), 0)
CONST 0
CONST 1
LDLW 12
LDLW -4
LSR
GLOBAL Bit.And
CALLW 2
GLOBAL Out.Int
CALL 2
!   FOR j := 0 TO 7 DO
INCL -4
JUMP L24
LABEL L25
RETURN
END

PROC tSignals07.OutName 0 3 0x00100001
! PROCEDURE OutName(p: Sig);
!   Out.Char('s'); Out.Int(p.id, 0)
CONST 115
ALIGNC
GLOBAL Out.Char
CALL 1
CONST 0
LDLW 12
NCHECK 81
LDNW 16
GLOBAL Out.Int
CALL 2
RETURN
END

PROC tSignals07.OutArc 0 3 0x00300001
! PROCEDURE OutArc(p, q: Sig);
!   OutName(p); Out.String(" -> "); OutName(q); Out.Ln
LDLW 12
GLOBAL tSignals07.OutName
CALL 1
CONST 5
GLOBAL tSignals07.%1
GLOBAL Out.String
CALL 2
LDLW 16
GLOBAL tSignals07.OutName
CALL 1
GLOBAL Out.Ln
CALL 0
RETURN
END

PROC tSignals07.Print 0 3 0x00100001
! PROCEDURE Print(p: Sig);
!   OutName(p); Out.String('[label="'); 
LDLW 12
GLOBAL tSignals07.OutName
CALL 1
CONST 9
GLOBAL tSignals07.%2
GLOBAL Out.String
CALL 2
!   Out.Char(p.op); Out.String(" ("); OutName(p);
LDLW 12
NCHECK 92
LOADC
ALIGNC
GLOBAL Out.Char
CALL 1
CONST 3
GLOBAL tSignals07.%3
GLOBAL Out.String
CALL 2
LDLW 12
GLOBAL tSignals07.OutName
CALL 1
!   Out.String(')"]'); Out.Ln;
CONST 4
GLOBAL tSignals07.%4
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
!   Out.String('# '); OutName(p); 
CONST 3
GLOBAL tSignals07.%5
GLOBAL Out.String
CALL 2
LDLW 12
GLOBAL tSignals07.OutName
CALL 1
!   Out.Char('['); OutBits(p.value); Out.String("] = ");  
CONST 91
ALIGNC
GLOBAL Out.Char
CALL 1
LDLW 12
NCHECK 95
LDNW 12
GLOBAL tSignals07.OutBits
CALL 1
CONST 5
GLOBAL tSignals07.%6
GLOBAL Out.String
CALL 2
!   CASE p.op OF
LDLW 12
NCHECK 96
LOADC
CONST 120
TESTGEQ L31
CONST 38
JEQ L29
JUMP L26
LABEL L31
CONST 120
MINUS
JCASE 7
CASEL L28
CASEL L28
CASEL L28
CASEL L26
CASEL L29
CASEL L26
CASEL L30
JUMP L26
LABEL L28
! 	Out.Char(p.op); Out.Ln
LDLW 12
NCHECK 98
LOADC
ALIGNC
GLOBAL Out.Char
CALL 1
GLOBAL Out.Ln
CALL 0
RETURN
LABEL L29
! 	OutName(p.left); Out.Char(' '); Out.Char(p.op); 
LDLW 12
NCHECK 100
LDNW 4
GLOBAL tSignals07.OutName
CALL 1
CONST 32
ALIGNC
GLOBAL Out.Char
CALL 1
LDLW 12
NCHECK 100
LOADC
ALIGNC
GLOBAL Out.Char
CALL 1
! 	Out.Char(' '); OutName(p.right); Out.Ln;
CONST 32
ALIGNC
GLOBAL Out.Char
CALL 1
LDLW 12
NCHECK 101
LDNW 8
GLOBAL tSignals07.OutName
CALL 1
GLOBAL Out.Ln
CALL 0
! 	OutArc(p.left, p); OutArc(p.right, p)
LDLW 12
LDLW 12
NCHECK 102
LDNW 4
GLOBAL tSignals07.OutArc
CALL 2
LDLW 12
LDLW 12
NCHECK 102
LDNW 8
GLOBAL tSignals07.OutArc
CALL 2
RETURN
LABEL L30
! 	Out.Char(p.op); Out.Char(' '); OutName(p.right); Out.Ln;
LDLW 12
NCHECK 104
LOADC
ALIGNC
GLOBAL Out.Char
CALL 1
CONST 32
ALIGNC
GLOBAL Out.Char
CALL 1
LDLW 12
NCHECK 104
LDNW 8
GLOBAL tSignals07.OutName
CALL 1
GLOBAL Out.Ln
CALL 0
! 	OutArc(p.right, p)
LDLW 12
LDLW 12
NCHECK 105
LDNW 8
GLOBAL tSignals07.OutArc
CALL 2
RETURN
LABEL L26
ERROR E_CASE 96
RETURN
END

PROC tSignals07.Mark 0 3 0x00100001
! PROCEDURE Mark(s: Sig);
!   IF (s # NIL) & ~s.marked THEN
LDLW 12
JEQZ L34
LDLW 12
NCHECK 111
CONST 20
LDIC
JNEQZ L34
!     s.marked := TRUE;
CONST 1
LDLW 12
NCHECK 112
CONST 20
STIC
!     Mark(s.left); Mark(s.right)
LDLW 12
NCHECK 113
LDNW 4
GLOBAL tSignals07.Mark
CALL 1
LDLW 12
NCHECK 113
LDNW 8
GLOBAL tSignals07.Mark
CALL 1
LABEL L34
RETURN
END

PROC tSignals07.PrintMarked 8 4 0
! PROCEDURE PrintMarked;
!   FOR i := 0 TO sp-1 DO
LDGW tSignals07.sp
DEC
STLW -8
CONST 0
STLW -4
LABEL L36
LDLW -4
LDLW -8
JGT L37
!     IF stack[i].marked THEN
GLOBAL tSignals07.stack
LDLW -4
CONST 1024
BOUND 121
LDIW
NCHECK 121
CONST 20
LDIC
JEQZ L40
!       stack[i].marked := FALSE;
CONST 0
GLOBAL tSignals07.stack
LDLW -4
CONST 1024
BOUND 122
LDIW
NCHECK 122
CONST 20
STIC
!       Print(stack[i])
GLOBAL tSignals07.stack
LDLW -4
CONST 1024
BOUND 123
LDIW
GLOBAL tSignals07.Print
CALL 1
LABEL L40
!   FOR i := 0 TO sp-1 DO
INCL -4
JUMP L36
LABEL L37
RETURN
END

PROC tSignals07.Check 12 3 0
! PROCEDURE Check;
!   notx := Bit.Xor(xxx, 0FFH); 
CONST 255
CONST 240
GLOBAL Bit.Xor
CALLW 2
STLW -4
!   noty := Bit.Xor(yyy, 0FFH);
CONST 255
CONST 204
GLOBAL Bit.Xor
CALLW 2
STLW -8
!   notz := Bit.Xor(zzz, 0FFH);
CONST 255
CONST 170
GLOBAL Bit.Xor
CALLW 2
STLW -12
!   IF (signal[notx] # NIL) & (signal[noty] # NIL) & (signal[notz] # NIL) THEN
GLOBAL tSignals07.signal
LDLW -4
CONST 256
BOUND 134
LDIW
JEQZ L43
GLOBAL tSignals07.signal
LDLW -8
CONST 256
BOUND 134
LDIW
JEQZ L43
GLOBAL tSignals07.signal
LDLW -12
CONST 256
BOUND 134
LDIW
JEQZ L43
!      Mark(signal[notx]); Mark(signal[noty]); Mark(signal[notz]);
GLOBAL tSignals07.signal
LDLW -4
CONST 256
BOUND 135
LDIW
GLOBAL tSignals07.Mark
CALL 1
GLOBAL tSignals07.signal
LDLW -8
CONST 256
BOUND 135
LDIW
GLOBAL tSignals07.Mark
CALL 1
GLOBAL tSignals07.signal
LDLW -12
CONST 256
BOUND 135
LDIW
GLOBAL tSignals07.Mark
CALL 1
!      Out.String("digraph Circuit {"); Out.Ln;
CONST 18
GLOBAL tSignals07.%7
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
!      PrintMarked;
GLOBAL tSignals07.PrintMarked
CALL 0
!      Out.String("}"); Out.Ln
CONST 2
GLOBAL tSignals07.%8
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
LABEL L43
RETURN
END

PROC tSignals07.Search 24 5 0
! PROCEDURE Search;
!   MakeSig('x', NIL, NIL, xxx);
CONST 240
CONST 0
CONST 0
CONST 120
ALIGNC
GLOBAL tSignals07.MakeSig
CALL 4
!   MakeSig('y', NIL, NIL, yyy);
CONST 204
CONST 0
CONST 0
CONST 121
ALIGNC
GLOBAL tSignals07.MakeSig
CALL 4
!   MakeSig('z', NIL, NIL, zzz);
CONST 170
CONST 0
CONST 0
CONST 122
ALIGNC
GLOBAL tSignals07.MakeSig
CALL 4
!   Closure(0);
CONST 0
GLOBAL tSignals07.Closure
CALL 1
!   u := sp;
LDGW tSignals07.sp
STLW -12
!   FOR i := 0 TO u-1 DO
LDLW -12
DEC
STLW -20
CONST 0
STLW -4
LABEL L46
LDLW -4
LDLW -20
JGT L47
!     IF Negate(stack[i]) THEN
GLOBAL tSignals07.stack
LDLW -4
CONST 1024
BOUND 151
LDIW
GLOBAL tSignals07.Negate
CALLW 1
JEQZ L50
!       Closure(sp-1);
LDGW tSignals07.sp
DEC
GLOBAL tSignals07.Closure
CALL 1
!       v := sp;
LDGW tSignals07.sp
STLW -16
!       FOR j := i+1 TO v-1 DO
LDLW -16
DEC
STLW -24
LDLW -4
INC
STLW -8
LABEL L51
LDLW -8
LDLW -24
JGT L52
!         IF Negate(stack[j]) THEN
GLOBAL tSignals07.stack
LDLW -8
CONST 1024
BOUND 155
LDIW
GLOBAL tSignals07.Negate
CALLW 1
JEQZ L55
!           Closure(sp-1);
LDGW tSignals07.sp
DEC
GLOBAL tSignals07.Closure
CALL 1
! 	  Check;
GLOBAL tSignals07.Check
CALL 0
! 	  Pop(v)
LDLW -16
GLOBAL tSignals07.Pop
CALL 1
LABEL L55
!       FOR j := i+1 TO v-1 DO
INCL -8
JUMP L51
LABEL L52
!       Pop(u)
LDLW -12
GLOBAL tSignals07.Pop
CALL 1
LABEL L50
!   FOR i := 0 TO u-1 DO
INCL -4
JUMP L46
LABEL L47
RETURN
END

PROC tSignals07.%main 0 2 0
!   sp := 0;
CONST 0
STGW tSignals07.sp
!   Search
GLOBAL tSignals07.Search
CALL 0
RETURN
END

! Global variables
GLOVAR tSignals07.signal 1024
GLOVAR tSignals07.stack 4096
GLOVAR tSignals07.sp 4

! Global pointer map
DEFINE tSignals07.%gcmap
WORD GC_BASE
WORD tSignals07.signal
WORD GC_BLOCK
WORD 0
WORD 256
WORD GC_BASE
WORD tSignals07.stack
WORD GC_BLOCK
WORD 0
WORD 1024
WORD GC_END

! String " -> "
DEFINE tSignals07.%1
STRING 202D3E2000

! String "[label=\""
DEFINE tSignals07.%2
STRING 5B6C6162656C3D2200

! String " ("
DEFINE tSignals07.%3
STRING 202800

! String ")\"]"
DEFINE tSignals07.%4
STRING 29225D00

! String "# "
DEFINE tSignals07.%5
STRING 232000

! String "] = "
DEFINE tSignals07.%6
STRING 5D203D2000

! String "digraph Circuit {"
DEFINE tSignals07.%7
STRING 646967726170682043697263756974207B00

! String "}"
DEFINE tSignals07.%8
STRING 7D00

! Descriptor for Blob
DEFINE tSignals07.Blob
WORD 0x0000000d
WORD 0
WORD tSignals07.Blob.%anc

DEFINE tSignals07.Blob.%anc
WORD tSignals07.Blob

! End of file
]]*)
