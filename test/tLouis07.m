MODULE tLouis07;

(* Issue #20: use of qualfied names as labels in typecase *)

IMPORT xTypes07, Out;

PROCEDURE b(x: xTypes07.Base);
BEGIN
  CASE x OF
      xTypes07.Ext1:
        Out.String("One "); Out.Int(x.stuff1, 0); Out.Ln
    | xTypes07.Ext2:
        Out.String("Two "); Out.Real(x.stuff2); Out.Ln
  END
END b;

PROCEDURE test;
  VAR p1: xTypes07.Ext1; p2: xTypes07.Ext2;
BEGIN
  NEW(p1); p1.stuff1 := 3; b(p1);
  NEW(p2); p2.stuff2 := 3.14; b(p2)
END test;

BEGIN
  test
END tLouis07.

(*<<
One 3
Two 3.14000
>>*)

(*[[
!! (SYMFILE #tLouis07 STAMP #tLouis07.%main 1 #tLouis07.m)
!! (CHKSUM STAMP)
!! 
MODULE tLouis07 STAMP 0
IMPORT xTypes07 STAMP
IMPORT Out STAMP
ENDHDR

PROC tLouis07.b 0 4 0x00100001
! PROCEDURE b(x: xTypes07.Base);
!   CASE x OF
LDLW 12
NCHECK 9
CONST -4
OFFSET
DUP 0
LOADW
DUP 0
LDNW 4
CONST 1
JGEQ L6
POP 1
JUMP L4
LABEL L6
LDNW 8
LDNW 4
GLOBAL xTypes07.Ext1Cell
JNEQ L4
POP 1
!         Out.String("One "); Out.Int(x.stuff1, 0); Out.Ln
CONST 5
GLOBAL tLouis07.%1
GLOBAL Out.String
CALL 2
CONST 0
LDLW 12
NCHECK 11
LOADW
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
LABEL L4
DUP 0
LOADW
DUP 0
LDNW 4
CONST 1
JGEQ L9
POP 1
JUMP L7
LABEL L9
LDNW 8
LDNW 4
GLOBAL xTypes07.Ext2Cell
JNEQ L7
POP 1
!         Out.String("Two "); Out.Real(x.stuff2); Out.Ln
CONST 5
GLOBAL tLouis07.%2
GLOBAL Out.String
CALL 2
LDLW 12
NCHECK 13
LOADF
GLOBAL Out.Real
CALL 1
GLOBAL Out.Ln
CALL 0
RETURN
LABEL L7
POP 1
ERROR E_CASE 9
RETURN
END

PROC tLouis07.test 8 3 0x00018001
! PROCEDURE test;
!   NEW(p1); p1.stuff1 := 3; b(p1);
CONST 4
GLOBAL xTypes07.Ext1Cell
GLOBAL NEW
CALLW 2
STLW -4
CONST 3
LDLW -4
NCHECK 20
STOREW
LDLW -4
GLOBAL tLouis07.b
CALL 1
!   NEW(p2); p2.stuff2 := 3.14; b(p2)
CONST 4
GLOBAL xTypes07.Ext2Cell
GLOBAL NEW
CALLW 2
STLW -8
FCONST 3.14
LDLW -8
NCHECK 21
STOREF
LDLW -8
GLOBAL tLouis07.b
CALL 1
RETURN
END

PROC tLouis07.%main 0 1 0
!   test
GLOBAL tLouis07.test
CALL 0
RETURN
END

! String "One "
DEFINE tLouis07.%1
STRING 4F6E652000

! String "Two "
DEFINE tLouis07.%2
STRING 54776F2000

! End of file
]]*)

(*{{
JIT: tLouis07.b
---   push rBP
---   push rBX
---   subq rSP, #8
--- GETARG V1, 0
---   mov rBP, rDI
--- BGEu V1, <addr>, L7
---   cmp rBP, #<addr>
---   jae L7
--- L1:
--- PREP 1
--- ARG V1
--- CALL <addr>
---   mov rDI, rBP
---   call *<addr>
--- L7:
--- MOV I0, 0
---   xor rAX, rAX
0: LDLW 12
+ LOCAL 12
+ LOADW
1: NCHECK 9
--- LDW I0, V1, 12
---   mov rAX, 12(rBP)
--- BEQ I0, 0, L8
---   test rAX, rAX
---   je L8
4: PUSH -4
6: INDEXC
+ PLUSA
7: DUP 0
8: LOADW
9: DUP 0
10: LDNW 4
+ PUSH 4
+ PLUSA
--- LDW I1, I0, -4
---   mov rCX, -4(rAX)
+ LOADW
11: PUSH 1
12: JGE 18
--- ADD I2, I0, -4
---   mov rDX, rAX
---   add rDX, #-4
--- STW I2, V1, -4
---   mov -4(rBP), rDX
--- STW I1, V1, -8
---   mov -8(rBP), rCX
--- LDW I3, I1, 4
---   mov rSI, 4(rCX)
--- BGE I3, 1, L3
---   cmp rSI, #1
---   jge L3
14: POP 1
16: JUMP 51
--- JUMP L4
---   jmp L4
18: LDNW 8
[Restore 2]
--- L3:
+ PUSH 8
+ OFFSET
--- LDW I0, V1, -8
---   mov rAX, -8(rBP)
+ LOADW
19: LDNW 4
+ PUSH 4
+ OFFSET
--- LDW I0, I0, 8
---   mov rAX, 8(rAX)
+ LOADW
20: LDKW 0
22: JNE 51
--- LDW I0, I0, 4
---   mov rAX, 4(rAX)
--- BNE I0, <addr>, L4
---   cmp rAX, #<addr>
---   jne L4
24: POP 1
26: PUSH 5
27: LDKW 1
29: LDKW 2
31: JPROC
32: SLIDE 2
--- LDKW I0, <addr>
---   mov rAX, #<addr>
--- MOV I1, 5
---   mov rCX, #5
--- STW I1, V1, -4
---   mov -4(rBP), rCX
--- LDKW I2, <addr>
---   mov rDX, #<addr>
--- STW I2, V1, -8
---   mov -8(rBP), rDX
--- STW I0, V1, -12
---   mov -12(rBP), rAX
--- MOV I3, 0
---   xor rSI, rSI
--- STW I3, V1, -16
---   mov -16(rBP), rSI
--- STW V1, V1, -20
---   mov -20(rBP), rBP
--- LDW I0, I0, 0
---   mov rAX, (rAX)
--- SUB I1, V1, 20
---   mov rCX, rBP
---   sub rCX, #20
--- PREP 1
--- ARG I1
--- CALL I0
---   mov rDI, rCX
---   call *(rAX)
34: PUSH 0
35: LDLW 12
+ LOCAL 12
+ LOADW
36: NCHECK 11
--- LDW I0, V1, 12
---   mov rAX, 12(rBP)
--- BEQ I0, 0, L9
---   test rAX, rAX
---   je L9
39: LOADW
40: LDKW 3
42: JPROC
43: SLIDE 2
--- LDKW I1, <addr>
---   mov rCX, #<addr>
--- MOV I2, 0
---   xor rDX, rDX
--- STW I2, V1, -4
---   mov -4(rBP), rDX
--- LDW I3, I0, 0
---   mov rSI, (rAX)
--- STW I3, V1, -8
---   mov -8(rBP), rSI
--- STW I1, V1, -12
---   mov -12(rBP), rCX
--- STW I2, V1, -16
---   mov -16(rBP), rDX
--- STW V1, V1, -20
---   mov -20(rBP), rBP
--- LDW I1, I1, 0
---   mov rCX, (rCX)
--- SUB I0, V1, 20
---   mov rAX, rBP
---   sub rAX, #20
--- PREP 1
--- ARG I0
--- CALL I1
---   mov rDI, rAX
---   call *(rCX)
45: LDKW 4
47: JPROC
48: SLIDE 0
--- LDKW I0, <addr>
---   mov rAX, #<addr>
--- STW I0, V1, -4
---   mov -4(rBP), rAX
--- MOV I1, 0
---   xor rCX, rCX
--- STW I1, V1, -8
---   mov -8(rBP), rCX
--- STW V1, V1, -12
---   mov -12(rBP), rBP
--- LDW I0, I0, 0
---   mov rAX, (rAX)
--- SUB I1, V1, 12
---   mov rCX, rBP
---   sub rCX, #12
--- PREP 1
--- ARG I1
--- CALL I0
---   mov rDI, rCX
---   call *(rAX)
50: RETURN
--- JUMP L2
---   jmp L2
51: DUP 0
[Restore 1]
--- L4:
--- LDW I0, V1, -4
---   mov rAX, -4(rBP)
52: LOADW
53: DUP 0
54: LDNW 4
+ PUSH 4
+ OFFSET
--- LDW I1, I0, 0
---   mov rCX, (rAX)
+ LOADW
55: PUSH 1
56: JGE 62
--- STW I0, V1, -4
---   mov -4(rBP), rAX
--- STW I1, V1, -8
---   mov -8(rBP), rCX
--- LDW I0, I1, 4
---   mov rAX, 4(rCX)
--- BGE I0, 1, L5
---   cmp rAX, #1
---   jge L5
58: POP 1
60: JUMP 94
--- JUMP L6
---   jmp L6
62: LDNW 8
[Restore 2]
--- L5:
+ PUSH 8
+ OFFSET
--- LDW I0, V1, -8
---   mov rAX, -8(rBP)
+ LOADW
63: LDNW 4
+ PUSH 4
+ OFFSET
--- LDW I0, I0, 8
---   mov rAX, 8(rAX)
+ LOADW
64: LDKW 5
66: JNE 94
--- LDW I0, I0, 4
---   mov rAX, 4(rAX)
--- BNE I0, <addr>, L6
---   cmp rAX, #<addr>
---   jne L6
68: POP 1
70: PUSH 5
71: LDKW 6
73: LDKW 2
75: JPROC
76: SLIDE 2
--- LDKW I0, <addr>
---   mov rAX, #<addr>
--- MOV I1, 5
---   mov rCX, #5
--- STW I1, V1, -4
---   mov -4(rBP), rCX
--- LDKW I2, <addr>
---   mov rDX, #<addr>
--- STW I2, V1, -8
---   mov -8(rBP), rDX
--- STW I0, V1, -12
---   mov -12(rBP), rAX
--- MOV I3, 0
---   xor rSI, rSI
--- STW I3, V1, -16
---   mov -16(rBP), rSI
--- STW V1, V1, -20
---   mov -20(rBP), rBP
--- LDW I0, I0, 0
---   mov rAX, (rAX)
--- SUB I1, V1, 20
---   mov rCX, rBP
---   sub rCX, #20
--- PREP 1
--- ARG I1
--- CALL I0
---   mov rDI, rCX
---   call *(rAX)
78: LDLW 12
+ LOCAL 12
+ LOADW
79: NCHECK 13
--- LDW I0, V1, 12
---   mov rAX, 12(rBP)
--- BEQ I0, 0, L10
---   test rAX, rAX
---   je L10
82: LOADF
83: LDKW 7
85: JPROC
86: SLIDE 1
--- LDKW I1, <addr>
---   mov rCX, #<addr>
--- LDW F0, I0, 0
---   flds (rAX)
---   fstp rF1
--- STW F0, V1, -4
---   fsts -4(rBP)
--- STW I1, V1, -8
---   mov -8(rBP), rCX
--- MOV I2, 0
---   xor rDX, rDX
--- STW I2, V1, -12
---   mov -12(rBP), rDX
--- STW V1, V1, -16
---   mov -16(rBP), rBP
--- LDW I1, I1, 0
---   mov rCX, (rCX)
--- SUB I0, V1, 16
---   mov rAX, rBP
---   sub rAX, #16
--- PREP 1
--- ARG I0
--- CALL I1
---   mov rDI, rAX
---   call *(rCX)
88: LDKW 4
90: JPROC
91: SLIDE 0
--- LDKW I0, <addr>
---   mov rAX, #<addr>
--- STW I0, V1, -4
---   mov -4(rBP), rAX
--- MOV I1, 0
---   xor rCX, rCX
--- STW I1, V1, -8
---   mov -8(rBP), rCX
--- STW V1, V1, -12
---   mov -12(rBP), rBP
--- LDW I0, I0, 0
---   mov rAX, (rAX)
--- SUB I1, V1, 12
---   mov rCX, rBP
---   sub rCX, #12
--- PREP 1
--- ARG I1
--- CALL I0
---   mov rDI, rCX
---   call *(rAX)
93: RETURN
--- JUMP L2
---   jmp L2
94: POP 1
[Restore 1]
--- L6:
96: ERROR 3 9
--- JUMP L11
---   jmp L11
100: RETURN
--- L2:
--- RET
---   addq rSP, #8
---   pop rBX
---   pop rBP
---   ret
--- L11:
--- PREP 3
--- ARG V1
--- ARG 9
--- ARG 3
--- CALL <addr>
---   mov rDX, rBP
---   mov rDI, #3
---   mov rSI, #9
---   call *<addr>
--- L10:
--- PREP 3
--- ARG V1
--- ARG 13
--- ARG 8
--- CALL <addr>
---   mov rDX, rBP
---   mov rDI, #8
---   mov rSI, #13
---   call *<addr>
--- L9:
--- PREP 3
--- ARG V1
--- ARG 11
--- ARG 8
--- CALL <addr>
---   mov rDX, rBP
---   mov rDI, #8
---   mov rSI, #11
---   call *<addr>
--- L8:
--- PREP 3
--- ARG V1
--- ARG 9
--- ARG 8
--- CALL <addr>
---   mov rDX, rBP
---   mov rDI, #8
---   mov rSI, #9
---   call *<addr>
JIT: tLouis07.test
---   push rBP
---   push rBX
---   subq rSP, #8
--- GETARG V1, 0
---   mov rBP, rDI
--- BGEu V1, <addr>, L14
---   cmp rBP, #<addr>
---   jae L14
--- L12:
--- PREP 1
--- ARG V1
--- CALL <addr>
---   mov rDI, rBP
---   call *<addr>
--- L14:
--- MOV I0, 0
---   xor rAX, rAX
--- STW I0, V1, -8
---   mov -8(rBP), rAX
--- STW I0, V1, -4
---   mov -4(rBP), rAX
0: PUSH 4
1: LDKW 0
3: LDKW 1
5: JPROC
6: SLIDEW 2
--- LDKW I0, <addr>
---   mov rAX, #<addr>
--- MOV I1, 4
---   mov rCX, #4
--- STW I1, V1, -12
---   mov -12(rBP), rCX
--- LDKW I2, <addr>
---   mov rDX, #<addr>
--- STW I2, V1, -16
---   mov -16(rBP), rDX
--- STW I0, V1, -20
---   mov -20(rBP), rAX
--- MOV I3, 0
---   xor rSI, rSI
--- STW I3, V1, -24
---   mov -24(rBP), rSI
--- STW V1, V1, -28
---   mov -28(rBP), rBP
--- LDW I0, I0, 0
---   mov rAX, (rAX)
--- SUB I1, V1, 28
---   mov rCX, rBP
---   sub rCX, #28
--- PREP 1
--- ARG I1
--- CALL I0
---   mov rDI, rCX
---   call *(rAX)
8: STLW -4
+ LOCAL -4
+ STOREW
--- LDW I0, <addr>
---   mov rAX, <addr>
--- STW I0, V1, -4
---   mov -4(rBP), rAX
9: PUSH 3
10: LDLW -4
+ LOCAL -4
+ LOADW
11: NCHECK 20
--- BEQ I0, 0, L15
---   test rAX, rAX
---   je L15
14: STOREW
--- MOV I1, 3
---   mov rCX, #3
--- STW I1, I0, 0
---   mov (rAX), rCX
15: LDLW -4
+ LOCAL -4
+ LOADW
16: LDKW 2
18: JPROC
19: SLIDE 1
--- LDKW I0, <addr>
---   mov rAX, #<addr>
--- LDW I1, V1, -4
---   mov rCX, -4(rBP)
--- STW I1, V1, -12
---   mov -12(rBP), rCX
--- STW I0, V1, -16
---   mov -16(rBP), rAX
--- MOV I2, 0
---   xor rDX, rDX
--- STW I2, V1, -20
---   mov -20(rBP), rDX
--- STW V1, V1, -24
---   mov -24(rBP), rBP
--- LDW I0, I0, 0
---   mov rAX, (rAX)
--- SUB I1, V1, 24
---   mov rCX, rBP
---   sub rCX, #24
--- PREP 1
--- ARG I1
--- CALL I0
---   mov rDI, rCX
---   call *(rAX)
21: PUSH 4
22: LDKW 3
24: LDKW 1
26: JPROC
27: SLIDEW 2
--- LDKW I0, <addr>
---   mov rAX, #<addr>
--- MOV I1, 4
---   mov rCX, #4
--- STW I1, V1, -12
---   mov -12(rBP), rCX
--- LDKW I2, <addr>
---   mov rDX, #<addr>
--- STW I2, V1, -16
---   mov -16(rBP), rDX
--- STW I0, V1, -20
---   mov -20(rBP), rAX
--- MOV I3, 0
---   xor rSI, rSI
--- STW I3, V1, -24
---   mov -24(rBP), rSI
--- STW V1, V1, -28
---   mov -28(rBP), rBP
--- LDW I0, I0, 0
---   mov rAX, (rAX)
--- SUB I1, V1, 28
---   mov rCX, rBP
---   sub rCX, #28
--- PREP 1
--- ARG I1
--- CALL I0
---   mov rDI, rCX
---   call *(rAX)
29: STLW -8
+ LOCAL -8
+ STOREW
--- LDW I0, <addr>
---   mov rAX, <addr>
--- STW I0, V1, -8
---   mov -8(rBP), rAX
30: LDKF 4
32: LDLW -8
+ LOCAL -8
+ LOADW
33: NCHECK 21
--- BEQ I0, 0, L16
---   test rAX, rAX
---   je L16
36: STOREF
--- LDKW F0, <addr>
---   flds <addr>
---   fstp rF1
--- STW F0, I0, 0
---   fsts (rAX)
37: LDLW -8
+ LOCAL -8
+ LOADW
38: LDKW 2
40: JPROC
41: SLIDE 1
--- LDKW I0, <addr>
---   mov rAX, #<addr>
--- LDW I1, V1, -8
---   mov rCX, -8(rBP)
--- STW I1, V1, -12
---   mov -12(rBP), rCX
--- STW I0, V1, -16
---   mov -16(rBP), rAX
--- MOV I2, 0
---   xor rDX, rDX
--- STW I2, V1, -20
---   mov -20(rBP), rDX
--- STW V1, V1, -24
---   mov -24(rBP), rBP
--- LDW I0, I0, 0
---   mov rAX, (rAX)
--- SUB I1, V1, 24
---   mov rCX, rBP
---   sub rCX, #24
--- PREP 1
--- ARG I1
--- CALL I0
---   mov rDI, rCX
---   call *(rAX)
43: RETURN
--- L13:
--- RET
---   addq rSP, #8
---   pop rBX
---   pop rBP
---   ret
--- L16:
--- PREP 3
--- ARG V1
--- ARG 21
--- ARG 8
--- CALL <addr>
---   mov rDX, rBP
---   mov rDI, #8
---   mov rSI, #21
---   call *<addr>
--- L15:
--- PREP 3
--- ARG V1
--- ARG 20
--- ARG 8
--- CALL <addr>
---   mov rDX, rBP
---   mov rDI, #8
---   mov rSI, #20
---   call *<addr>
JIT: tLouis07.%main
---   push rBP
---   push rBX
---   subq rSP, #8
--- GETARG V1, 0
---   mov rBP, rDI
--- BGEu V1, <addr>, L19
---   cmp rBP, #<addr>
---   jae L19
--- L17:
--- PREP 1
--- ARG V1
--- CALL <addr>
---   mov rDI, rBP
---   call *<addr>
--- L19:
0: LDKW 0
2: JPROC
3: SLIDE 0
--- LDKW I0, <addr>
---   mov rAX, #<addr>
--- STW I0, V1, -4
---   mov -4(rBP), rAX
--- MOV I1, 0
---   xor rCX, rCX
--- STW I1, V1, -8
---   mov -8(rBP), rCX
--- STW V1, V1, -12
---   mov -12(rBP), rBP
--- LDW I0, I0, 0
---   mov rAX, (rAX)
--- SUB I1, V1, 12
---   mov rCX, rBP
---   sub rCX, #12
--- PREP 1
--- ARG I1
--- CALL I0
---   mov rDI, rCX
---   call *(rAX)
5: RETURN
--- L18:
--- RET
---   addq rSP, #8
---   pop rBX
---   pop rBP
---   ret
}}*)
