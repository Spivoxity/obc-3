MODULE tMerge;

(*<<
 37 98 59 20 81 42  3 64 25 86 47  8 69 30 91 52 13 74 35 96 57 18 79 40  1
  1  3  8 13 18 20 25 30 35 37 40 42 47 52 57 59 64 69 74 79 81 86 91 96 98
>>*)

IMPORT Out;

(* Mergesort using iterators *)

TYPE stream = POINTER TO streamrec;
  streamrec = ABSTRACT RECORD END;

ABSTRACT PROCEDURE (s: stream) Done(): BOOLEAN;
ABSTRACT PROCEDURE (s: stream) Current(): INTEGER;
ABSTRACT PROCEDURE (s: stream) Next(k: INTEGER);
ABSTRACT PROCEDURE (s: stream) Copy(): stream;

(* Array streams *)

TYPE arrayptr = POINTER TO ARRAY OF INTEGER;
  arraystream = POINTER TO arraystreamrec;
  arraystreamrec = 
    RECORD (streamrec)
      a: arrayptr;
      i: INTEGER;
    END;

PROCEDURE NewArrayStream(a: arrayptr): arraystream;
  VAR s: arraystream;
BEGIN
  NEW(s);
  s.a := a;
  s.i := 0;
  RETURN s
END NewArrayStream;

PROCEDURE (s: arraystream) Done(): BOOLEAN;
BEGIN
  RETURN s.i >= LEN(s.a^)
END Done;

PROCEDURE (s: arraystream) Current(): INTEGER;
BEGIN
  RETURN s.a[s.i]
END Current;

PROCEDURE (s: arraystream) Next(k: INTEGER);
BEGIN
  s.i := s.i + k
END Next;

PROCEDURE (s: arraystream) Copy(): stream;
  VAR t: arraystream;
BEGIN
  NEW(t);
  t.a := s.a;
  t.i := s.i;
  RETURN t
END Copy;

(* halfstream *)

TYPE halfstream = POINTER TO halfstreamrec;
  halfstreamrec = 
    RECORD (streamrec)
      base: stream;
      inc: INTEGER;
    END;

PROCEDURE NewHalfStream(base: stream; odd: BOOLEAN): halfstream;
  VAR s: halfstream;
BEGIN
  NEW(s);
  WITH base: halfstream DO
    s.base := base.base;
    s.inc := 2 * base.inc;
  ELSE
    s.base := base;
    s.inc := 1
  END;
  IF odd THEN s.base.Next(s.inc) END;
  RETURN s;
END NewHalfStream;

PROCEDURE (s: halfstream) Done(): BOOLEAN; 
BEGIN RETURN s.base.Done() END Done;

PROCEDURE (s: halfstream) Current(): INTEGER;
BEGIN RETURN s.base.Current() END Current;

PROCEDURE (s: halfstream) Next(k: INTEGER);
BEGIN
  s.base.Next(2*k*s.inc)
END Next;

PROCEDURE (s: halfstream) Copy(): stream;
  VAR r: halfstream;
BEGIN
  NEW(r);
  r.base := s.base.Copy();
  r.inc := s.inc;
  RETURN r
END Copy;  

(* mergestream *)

TYPE mergestream = POINTER TO mergestreamrec;
  mergestreamrec = 
    RECORD (streamrec)
      p, q: stream;
      known: BOOLEAN;
      done, isleft: BOOLEAN;
      current: INTEGER;
    END;

PROCEDURE NewMergeStream*(p, q: stream): mergestream;
  VAR s: mergestream;
BEGIN
  NEW(s);
  s.p := p;
  s.q := q;
  s.known := FALSE;
  RETURN s
END NewMergeStream;

PROCEDURE (s: mergestream) Fix();
  VAR lx, rx: BOOLEAN;
    lv, rv: INTEGER;
BEGIN
  lv := 0; rv := 0;
  IF ~s.known THEN
    lx := s.p.Done(); rx := s.q.Done();
    s.done := lx & rx;
    IF ~s.done THEN
      IF ~lx THEN lv := s.p.Current() END; 
      IF ~rx THEN rv := s.q.Current() END;
      s.isleft := ~lx & (rx OR (lv <= rv));
      IF s.isleft THEN s.current := lv ELSE s.current := rv END;
      s.known := TRUE
    END
  END
END Fix;

PROCEDURE (s: mergestream) Done(): BOOLEAN;
BEGIN s.Fix(); RETURN s.done END Done;

PROCEDURE (s: mergestream) Current(): INTEGER;
BEGIN
  s.Fix();
  RETURN s.current
END Current;

PROCEDURE (s: mergestream) Next(k: INTEGER);
  VAR i: INTEGER;
BEGIN
  i := 0;
  WHILE i < k DO
    s.Fix();
    IF s.isleft THEN s.p.Next(1) ELSE s.q.Next(1) END;
    s.known := FALSE;
    i := i+1
  END
END Next;

PROCEDURE (s: mergestream) Copy(): stream;
BEGIN
  RETURN NewMergeStream(s.p.Copy(), s.q.Copy())
END Copy;

PROCEDURE sort(s: stream): stream;
  VAR p, q: stream;
BEGIN
  p := NewHalfStream(s.Copy(), FALSE);
  q := NewHalfStream(s.Copy(), TRUE);
  IF q.Done() THEN
    RETURN p
  ELSE
    RETURN NewMergeStream(sort(p), sort(q))
  END
END sort;

VAR 
  a: arrayptr; s: stream; k, n, m: INTEGER;

BEGIN
  (* This is not very random ... *)
  n := 25;
  NEW(a, n);
  m := 37;
  FOR k := 0 TO n-1 DO 
    a[k] := m;
    m := (m + 61) MOD 100;
    Out.Int(a[k], 3)
  END;
  Out.Ln;

  s := sort(NewArrayStream(a));
  WHILE ~s.Done() DO
    k := s.Current();
    Out.Int(k, 3);
    s.Next(1)
  END;
  Out.Ln
END tMerge.

(*[[
!! SYMFILE #tMerge STAMP #tMerge.%main 1
!! DEF !0 POINTER
!! DEF !1 POINTER
!! PROCEDURE #NewMergeStream* 118 #tMerge.NewMergeStream !2 PROC 2 1
!!   PARAM #p 12 0
!!   PARAM #q 16 0;;
!! DEF !3 RECORD #tMerge.streamrec 0 VOID;
!! TARGET 1 !4 RECORD #tMerge.mergestreamrec 16 3
!!   FIELD #p 0 0
!!   FIELD #q 4 0
!!   FIELD #known 8 BOOLEAN
!!   FIELD #done 9 BOOLEAN
!!   FIELD #isleft 10 BOOLEAN
!!   FIELD #current 12 INTEGER;
!! TARGET 0 3
!! METHOD 3 #Done 15 0 #tMerge.streamrec.Done !5 ABSMETH 1 BOOLEAN
!!   PARAM #s 12 0;;
!! METHOD 3 #Current 16 1 #tMerge.streamrec.Current !6 ABSMETH 1 INTEGER
!!   PARAM #s 12 0;;
!! METHOD 3 #Next 17 2 #tMerge.streamrec.Next !7 ABSMETH 2 VOID
!!   PARAM #s 12 0
!!   PARAM #k 16 INTEGER;;
!! METHOD 3 #Copy 18 3 #tMerge.streamrec.Copy !8 ABSMETH 1 0
!!   PARAM #s 12 0;;
!! METHOD 4 #Done 146 0 #tMerge.mergestreamrec.Done !9 METH 1 BOOLEAN
!!   PARAM #s 12 1;;
!! METHOD 4 #Current 149 1 #tMerge.mergestreamrec.Current !10 METH 1 INTEGER
!!   PARAM #s 12 1;;
!! METHOD 4 #Next 155 2 #tMerge.mergestreamrec.Next !11 METH 2 VOID
!!   PARAM #s 12 1
!!   PARAM #k 16 INTEGER;;
!! METHOD 4 #Copy 167 3 #tMerge.mergestreamrec.Copy !12 METH 1 0
!!   PARAM #s 12 1;;
!! METHOD 4 #Fix 128 4 #tMerge.mergestreamrec.Fix !13 METH 1 VOID
!!   PARAM #s 12 1;;
!! END STAMP
!! 
MODULE tMerge STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tMerge.NewArrayStream 4 4 0x00110001
! PROCEDURE NewArrayStream(a: arrayptr): arraystream;
!   NEW(s);
CONST 8
GLOBAL tMerge.arraystreamrec
LOCAL -4
GLOBAL NEW
CALL 3
!   s.a := a;
LDLW 12
LDLW -4
NCHECK 34
STOREW
!   s.i := 0;
CONST 0
LDLW -4
NCHECK 35
STNW 4
!   RETURN s
LDLW -4
RETURNW
END

PROC tMerge.arraystreamrec.Done 0 4 0x00100001
! PROCEDURE (s: arraystream) Done(): BOOLEAN;
!   RETURN s.i >= LEN(s.a^)
LDLW 12
NCHECK 41
LDNW 4
LDLW 12
NCHECK 41
LOADW
NCHECK 41
LDNW -4
LDNW 4
GEQ
RETURNW
END

PROC tMerge.arraystreamrec.Current 0 4 0x00100001
! PROCEDURE (s: arraystream) Current(): INTEGER;
!   RETURN s.a[s.i]
LDLW 12
NCHECK 46
LOADW
NCHECK 46
LDLW 12
NCHECK 46
LDNW 4
DUP 1
LDNW -4
LDNW 4
BOUND 46
LDIW
RETURNW
END

PROC tMerge.arraystreamrec.Next 0 4 0x00100001
! PROCEDURE (s: arraystream) Next(k: INTEGER);
!   s.i := s.i + k
LDLW 12
NCHECK 51
LDNW 4
LDLW 16
PLUS
LDLW 12
NCHECK 51
STNW 4
RETURN
END

PROC tMerge.arraystreamrec.Copy 4 4 0x00110001
! PROCEDURE (s: arraystream) Copy(): stream;
!   NEW(t);
CONST 8
GLOBAL tMerge.arraystreamrec
LOCAL -4
GLOBAL NEW
CALL 3
!   t.a := s.a;
LDLW 12
NCHECK 58
LOADW
LDLW -4
NCHECK 58
STOREW
!   t.i := s.i;
LDLW 12
NCHECK 59
LDNW 4
LDLW -4
NCHECK 59
STNW 4
!   RETURN t
LDLW -4
RETURNW
END

PROC tMerge.NewHalfStream 4 4 0x00110001
! PROCEDURE NewHalfStream(base: stream; odd: BOOLEAN): halfstream;
!   NEW(s);
CONST 8
GLOBAL tMerge.halfstreamrec
LOCAL -4
GLOBAL NEW
CALL 3
!   WITH base: halfstream DO
LDLW 12
NCHECK 76
LDNW -4
GLOBAL tMerge.halfstreamrec
TYPETEST 1
JUMPF 2
!     s.base := base.base;
LDLW 12
NCHECK 77
LOADW
LDLW -4
NCHECK 77
STOREW
!     s.inc := 2 * base.inc;
LDLW 12
NCHECK 78
LDNW 4
CONST 2
TIMES
LDLW -4
NCHECK 78
STNW 4
JUMP 1
LABEL 2
!     s.base := base;
LDLW 12
LDLW -4
NCHECK 80
STOREW
!     s.inc := 1
CONST 1
LDLW -4
NCHECK 81
STNW 4
LABEL 1
!   IF odd THEN s.base.Next(s.inc) END;
LDLC 16
JUMPF 5
LDLW -4
NCHECK 83
LDNW 4
LDLW -4
NCHECK 83
LOADW
NCHECK 83
DUP 0
LDNW -4
LDNW 20
CALL 2
LABEL 5
!   RETURN s;
LDLW -4
RETURNW
END

PROC tMerge.halfstreamrec.Done 0 4 0x00100001
! PROCEDURE (s: halfstream) Done(): BOOLEAN; 
! BEGIN RETURN s.base.Done() END Done;
LDLW 12
NCHECK 88
LOADW
NCHECK 88
DUP 0
LDNW -4
LDNW 12
CALLW 1
RETURNW
END

PROC tMerge.halfstreamrec.Current 0 4 0x00100001
! PROCEDURE (s: halfstream) Current(): INTEGER;
! BEGIN RETURN s.base.Current() END Current;
LDLW 12
NCHECK 91
LOADW
NCHECK 91
DUP 0
LDNW -4
LDNW 16
CALLW 1
RETURNW
END

PROC tMerge.halfstreamrec.Next 0 4 0x00100001
! PROCEDURE (s: halfstream) Next(k: INTEGER);
!   s.base.Next(2*k*s.inc)
LDLW 16
CONST 2
TIMES
LDLW 12
NCHECK 95
LDNW 4
TIMES
LDLW 12
NCHECK 95
LOADW
NCHECK 95
DUP 0
LDNW -4
LDNW 20
CALL 2
RETURN
END

PROC tMerge.halfstreamrec.Copy 4 4 0x00110001
! PROCEDURE (s: halfstream) Copy(): stream;
!   NEW(r);
CONST 8
GLOBAL tMerge.halfstreamrec
LOCAL -4
GLOBAL NEW
CALL 3
!   r.base := s.base.Copy();
LDLW 12
NCHECK 102
LOADW
NCHECK 102
DUP 0
LDNW -4
LDNW 24
CALLW 1
LDLW -4
NCHECK 102
STOREW
!   r.inc := s.inc;
LDLW 12
NCHECK 103
LDNW 4
LDLW -4
NCHECK 103
STNW 4
!   RETURN r
LDLW -4
RETURNW
END

PROC tMerge.NewMergeStream 4 4 0x00310001
! PROCEDURE NewMergeStream*(p, q: stream): mergestream;
!   NEW(s);
CONST 16
GLOBAL tMerge.mergestreamrec
LOCAL -4
GLOBAL NEW
CALL 3
!   s.p := p;
LDLW 12
LDLW -4
NCHECK 122
STOREW
!   s.q := q;
LDLW 16
LDLW -4
NCHECK 123
STNW 4
!   s.known := FALSE;
CONST 0
LDLW -4
NCHECK 124
CONST 8
STIC
!   RETURN s
LDLW -4
RETURNW
END

PROC tMerge.mergestreamrec.Fix 12 4 0x00100001
! PROCEDURE (s: mergestream) Fix();
!   lv := 0; rv := 0;
CONST 0
STLW -8
CONST 0
STLW -12
!   IF ~s.known THEN
LDLW 12
NCHECK 133
CONST 8
LDIC
JUMPT 8
!     lx := s.p.Done(); rx := s.q.Done();
LDLW 12
NCHECK 134
LOADW
NCHECK 134
DUP 0
LDNW -4
LDNW 12
CALLW 1
STLC -1
LDLW 12
NCHECK 134
LDNW 4
NCHECK 134
DUP 0
LDNW -4
LDNW 12
CALLW 1
STLC -2
!     s.done := lx & rx;
LDLC -1
LDLC -2
AND
LDLW 12
NCHECK 135
CONST 9
STIC
!     IF ~s.done THEN
LDLW 12
NCHECK 136
CONST 9
LDIC
JUMPT 8
!       IF ~lx THEN lv := s.p.Current() END; 
LDLC -1
JUMPT 14
LDLW 12
NCHECK 137
LOADW
NCHECK 137
DUP 0
LDNW -4
LDNW 16
CALLW 1
STLW -8
LABEL 14
!       IF ~rx THEN rv := s.q.Current() END;
LDLC -2
JUMPT 17
LDLW 12
NCHECK 138
LDNW 4
NCHECK 138
DUP 0
LDNW -4
LDNW 16
CALLW 1
STLW -12
LABEL 17
!       s.isleft := ~lx & (rx OR (lv <= rv));
LDLC -1
NOT
LDLC -2
LDLW -8
LDLW -12
LEQ
OR
AND
LDLW 12
NCHECK 139
CONST 10
STIC
!       IF s.isleft THEN s.current := lv ELSE s.current := rv END;
LDLW 12
NCHECK 140
CONST 10
LDIC
JUMPF 20
LDLW -8
LDLW 12
NCHECK 140
STNW 12
JUMP 18
LABEL 20
LDLW -12
LDLW 12
NCHECK 140
STNW 12
LABEL 18
!       s.known := TRUE
CONST 1
LDLW 12
NCHECK 141
CONST 8
STIC
LABEL 8
RETURN
END

PROC tMerge.mergestreamrec.Done 0 4 0x00100001
! PROCEDURE (s: mergestream) Done(): BOOLEAN;
! BEGIN s.Fix(); RETURN s.done END Done;
LDLW 12
NCHECK 147
DUP 0
LDNW -4
LDNW 28
CALL 1
LDLW 12
NCHECK 147
CONST 9
LDIC
RETURNW
END

PROC tMerge.mergestreamrec.Current 0 4 0x00100001
! PROCEDURE (s: mergestream) Current(): INTEGER;
!   s.Fix();
LDLW 12
NCHECK 151
DUP 0
LDNW -4
LDNW 28
CALL 1
!   RETURN s.current
LDLW 12
NCHECK 152
LDNW 12
RETURNW
END

PROC tMerge.mergestreamrec.Next 4 4 0x00100001
! PROCEDURE (s: mergestream) Next(k: INTEGER);
!   i := 0;
CONST 0
STLW -4
LABEL 21
!   WHILE i < k DO
LDLW -4
LDLW 16
JGEQ 23
!     s.Fix();
LDLW 12
NCHECK 160
DUP 0
LDNW -4
LDNW 28
CALL 1
!     IF s.isleft THEN s.p.Next(1) ELSE s.q.Next(1) END;
LDLW 12
NCHECK 161
CONST 10
LDIC
JUMPF 26
CONST 1
LDLW 12
NCHECK 161
LOADW
NCHECK 161
DUP 0
LDNW -4
LDNW 20
CALL 2
JUMP 24
LABEL 26
CONST 1
LDLW 12
NCHECK 161
LDNW 4
NCHECK 161
DUP 0
LDNW -4
LDNW 20
CALL 2
LABEL 24
!     s.known := FALSE;
CONST 0
LDLW 12
NCHECK 162
CONST 8
STIC
!     i := i+1
INCL -4
JUMP 21
LABEL 23
RETURN
END

PROC tMerge.mergestreamrec.Copy 0 4 0x00100001
! PROCEDURE (s: mergestream) Copy(): stream;
!   RETURN NewMergeStream(s.p.Copy(), s.q.Copy())
LDLW 12
NCHECK 169
LDNW 4
NCHECK 169
DUP 0
LDNW -4
LDNW 24
CALLW 1
LDLW 12
NCHECK 169
LOADW
NCHECK 169
DUP 0
LDNW -4
LDNW 24
STKMAP 0x00000005
CALLW 1
GLOBAL tMerge.NewMergeStream
CALLW 2
RETURNW
END

PROC tMerge.sort 8 4 0x00118001
! PROCEDURE sort(s: stream): stream;
!   p := NewHalfStream(s.Copy(), FALSE);
CONST 0
ALIGNC
LDLW 12
NCHECK 175
DUP 0
LDNW -4
LDNW 24
CALLW 1
GLOBAL tMerge.NewHalfStream
CALLW 2
STLW -4
!   q := NewHalfStream(s.Copy(), TRUE);
CONST 1
ALIGNC
LDLW 12
NCHECK 176
DUP 0
LDNW -4
LDNW 24
CALLW 1
GLOBAL tMerge.NewHalfStream
CALLW 2
STLW -8
!   IF q.Done() THEN
LDLW -8
NCHECK 177
DUP 0
LDNW -4
LDNW 12
CALLW 1
JUMPF 30
!     RETURN p
LDLW -4
RETURNW
LABEL 30
!     RETURN NewMergeStream(sort(p), sort(q))
LDLW -8
GLOBAL tMerge.sort
CALLW 1
LDLW -4
GLOBAL tMerge.sort
STKMAP 0x00000005
CALLW 1
GLOBAL tMerge.NewMergeStream
CALLW 2
RETURNW
END

PROC tMerge.%main 4 6 0
!   n := 25;
CONST 25
STGW tMerge.n
!   NEW(a, n);
LDGW tMerge.n
CONST 1
CONST 4
CONST 0
GLOBAL tMerge.a
GLOBAL NEWFLEX
CALL 5
!   m := 37;
CONST 37
STGW tMerge.m
!   FOR k := 0 TO n-1 DO 
LDGW tMerge.n
DEC
STLW -4
CONST 0
STGW tMerge.k
LABEL 32
LDGW tMerge.k
LDLW -4
JGT 33
!     a[k] := m;
LDGW tMerge.m
LDGW tMerge.a
NCHECK 193
LDGW tMerge.k
DUP 1
LDNW -4
LDNW 4
BOUND 193
STIW
!     m := (m + 61) MOD 100;
LDGW tMerge.m
CONST 61
PLUS
CONST 100
MOD
STGW tMerge.m
!     Out.Int(a[k], 3)
CONST 3
LDGW tMerge.a
NCHECK 195
LDGW tMerge.k
DUP 1
LDNW -4
LDNW 4
BOUND 195
LDIW
GLOBAL Out.Int
CALL 2
!   FOR k := 0 TO n-1 DO 
LDGW tMerge.k
INC
STGW tMerge.k
JUMP 32
LABEL 33
!   Out.Ln;
GLOBAL Out.Ln
CALL 0
!   s := sort(NewArrayStream(a));
LDGW tMerge.a
GLOBAL tMerge.NewArrayStream
CALLW 1
GLOBAL tMerge.sort
CALLW 1
STGW tMerge.s
LABEL 34
!   WHILE ~s.Done() DO
LDGW tMerge.s
NCHECK 200
DUP 0
LDNW -4
LDNW 12
CALLW 1
JUMPT 36
!     k := s.Current();
LDGW tMerge.s
NCHECK 201
DUP 0
LDNW -4
LDNW 16
CALLW 1
STGW tMerge.k
!     Out.Int(k, 3);
CONST 3
LDGW tMerge.k
GLOBAL Out.Int
CALL 2
!     s.Next(1)
CONST 1
LDGW tMerge.s
NCHECK 203
DUP 0
LDNW -4
LDNW 20
CALL 2
JUMP 34
LABEL 36
!   Out.Ln
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tMerge.a 4
GLOVAR tMerge.s 4
GLOVAR tMerge.k 4
GLOVAR tMerge.n 4
GLOVAR tMerge.m 4

! Pointer map
DEFINE tMerge.%gcmap
WORD GC_BASE
WORD tMerge.a
WORD 0
WORD GC_BASE
WORD tMerge.s
WORD 0
WORD GC_END

! Descriptor for streamrec
DEFINE tMerge.streamrec
WORD 0

! Descriptor for arraystreamrec
DEFINE tMerge.arraystreamrec
WORD 0x00000003
WORD 1
WORD tMerge.arraystreamrec.%anc
WORD tMerge.arraystreamrec.Done
WORD tMerge.arraystreamrec.Current
WORD tMerge.arraystreamrec.Next
WORD tMerge.arraystreamrec.Copy

DEFINE tMerge.arraystreamrec.%anc
WORD tMerge.streamrec
WORD tMerge.arraystreamrec

! Descriptor for halfstreamrec
DEFINE tMerge.halfstreamrec
WORD 0x00000003
WORD 1
WORD tMerge.halfstreamrec.%anc
WORD tMerge.halfstreamrec.Done
WORD tMerge.halfstreamrec.Current
WORD tMerge.halfstreamrec.Next
WORD tMerge.halfstreamrec.Copy

DEFINE tMerge.halfstreamrec.%anc
WORD tMerge.streamrec
WORD tMerge.halfstreamrec

! Descriptor for mergestreamrec
DEFINE tMerge.mergestreamrec
WORD 0x00000007
WORD 1
WORD tMerge.mergestreamrec.%anc
WORD tMerge.mergestreamrec.Done
WORD tMerge.mergestreamrec.Current
WORD tMerge.mergestreamrec.Next
WORD tMerge.mergestreamrec.Copy
WORD tMerge.mergestreamrec.Fix

DEFINE tMerge.mergestreamrec.%anc
WORD tMerge.streamrec
WORD tMerge.mergestreamrec

! End of file
]]*)
