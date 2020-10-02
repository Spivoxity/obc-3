MODULE tHide;

(*<<
blob.one
blob.two 12
blob.one
spot.two 23
spot.one
>>*)

IMPORT Base := xPrelude, Out;

TYPE spot* = RECORD (Base.blob) x*: INTEGER END;

PROCEDURE (VAR b: spot) one;
BEGIN
  Out.String("spot.one"); Out.Ln;
  b.x := 34
END one;

PROCEDURE (VAR b: spot) two*;
BEGIN
  Out.String("spot.two "); Out.Int(b.x, 0); Out.Ln
END two;

VAR b: Base.blob; bb: spot;

BEGIN
  bb.x := 23;
  b.proc; bb.proc; bb.one
END tHide.

(*[[
!! (SYMFILE #tHide STAMP #tHide.%main 1 #tHide.m)
!! (DEF ?1 #xPrelude 2 #blob (RECORD #xPrelude.blob 8 VOID
!!     (FIELD #x 0 INTEGER)
!!     (FIELD #y* 4 INTEGER)))
!! (TYPE #spot* !2 (RECORD #tHide.spot 12 =1
!!     (FIELD #x 0 INTEGER)
!!     (FIELD #y* 4 INTEGER)
!!     (FIELD #x* 8 INTEGER)))
!! (METHOD =1 #one 31 0 #xPrelude.blob.one ?3 #xPrelude 3 ANON (METH 2 VOID
!!     (VPARAM #b 12 =1)))
!! (METHOD =1 #two* 37 1 #xPrelude.blob.two ?4 #xPrelude 4 ANON (METH 2 VOID
!!     (VPARAM #b 12 =1)))
!! (METHOD =1 #proc* 42 2 #xPrelude.blob.proc ?5 #xPrelude 5 ANON (METH 2 VOID
!!     (VPARAM #b 12 =1)))
!! (METHOD =2 #one 31 0 #xPrelude.blob.one =3)
!! (METHOD =2 #two* 21 1 #tHide.spot.two !6 (METH 2 VOID
!!     (VPARAM #b 12 =2)))
!! (METHOD =2 #proc* 42 2 #xPrelude.blob.proc =5)
!! (METHOD =2 #one 15 3 #tHide.spot.one !7 (METH 2 VOID
!!     (VPARAM #b 12 =2)))
!! (CHKSUM STAMP)
!! 
MODULE tHide STAMP 0
IMPORT xPrelude STAMP
IMPORT Out STAMP
ENDHDR

PROC tHide.spot.one 0 3 0x00100001
! PROCEDURE (VAR b: spot) one;
!   Out.String("spot.one"); Out.Ln;
CONST 9
GLOBAL tHide.%1
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
!   b.x := 34
CONST 34
LDLW 12
STNW 8
RETURN
END

PROC tHide.spot.two 0 3 0x00100001
! PROCEDURE (VAR b: spot) two*;
!   Out.String("spot.two "); Out.Int(b.x, 0); Out.Ln
CONST 10
GLOBAL tHide.%2
GLOBAL Out.String
CALL 2
CONST 0
LDLW 12
LDNW 8
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

PROC tHide.%main 0 3 0
!   bb.x := 23;
CONST 23
GLOBAL tHide.bb
STNW 8
!   b.proc; bb.proc; bb.one
GLOBAL xPrelude.blob
GLOBAL tHide.b
GLOBAL xPrelude.blob.proc
CALL 2
GLOBAL tHide.spot
GLOBAL tHide.bb
GLOBAL xPrelude.blob.proc
CALL 2
GLOBAL tHide.spot
GLOBAL tHide.bb
GLOBAL tHide.spot.one
CALL 2
RETURN
END

! Global variables
GLOVAR tHide.b 8
GLOVAR tHide.bb 12

! String "spot.one"
DEFINE tHide.%1
STRING 73706F742E6F6E6500

! String "spot.two "
DEFINE tHide.%2
STRING 73706F742E74776F2000

! Descriptor for spot
DEFINE tHide.spot
WORD 0
WORD 1
WORD tHide.spot.%anc
WORD xPrelude.blob.one
WORD tHide.spot.two
WORD xPrelude.blob.proc
WORD tHide.spot.one

DEFINE tHide.spot.%anc
WORD xPrelude.blob
WORD tHide.spot

! End of file
]]*)
