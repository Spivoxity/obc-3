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
!! SYMFILE #tHide STAMP #tHide.%main 1
!! DEF ?0 #xPrelude 1 #blob RECORD #xPrelude.blob 8 VOID
!!   FIELD #x 0 INTEGER
!!   FIELD #y* 4 INTEGER;
!! TYPE #spot* !1 RECORD #tHide.spot 12 0
!!   FIELD #x 0 INTEGER
!!   FIELD #y* 4 INTEGER
!!   FIELD #x* 8 INTEGER;
!! METHOD 0 #one 31 0 #xPrelude.blob.one ?2 #xPrelude 2 ANON METH 2 VOID
!!   VPARAM #b 12 0;;
!! METHOD 0 #two* 37 1 #xPrelude.blob.two ?3 #xPrelude 3 ANON METH 2 VOID
!!   VPARAM #b 12 0;;
!! METHOD 0 #proc* 42 2 #xPrelude.blob.proc ?4 #xPrelude 4 ANON METH 2 VOID
!!   VPARAM #b 12 0;;
!! METHOD 1 #one 31 0 #xPrelude.blob.one 2;
!! METHOD 1 #two* 21 1 #tHide.spot.two !5 METH 2 VOID
!!   VPARAM #b 12 1;;
!! METHOD 1 #proc* 42 2 #xPrelude.blob.proc 4;
!! METHOD 1 #one 15 3 #tHide.spot.one !6 METH 2 VOID
!!   VPARAM #b 12 1;;
!! END STAMP
!! 
MODULE tHide STAMP 0
IMPORT xPrelude STAMP
IMPORT Out STAMP
ENDHDR

PROC tHide.spot.one 0 12 0x00100001
! PROCEDURE (VAR b: spot) one;
!   Out.String("spot.one"); Out.Ln;
CONST 9
CONST tHide.%1
CONST Out.String
CALL 2
CONST Out.Ln
CALL 0
!   b.x := 34
CONST 34
LDLW 12
STNW 8
RETURN
END

PROC tHide.spot.two 0 12 0x00100001
! PROCEDURE (VAR b: spot) two*;
!   Out.String("spot.two "); Out.Int(b.x, 0); Out.Ln
CONST 10
CONST tHide.%2
CONST Out.String
CALL 2
CONST 0
LDLW 12
LDNW 8
CONST Out.Int
CALL 2
CONST Out.Ln
CALL 0
RETURN
END

PROC tHide.%main 0 12 0
!   bb.x := 23;
CONST 23
CONST tHide.bb
STNW 8
!   b.proc; bb.proc; bb.one
CONST xPrelude.blob
CONST tHide.b
CONST xPrelude.blob.proc
CALL 2
CONST tHide.spot
CONST tHide.bb
CONST xPrelude.blob.proc
CALL 2
CONST tHide.spot
CONST tHide.bb
CONST tHide.spot.one
CALL 2
RETURN
END

! Global variables
GLOBAL tHide.b 8
GLOBAL tHide.bb 12

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
