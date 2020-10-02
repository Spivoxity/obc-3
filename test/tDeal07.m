MODULE tDeal07;

IMPORT Random, Out;

CONST NS = 4; (* Number of suits *)
  NR = 13; (* Number of ranks *)
  NP = 4; (* Number of players *)
  NH = 13; (* Size of each hand *)

  suits = "CDHS"; 
  ranks = "23456789TJQKA";

TYPE Card = ARRAY 2 OF CHAR;
  Deal = ARRAY NP OF ARRAY NH OF Card;

VAR deck: ARRAY NS*NR OF Card;

(* Construct the (unshuffled) deck *)
PROCEDURE MakeDeck;
  VAR i, s, r: INTEGER;
BEGIN
  i := 0;
  FOR s := 0 TO NS-1 DO
    FOR r := 0 TO NR-1 DO
      deck[i][0] := ranks[r];
      deck[i][1] := suits[s];
      i := i+1
    END
  END
END MakeDeck;

(* Deal hands from the deck, each hand in order *)
PROCEDURE MakeDeal(VAR deal: Deal);
  VAR n, p, r, t: INTEGER; 
    k: ARRAY NP OF INTEGER;
BEGIN
  (* For each i, player i has received k[i] cards so far *)
  FOR p := 0 TO NP-1 DO k[p] := 0 END;

  n := 0;
  WHILE n < NS*NR DO
    (* Each player i has prob. (NH - k[i]) / (NP*NH - n) 
       of getting the next card. *)
    r := Random.Roll(NP*NH - n);

    (* Find p s.t. thresh(p) <= r < thresh(p+1),
       where thresh(p) = sum_{0 <= i < p} NH - k[i] *)
    p := 0; t := 0;
    LOOP
      t := t + NH - k[p];
      IF r < t THEN EXIT END;
      p := p+1
    END;

    (* Player p gets the card *)
    deal[p][k[p]] := deck[n];
    INC(k[p]);
    n := n+1
  END
END MakeDeal;

PROCEDURE Main;
  VAR deal: Deal; i, j: INTEGER;
BEGIN
  MakeDeck;
  MakeDeal(deal);

  FOR i := 0 TO NP-1 DO
    FOR j := 0 TO NH-1 DO
      Out.Char(' '); Out.String(deal[i][j])
    END;
    Out.Ln
  END
END Main;

BEGIN
  Main
END tDeal07.

(*<<
 2C 3C 9C JC QC 2D 7D 8D AD QH KH 9S AS
 8C 6D 9D TD KD 2H 7H 9H JH 2S 4S 7S 8S
 6C TC KC 4D QD 3H 5H 6H AH 6S TS JS QS
 4C 5C 7C AC 3D 5D JD 4H 8H TH 3S 5S KS
>>*)

(*[[
!! (SYMFILE #tDeal07 STAMP #tDeal07.%main 1 #tDeal07.m)
!! (CHKSUM STAMP)
!! 
MODULE tDeal07 STAMP 0
IMPORT Random STAMP
IMPORT Out STAMP
ENDHDR

PROC tDeal07.MakeDeck 12 5 0
! PROCEDURE MakeDeck;
!   i := 0;
CONST 0
STLW -4
!   FOR s := 0 TO NS-1 DO
CONST 0
STLW -8
LABEL L3
LDLW -8
CONST 3
JGT L4
!     FOR r := 0 TO NR-1 DO
CONST 0
STLW -12
LABEL L5
LDLW -12
CONST 12
JGT L6
!       deck[i][0] := ranks[r];
GLOBAL tDeal07.%2
LDLW -12
CONST 14
BOUND 25
LDIC
GLOBAL tDeal07.deck
LDLW -4
CONST 52
BOUND 25
INDEXS
STOREC
!       deck[i][1] := suits[s];
GLOBAL tDeal07.%1
LDLW -8
CONST 5
BOUND 26
LDIC
GLOBAL tDeal07.deck
LDLW -4
CONST 52
BOUND 26
CONST 2
TIMES
INC
STIC
!       i := i+1
INCL -4
!     FOR r := 0 TO NR-1 DO
INCL -12
JUMP L5
LABEL L6
!   FOR s := 0 TO NS-1 DO
INCL -8
JUMP L3
LABEL L4
RETURN
END

PROC tDeal07.MakeDeal 32 5 0x00100001
! PROCEDURE MakeDeal(VAR deal: Deal);
!   FOR p := 0 TO NP-1 DO k[p] := 0 END;
CONST 0
STLW -8
LABEL L7
LDLW -8
CONST 3
JGT L8
CONST 0
LOCAL -32
LDLW -8
CONST 4
BOUND 38
STIW
INCL -8
JUMP L7
LABEL L8
!   n := 0;
CONST 0
STLW -4
LABEL L9
!   WHILE n < NS*NR DO
LDLW -4
CONST 52
JGEQ L11
!     r := Random.Roll(NP*NH - n);
CONST 52
LDLW -4
MINUS
GLOBAL Random.Roll
CALLW 1
STLW -12
!     p := 0; t := 0;
CONST 0
STLW -8
CONST 0
STLW -16
LABEL L12
!       t := t + NH - k[p];
LDLW -16
CONST 13
PLUS
LOCAL -32
LDLW -8
CONST 4
BOUND 50
LDIW
MINUS
STLW -16
!       IF r < t THEN EXIT END;
LDLW -12
LDLW -16
JLT L13
!       p := p+1
INCL -8
JUMP L12
LABEL L13
!     deal[p][k[p]] := deck[n];
LDLW 12
LDLW -8
CONST 4
BOUND 56
CONST 13
TIMES
LOCAL -32
LDLW -8
CONST 4
BOUND 56
LDIW
CONST 13
BOUND 56
PLUS
INDEXS
GLOBAL tDeal07.deck
LDLW -4
CONST 52
BOUND 56
INDEXS
CONST 2
FIXCOPY
!     INC(k[p]);
LOCAL -32
LDLW -8
CONST 4
BOUND 57
INDEXW
DUP 0
LOADW
INC
SWAP
STOREW
!     n := n+1
INCL -4
JUMP L9
LABEL L11
RETURN
END

PROC tDeal07.Main 112 5 0
! PROCEDURE Main;
!   MakeDeck;
GLOBAL tDeal07.MakeDeck
CALL 0
!   MakeDeal(deal);
LOCAL -104
GLOBAL tDeal07.MakeDeal
CALL 1
!   FOR i := 0 TO NP-1 DO
CONST 0
STLW -108
LABEL L17
LDLW -108
CONST 3
JGT L18
!     FOR j := 0 TO NH-1 DO
CONST 0
STLW -112
LABEL L19
LDLW -112
CONST 12
JGT L20
!       Out.Char(' '); Out.String(deal[i][j])
CONST 32
ALIGNC
GLOBAL Out.Char
CALL 1
CONST 2
LOCAL -104
LDLW -108
CONST 4
BOUND 70
CONST 13
TIMES
LDLW -112
CONST 13
BOUND 70
PLUS
INDEXS
GLOBAL Out.String
CALL 2
!     FOR j := 0 TO NH-1 DO
INCL -112
JUMP L19
LABEL L20
!     Out.Ln
GLOBAL Out.Ln
CALL 0
!   FOR i := 0 TO NP-1 DO
INCL -108
JUMP L17
LABEL L18
RETURN
END

PROC tDeal07.%main 0 1 0
!   Main
GLOBAL tDeal07.Main
CALL 0
RETURN
END

! Global variables
GLOVAR tDeal07.deck 104

! String "CDHS"
DEFINE tDeal07.%1
STRING 4344485300

! String "23456789TJQKA"
DEFINE tDeal07.%2
STRING 3233343536373839544A514B4100

! End of file
]]*)
