MODULE Buffer;

(* pp/ewoks/Buffer.m *)

(* This module implements a one-dimensional text buffer with
   insertion and deletion.  The text is
   stored in an array |buf[0..SIZE)| with a `gap' in the middle, so
   that the actual text is |buf[0..gap) \cat buf[SIZE-len+gap..SIZE)|,
   where |0 \leq len \leq SIZE| and |0 \leq gap \leq len|.  We also keep the
   current position in the buffer in the variable |pos|, 
   with |0 \leq pos \leq len|.

   In general, |gap| may be different from |pos|, but before
   insertions and deletions we establish |gap = pos| by 
   calling~|MoveGap|. *)

IMPORT Files;

CONST SIZE = 50000;

CONST NUL* = 0X;			(* Char returned at end of file *)

TYPE Buffer* = RECORD
    pos-: INTEGER;			(* Cursor position *)
    gap: INTEGER;			(* Position of gap *)
    len-: INTEGER;			(* Length of text *)
    buf: ARRAY SIZE OF CHAR;		(* The text itself *)
  END;

(* |Clear| -- make the buffer empty *)
PROCEDURE (VAR b: Buffer) Clear*;
BEGIN
  b.pos := 0; b.gap := 0; b.len := 0
END Clear;

(* |MoveLeft| -- move cursor left if not at beginning *)
PROCEDURE (VAR b: Buffer) MoveLeft*;
BEGIN
  IF b.pos > 0 THEN b.pos := b.pos-1 END
END MoveLeft;

(* |MoveRight| -- move cursor right if not at end *)
PROCEDURE (VAR b: Buffer) MoveRight*;
BEGIN
  IF b.pos < b.len THEN b.pos := b.pos+1 END
END MoveRight;

(* |Peek| -- return char at position |i| *)
PROCEDURE (VAR b: Buffer) Peek*(i: INTEGER): CHAR;
BEGIN
  IF (i < 0) OR (i >= b.len) THEN
    RETURN NUL
  ELSIF i < b.gap THEN
    RETURN b.buf[i]
  ELSE
    RETURN b.buf[i+SIZE-b.len]
  END
END Peek;

(* |MoveGap| -- move gap to just right of cursor *)
PROCEDURE (VAR b: Buffer) MoveGap;
  VAR gaplen, i: INTEGER;
BEGIN
  gaplen := SIZE - b.len;
  IF b.pos < b.gap THEN
    FOR i := b.gap-1 TO b.pos BY -1 DO
      b.buf[i+gaplen] := b.buf[i]
    END
  ELSE
    FOR i := b.gap TO b.pos-1 DO
      b.buf[i] := b.buf[i+gaplen]
    END
  END;
  b.gap := b.pos
END MoveGap;

(* |InsertLeft| -- insert char to left of cursor *)
PROCEDURE (VAR b: Buffer) InsertLeft*(ch: CHAR);
BEGIN
  ASSERT(b.len < SIZE);
  b.MoveGap;
  b.buf[b.pos] := ch;
  b.pos := b.pos+1; b.len := b.len+1; b.gap := b.gap+1
END InsertLeft;

(* |InsertRight| -- insert char to right of cursor *)
PROCEDURE (VAR b: Buffer) InsertRight*(ch: CHAR);
BEGIN
  ASSERT(b.len < SIZE);
  b.MoveGap;
  b.buf[b.pos] := ch;
  b.len := b.len+1; b.gap := b.gap+1
END InsertRight;

(* |DeleteLeft| -- delete char to left of cursor, if any *)
PROCEDURE (VAR b: Buffer) DeleteLeft*;
BEGIN
  IF b.pos > 0 THEN 
    b.MoveGap;
    b.pos := b.pos-1; b.gap := b.gap-1; b.len := b.len-1
  END
END DeleteLeft;

(* |DeleteRight| -- delete char to right of cursor, if any *)
PROCEDURE (VAR b: Buffer) DeleteRight*;
BEGIN
  IF b.pos < b.len THEN 
    b.MoveGap;
    b.len := b.len-1
  END
END DeleteRight;

(* |SetPosition| -- set cursor *)
PROCEDURE (VAR b: Buffer) SetPosition*(p: INTEGER);
BEGIN
  ASSERT((p >= 0) & (p <= b.len));
  b.pos := p
END SetPosition;

(* |InsertFile| -- insert contents of open file before cursor *)
PROCEDURE (VAR b: Buffer) InsertFile*(f: Files.File);
  VAR ch: CHAR;
BEGIN
  WHILE ~Files.Eof(f) DO
    Files.ReadChar(f, ch);
    b.InsertLeft(ch)
  END
END InsertFile;

(* |WriteFile| -- save buffer contents to open file *)
PROCEDURE (VAR b: Buffer) WriteFile*(f: Files.File);
  VAR i: INTEGER;
BEGIN
  FOR i := 0 TO b.gap-1 DO
    Files.WriteChar(f, b.buf[i])
  END;
  FOR i := b.gap TO b.len-1 DO
    Files.WriteChar(f, b.buf[SIZE-b.len+i])
  END
END WriteFile;

END Buffer.
