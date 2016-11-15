MODULE xTypes;

TYPE ptr* = POINTER TO rec; rec* = RECORD x*: INTEGER END;

VAR r99*: rec;

(* Bug Richard Bird 1/3/05 *)
TYPE Entry    = POINTER TO EntryRec;
     EntryRec = RECORD
                   Display* : PROCEDURE (x: Entry);
                 END;

TYPE Person*    = POINTER TO PersonRec;
     PersonRec* = RECORD (EntryRec) END;

TYPE Thing* = POINTER TO Blob;
  Blob* = ABSTRACT RECORD END;

ABSTRACT PROCEDURE (p: Thing) Print*;

END xTypes.

