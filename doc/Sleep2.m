MODULE Sleep2;

IMPORT DynLink;

PROCEDURE Usec*(usec: INTEGER): INTEGER IS "Sleep_Usec";

BEGIN
  DynLink.Load("./prim.so")
END Sleep2.
