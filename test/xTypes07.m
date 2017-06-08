MODULE xTypes07;

TYPE rec* = RECORD u, x*: INTEGER END;

TYPE Base* = POINTER TO BaseCell;
  Ext1* = POINTER TO Ext1Cell;
  Ext2* = POINTER TO Ext2Cell;
  BaseCell* = RECORD END;
  Ext1Cell* = RECORD (BaseCell) stuff1*: INTEGER END;
  Ext2Cell* = RECORD (BaseCell) stuff2*: REAL END;

END xTypes07.
