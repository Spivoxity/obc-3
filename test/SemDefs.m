MODULE SemDefs;

TYPE fred* = RECORD mint: INTEGER END;

PROCEDURE (VAR x: fred) smile*; END smile;

PROCEDURE (VAR x: fred) frown; END frown;

END SemDefs.
