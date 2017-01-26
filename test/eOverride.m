MODULE eOverride;

TYPE rec = RECORD x: INTEGER END; ptr = POINTER TO rec;

TYPE rec2 = RECORD (rec) END; ptr2 = POINTER TO rec2;

PROCEDURE (VAR r: rec) m; END m;

PROCEDURE (VAR r: rec2) m*(q: REAL); END m;

END eOverride.

(*<<
"./eOverride.m", line 9: method 'rec2.m' overrides 'rec.m' with incompatible
parameters
> PROCEDURE (VAR r: rec2) m*(q: REAL); END m;
>                         ^
>>*)
