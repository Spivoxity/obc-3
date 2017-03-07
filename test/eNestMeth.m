MODULE eNestMeth;

TYPE rec = RECORD END; ptr = POINTER TO rec;

PROCEDURE mopsy*;
  PROCEDURE (VAR x: ptr) methylate; END methylate;
END mopsy;

END eNestMeth.

(*<<
"eNestMeth.m", line 6: methods may only be declared at the outermost level
>   PROCEDURE (VAR x: ptr) methylate; END methylate;
>                          ^^^^^^^^^
>>*)
