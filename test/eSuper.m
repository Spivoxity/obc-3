MODULE eSuper;

TYPE rec = RECORD x: INTEGER END; ptr = POINTER TO rec;

TYPE rec2 = RECORD (rec) END; ptr2 = POINTER TO rec2;

PROCEDURE (VAR r: rec2) mm; END mm;

PROCEDURE super*;
  VAR r: rec2;
BEGIN
  r.mm^
END super;

END eSuper.

(*<<
"eSuper.m", line 12: in this super call, the parent record type 'rec' does
not support method 'mm'
>   r.mm^
>   ^^^^^
>>*)
