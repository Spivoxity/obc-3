MODULE eRcvrType;

PROCEDURE (x: INTEGER) method1; END method1;

PROCEDURE (VAR x: INTEGER) method2; END method2;

TYPE rec = RECORD x: INTEGER END; ptr = POINTER TO rec;

PROCEDURE (VAR r: rec) x; END x;

PROCEDURE (VAR r: rec) m; END m;

PROCEDURE (p: ptr) m; END m;

END eRcvrType.

(*<<
"eRcvrType.m", line 3: a value receiver must be a record pointer
> PROCEDURE (x: INTEGER) method1; END method1;
>            ^
> This expression has type INTEGER

"eRcvrType.m", line 5: a VAR receiver must have a record type
> PROCEDURE (VAR x: INTEGER) method2; END method2;
>                ^
> This expression has type INTEGER

"eRcvrType.m", line 9: method 'x' has the same name as a field
> PROCEDURE (VAR r: rec) x; END x;
>                        ^

"eRcvrType.m", line 13: method 'm' has already been defined
> PROCEDURE (p: ptr) m; END m;
>                    ^
>>*)
