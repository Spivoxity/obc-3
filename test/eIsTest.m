MODULE eIsTest;

TYPE rec = RECORD x: INTEGER END; ptr = POINTER TO rec;

TYPE rec3 = RECORD END; ptr3 = POINTER TO rec3;

VAR x: INTEGER; u: rec3;

PROCEDURE VP*(VAR r: rec);
  VAR b: BOOLEAN; q: ptr;
BEGIN
  b := r IS rec3;
  b := x IS INTEGER;
  b := u IS rec3;
  b := q^ IS rec3;
  b := q IS rec;

  b := q(rec3) IS ptr3
END VP;

END eIsTest.

(*<<
"eIsTest.m", line 12: a supertype of record type 'rec3' is needed here
>   b := r IS rec3;
>        ^
> This expression has record type 'rec'

"eIsTest.m", line 13: a record pointer or VAR parameter is needed here
>   b := x IS INTEGER;
>        ^
> This expression has type INTEGER

"eIsTest.m", line 14: a record pointer or VAR parameter is needed here
>   b := u IS rec3;
>        ^

"eIsTest.m", line 15: a supertype of record type 'rec3' is needed here
>   b := q^ IS rec3;
>        ^^
> This expression has record type 'rec'

"eIsTest.m", line 16: a supertype of record type 'rec' is needed here
>   b := q IS rec;
>        ^
> This expression has type ptr = POINTER TO rec

"eIsTest.m", line 18: a supertype of record type 'rec3' is needed here
>   b := q(rec3) IS ptr3
>        ^
> This expression has type ptr = POINTER TO rec

"eIsTest.m", line 18: a record pointer or VAR parameter is needed here
>   b := q(rec3) IS ptr3
>        ^^^^^^^
>>*)
