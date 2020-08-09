MODULE eAbsRec;

TYPE a = ABSTRACT RECORD END;
 b = RECORD (a) END;

ABSTRACT PROCEDURE (VAR self: a) m;

VAR u*: a; r*: b; s: POINTER TO a;
  t: POINTER TO RECORD (a) END;

BEGIN
  NEW(s)
END eAbsRec.

(*<<
"eAbsRec.m", line 8: cannot declare instance of abstract record type
> VAR u*: a; r*: b; s: POINTER TO a;
>         ^

"eAbsRec.m", line 4: record type 'b' should implement abstract method 'm' or
be declared abstract itself
>  b = RECORD (a) END;
>      ^^^^^^^^^^^^^^

"eAbsRec.m", line 9: an anonymous record type should implement abstract
method 'm' or be declared abstract itself
>   t: POINTER TO RECORD (a) END;
>                 ^^^^^^^^^^^^^^

"eAbsRec.m", line 12: cannot create instance of abstract record type
>   NEW(s)
>       ^
> This expression has type POINTER TO a
>>*)
