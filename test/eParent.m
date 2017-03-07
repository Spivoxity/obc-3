MODULE eParent;

TYPE rec = RECORD x: INTEGER END; ptr = POINTER TO rec;

TYPE rec2 = RECORD (ptr) END;

END eParent.

(*<<
"eParent.m", line 5: a parent must be a record type
> TYPE rec2 = RECORD (ptr) END;
>                     ^^^
> This expression has type ptr = POINTER TO rec
>>*)
