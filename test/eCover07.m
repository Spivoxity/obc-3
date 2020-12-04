MODULE eCover07,

(* Test coverage *)

VAR x-: RECORD y: INTEGER; END

PROCEDURE (CONST x: INTEGER) p; END;

PROCEDURE THEN; END p;

END eCover07.

(*<<
"eCover07.m", line 1: expected ';' at token ','
> MODULE eCover07,
>                ^

"eCover07.m", line 5: '-' is not allowed as an export mark in Oberon-07
> VAR x-: RECORD y: INTEGER; END
>      ^

"eCover07.m", line 5: Oberon-07 forbids a semicolon here
> VAR x-: RECORD y: INTEGER; END
>                          ^

"eCover07.m", line 5: missing ';'
> VAR x-: RECORD y: INTEGER; END
>                               ^

"eCover07.m", line 7: expected identifier 'p' after END
> PROCEDURE (CONST x: INTEGER) p; END;
>                                    ^

"eCover07.m", line 9: syntax error at token 'THEN'
> PROCEDURE THEN; END p;
>           ^^^^
>>*)
