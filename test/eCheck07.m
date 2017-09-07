MODULE eCheck07;

(* Coverage for check.ml *)

PROCEDURE one; RETURN 3 END one;

END eCheck07.

(*<<
"eCheck07.m", line 5: this procedure should not have a RETURN clause
> PROCEDURE one; RETURN 3 END one;
>                       ^
>>*)
