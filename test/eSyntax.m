#line 2 "foo.m"
MODULE eSyntax;

BEGIN
  f(1,
    2,
    3,
    4;
  g(1, 2, 3;
END eSyntax

(*<<
"foo.m", line 8: unmatched left parenthesis at token ';'
>   f(1, ...
>    ^
>     4;
> ^

"foo.m", line 9: unmatched left parenthesis at token ';'
>   g(1, 2, 3;
>    ^^

"foo.m", line 30: unterminated comment
> (* Unterminated
> ^^
>>*)

This closing bracket is needed! ---> *)

(* Unterminated

