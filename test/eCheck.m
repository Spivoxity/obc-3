MODULE eCheck;

IMPORT xTypes;

TYPE r = RECORD END;

ABSTRACT PROCEDURE (VAR x: r) m;

PROCEDURE (x: xTypes.ptr) foo; END foo;

PROCEDURE foo;
  PROCEDURE baz IS "baz";
END foo;

END eCheck.

(*<<
"eCheck.m", line 7: only abstract records can have abstract methods
> ABSTRACT PROCEDURE (VAR x: r) m;
>                               ^

"eCheck.m", line 9: record type 'xTypes.rec' does not belong to this module
> PROCEDURE (x: xTypes.ptr) foo; END foo;
>               ^^^^^^^^^^

"eCheck.m", line 12: primitives must be declared at the outermost level
>   PROCEDURE baz IS "baz";
>             ^^^
>>*)
