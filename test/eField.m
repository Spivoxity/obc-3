MODULE eField;

IMPORT xTypes;

VAR x: INTEGER; u: xTypes.rec;

BEGIN
  u.frown;
  x := u.mint
END eField.

(*<<
"t/eField.m", line 8: this record does not have a visible field called
'frown'
>   u.frown;
>   ^
> This expression has record type 'xTypes.rec'

"t/eField.m", line 9: this record does not have a visible field called 'mint'
>   x := u.mint
>        ^
> This expression has record type 'xTypes.rec'
>>*)
  
