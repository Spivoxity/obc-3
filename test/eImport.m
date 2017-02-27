MODULE eImport;

IMPORT Out;

BEGIN
  Out.Anything
END eImport.

(*<<
"t/eImport.m", line 6: module 'Out' does not export 'Anything'
>   Out.Anything
>   ^^^^^^^^^^^^
>>*)
