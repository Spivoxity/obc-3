MODULE tSelfRep07; IMPORT Out; CONST x = "MODULE tSelfRep07; IMPORT Out; CONST x = "; y = "; BEGIN Out.String(x); Out.Char(22X); Out.String(x); Out.Char(22X); Out.String('; y = '); Out.Char(22X); Out.String(y); Out.Char(22X); Out.String(y); Out.Ln END tSelfRep07."; BEGIN Out.String(x); Out.Char(22X); Out.String(x); Out.Char(22X); Out.String('; y = '); Out.Char(22X); Out.String(y); Out.Char(22X); Out.String(y); Out.Ln END tSelfRep07.

(*<<
MODULE tSelfRep07; IMPORT Out; CONST x = "MODULE tSelfRep07; IMPORT Out; CONST x = "; y = "; BEGIN Out.String(x); Out.Char(22X); Out.String(x); Out.Char(22X); Out.String('; y = '); Out.Char(22X); Out.String(y); Out.Char(22X); Out.String(y); Out.Ln END tSelfRep07."; BEGIN Out.String(x); Out.Char(22X); Out.String(x); Out.Char(22X); Out.String('; y = '); Out.Char(22X); Out.String(y); Out.Char(22X); Out.String(y); Out.Ln END tSelfRep07.
>>*)

(*[[
!! (SYMFILE #tSelfRep07 STAMP #tSelfRep07.%main 1 #tSelfRep07.m)
!! (CHKSUM STAMP)
!! 
MODULE tSelfRep07 STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tSelfRep07.%main 0 3 0
! MODULE tSelfRep07; IMPORT Out; CONST x = "MODULE tSelfRep07; IMPORT Out; CONST x = "; y = "; BEGIN Out.String(x); Out.Char(22X); Out.String(x); Out.Char(22X); Out.String('; y = '); Out.Char(22X); Out.String(y); Out.Char(22X); Out.String(y); Out.Ln END tSelfRep07."; BEGIN Out.String(x); Out.Char(22X); Out.String(x); Out.Char(22X); Out.String('; y = '); Out.Char(22X); Out.String(y); Out.Char(22X); Out.String(y); Out.Ln END tSelfRep07.
CONST 42
GLOBAL tSelfRep07.%1
GLOBAL Out.String
CALL 2
CONST 34
ALIGNC
GLOBAL Out.Char
CALL 1
CONST 42
GLOBAL tSelfRep07.%1
GLOBAL Out.String
CALL 2
CONST 34
ALIGNC
GLOBAL Out.Char
CALL 1
CONST 7
GLOBAL tSelfRep07.%3
GLOBAL Out.String
CALL 2
CONST 34
ALIGNC
GLOBAL Out.Char
CALL 1
CONST 173
GLOBAL tSelfRep07.%2
GLOBAL Out.String
CALL 2
CONST 34
ALIGNC
GLOBAL Out.Char
CALL 1
CONST 173
GLOBAL tSelfRep07.%2
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

! String "MODULE tSelfRep07; IMPORT Out; CONST x = "
DEFINE tSelfRep07.%1
STRING 4D4F44554C45207453656C6652657030373B20494D504F5254204F75743B2043
STRING 4F4E53542078203D2000

! String "; BEGIN Out.String(x); Out.Char(22X); Out.String(x); Out.Char(22X); Out.String('; y = '); Out.Char(22X); Out.String(y); Out.Char(22X); Out.String(y); Out.Ln END tSelfRep07."
DEFINE tSelfRep07.%2
STRING 3B20424547494E204F75742E537472696E672878293B204F75742E4368617228
STRING 323258293B204F75742E537472696E672878293B204F75742E43686172283232
STRING 58293B204F75742E537472696E6728273B2079203D2027293B204F75742E4368
STRING 617228323258293B204F75742E537472696E672879293B204F75742E43686172
STRING 28323258293B204F75742E537472696E672879293B204F75742E4C6E20454E44
STRING 207453656C6652657030372E00

! String "; y = "
DEFINE tSelfRep07.%3
STRING 3B2079203D2000

! End of file
]]*)
