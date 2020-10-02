MODULE tSelfRep; IMPORT Out; CONST x = "MODULE tSelfRep; IMPORT Out; CONST x = "; y = "; BEGIN Out.String(x); Out.Char(22X); Out.String(x); Out.Char(22X); Out.String('; y = '); Out.Char(22X); Out.String(y); Out.Char(22X); Out.String(y); Out.Ln END tSelfRep."; BEGIN Out.String(x); Out.Char(22X); Out.String(x); Out.Char(22X); Out.String('; y = '); Out.Char(22X); Out.String(y); Out.Char(22X); Out.String(y); Out.Ln END tSelfRep.

(*<<
MODULE tSelfRep; IMPORT Out; CONST x = "MODULE tSelfRep; IMPORT Out; CONST x = "; y = "; BEGIN Out.String(x); Out.Char(22X); Out.String(x); Out.Char(22X); Out.String('; y = '); Out.Char(22X); Out.String(y); Out.Char(22X); Out.String(y); Out.Ln END tSelfRep."; BEGIN Out.String(x); Out.Char(22X); Out.String(x); Out.Char(22X); Out.String('; y = '); Out.Char(22X); Out.String(y); Out.Char(22X); Out.String(y); Out.Ln END tSelfRep.
>>*)

(*[[
!! (SYMFILE #tSelfRep STAMP #tSelfRep.%main 1 #tSelfRep.m)
!! (CHKSUM STAMP)
!! 
MODULE tSelfRep STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tSelfRep.%main 0 3 0
! MODULE tSelfRep; IMPORT Out; CONST x = "MODULE tSelfRep; IMPORT Out; CONST x = "; y = "; BEGIN Out.String(x); Out.Char(22X); Out.String(x); Out.Char(22X); Out.String('; y = '); Out.Char(22X); Out.String(y); Out.Char(22X); Out.String(y); Out.Ln END tSelfRep."; BEGIN Out.String(x); Out.Char(22X); Out.String(x); Out.Char(22X); Out.String('; y = '); Out.Char(22X); Out.String(y); Out.Char(22X); Out.String(y); Out.Ln END tSelfRep.
CONST 40
GLOBAL tSelfRep.%1
GLOBAL Out.String
CALL 2
CONST 34
ALIGNC
GLOBAL Out.Char
CALL 1
CONST 40
GLOBAL tSelfRep.%1
GLOBAL Out.String
CALL 2
CONST 34
ALIGNC
GLOBAL Out.Char
CALL 1
CONST 7
GLOBAL tSelfRep.%3
GLOBAL Out.String
CALL 2
CONST 34
ALIGNC
GLOBAL Out.Char
CALL 1
CONST 171
GLOBAL tSelfRep.%2
GLOBAL Out.String
CALL 2
CONST 34
ALIGNC
GLOBAL Out.Char
CALL 1
CONST 171
GLOBAL tSelfRep.%2
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

! String "MODULE tSelfRep; IMPORT Out; CONST x = "
DEFINE tSelfRep.%1
STRING 4D4F44554C45207453656C665265703B20494D504F5254204F75743B20434F4E
STRING 53542078203D2000

! String "; BEGIN Out.String(x); Out.Char(22X); Out.String(x); Out.Char(22X); Out.String('; y = '); Out.Char(22X); Out.String(y); Out.Char(22X); Out.String(y); Out.Ln END tSelfRep."
DEFINE tSelfRep.%2
STRING 3B20424547494E204F75742E537472696E672878293B204F75742E4368617228
STRING 323258293B204F75742E537472696E672878293B204F75742E43686172283232
STRING 58293B204F75742E537472696E6728273B2079203D2027293B204F75742E4368
STRING 617228323258293B204F75742E537472696E672879293B204F75742E43686172
STRING 28323258293B204F75742E537472696E672879293B204F75742E4C6E20454E44
STRING 207453656C665265702E00

! String "; y = "
DEFINE tSelfRep.%3
STRING 3B2079203D2000

! End of file
]]*)
