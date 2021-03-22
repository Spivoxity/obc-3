MODULE tProlog;

IMPORT Out;

(* A prolog interpreter running a program that computes tilings *)

(* This program is the output of a macro processor, so contains many
untidy expressions *)


(* tunable parameters *)
CONST
  MAXSYMBOLS = 511;  (* max no. of symbols *)
  HASHFACTOR = 90;  (* percent loading factor for hash table *)
  MAXCHARS = 2048;  (* max chars in symbols *)
  MAXSTRING = 128;  (* max string length *)
  MAXARITY = 63;  (* max arity of function, vars in clause *)
  MEMSIZE = 25000;  (* size of |mem| array *)

(* special character values *)
CONST ENDSTR = CHR(0);  (* end of string *)
  TAB = CHR(9);  (* tab character *)
  ENDLINE = CHR(10);  (* newline character *)
  ENDFILE = CHR(127);  (* end of file *)

VAR run: BOOLEAN;  (* whether execution should continue *)
  dflag: BOOLEAN;  (* switch for debugging code *)

TYPE
  permstring = INTEGER;
  tempstring = ARRAY MAXSTRING OF CHAR;

VAR
  charptr: INTEGER;
  charbuf: ARRAY MAXCHARS OF CHAR;

(* |StringLength| -- length of a tempstring *)
PROCEDURE StringLength(VAR s: tempstring): INTEGER;
  VAR i: INTEGER;
BEGIN
  i := 0;
  WHILE s[i] # ENDSTR DO i := i+1 END;
  RETURN i
END StringLength;

(* |SaveString| -- make a tempstring permanent *)
PROCEDURE SaveString(VAR s: tempstring): permstring;
  VAR p, i: INTEGER;
BEGIN
  IF charptr + StringLength(s) + 1 > MAXCHARS THEN
    Out.Ln(); Out.String("Panic: "); Out.String("out of string space"); Out.Ln(); HALT(2)
  END;
  p := charptr; i := 0;
  REPEAT
    charbuf[charptr] := s[i]; charptr := charptr+1; i := i+1
  UNTIL charbuf[charptr-1] = ENDSTR;
  RETURN p
END SaveString;

(* |StringEqual| -- compare a tempstring to a permstring *)
PROCEDURE StringEqual(VAR s1: tempstring; s2: permstring): BOOLEAN;
  VAR i: INTEGER;
BEGIN
  i := 0;
  WHILE (s1[i] # ENDSTR) & (s1[i] = charbuf[s2+i]) DO i := i+1 END;
  RETURN (s1[i] = charbuf[s2+i])
END StringEqual;

(* |WriteString| -- print a permstring *)
PROCEDURE WriteString(s: permstring);
  VAR i: INTEGER;
BEGIN
  i := s;
  WHILE charbuf[i] # ENDSTR DO
    Out.Char(charbuf[i]); i := i+1
  END
END WriteString;

TYPE
  ptr = INTEGER;  (* index into |mem| array *)

CONST NULL = 0;  (* null pointer *)

TYPE term = ptr;

CONST FUNC = 1;  (* compound term *)
  INT = 2;  (* INTEGER *)
  CHRCTR = 3;  (* character *)
  CELL = 4;  (* variable cell *)
  REF = 5;  (* variable reference *)
  UNDO = 6;  (* trail item *)

CONST TERMSIZE = 2;  (* \dots\ plus no. of args *)

VAR
  lsp, gsp, hp, hmark: ptr;
  mem: ARRAY MEMSIZE+1 OF INTEGER;

(* |LocAlloc| -- allocate space on local stack *)
PROCEDURE LocAlloc(size: INTEGER): ptr;
  VAR p: ptr;
BEGIN
  IF lsp + size >= gsp THEN Out.Ln(); Out.String("Panic: "); Out.String("out of stack space"); Out.Ln(); HALT(2) END;
  p := lsp + 1; lsp := lsp + size; RETURN p
END LocAlloc;

(* |GloAlloc| -- allocate space on global stack *)
PROCEDURE GloAlloc(kind, size: INTEGER): ptr;
  VAR p: ptr;
BEGIN
  IF gsp - size <= lsp THEN
    Out.Ln(); Out.String("Panic: "); Out.String("out of stack space"); Out.Ln(); HALT(2)
  END;
  gsp := gsp - size; p := gsp;
  mem[p] := LSL(kind, 8) + size;
  RETURN p
END GloAlloc;

(* |HeapAlloc| -- allocate space on heap *)
PROCEDURE HeapAlloc(size: INTEGER): ptr;
  VAR p: ptr;
BEGIN
  IF hp + size > MEMSIZE THEN Out.Ln(); Out.String("Panic: "); Out.String("out of heap space"); Out.Ln(); HALT(2) END;
  p := hp + 1; hp := hp + size; RETURN p
END HeapAlloc;

VAR infile: ARRAY 3000 OF CHAR; pin, pout: INTEGER;

PROCEDURE prog(line: ARRAY OF CHAR);
  VAR i: INTEGER;
BEGIN
  FOR i := 0 TO LEN(line)-2 DO
    infile[pin] := line[i]; pin := pin+1
  END;
  infile[pin] := ENDLINE; pin := pin+1
END prog;

PROCEDURE rdchar(VAR ch: CHAR);
BEGIN
  IF pout >= pin THEN
    ch := ENDFILE
  ELSE
    ch := infile[pout]; pout := pout+1
  END
END rdchar;

VAR
  pbchar: CHAR;  (* pushed-back char, else |ENDFILE| *)
  lineno: INTEGER;  (* line number in current file *)

(* |GetChar| -- get a character *)
PROCEDURE GetChar(): CHAR;
  VAR ch: CHAR;
BEGIN
  IF pbchar # ENDFILE THEN
    ch := pbchar; pbchar := ENDFILE
  ELSE
    rdchar(ch);
    IF ch = ENDLINE THEN lineno := lineno+1 END
  END;
  RETURN ch
END GetChar;

(* |PushBack| -- push back a character on the input *)
PROCEDURE PushBack(ch: CHAR);
BEGIN
  pbchar := ch
END PushBack;

TYPE clause = ptr;

CONST CLSIZE = 4;  (* ... plus size of body + 1 *)

TYPE frame = ptr;

CONST FRSIZE = 7;  (* \dots plus space for local variables *)

VAR
  current: ptr;  (* current goal *)
  call: term;  (* |Deref|'ed first literal of goal *)
  goalframe: frame;  (* current stack frame *)
  choice: frame;  (* last choice point *)
  base: frame;  (* frame for original goal *)
  prok: clause;  (* clauses left to try on current goal *)

(* |Deref| -- follow |VAR| and |CELL| pointers *)
PROCEDURE Deref(t: term; e: frame): term;
BEGIN
  IF t = NULL THEN Out.Ln(); Out.String("Panic: "); Out.String("Deref"); Out.Ln(); HALT(2) END;
  IF (LSR(mem[t], 8) = REF) & (e # NULL) THEN
    t := (e+7+(mem[t+1]-1)*TERMSIZE)
  END;
  WHILE (LSR(mem[t], 8) = CELL) & (mem[t+1] # NULL) DO
    t := mem[t+1]
  END;
  RETURN t
END Deref;

TYPE symbol = INTEGER;  (* index in |symtab| *)

VAR
  nsymbols: INTEGER;  (* number of symbols *)
  symtab: ARRAY MAXSYMBOLS+1 OF RECORD
      name: INTEGER;  (* print name: index in |charbuf| *)
      arity: INTEGER;  (* number of arguments or -1 *)
      action: INTEGER;  (* code IF built-in, 0 otherwise *)
      prok: clause  (* clause chain *)
    END;
  cons, eqsym, cutsym, nilsym, notsym: symbol;
  node: symbol;

(* |Lookup| -- convert string to internal symbol *)
PROCEDURE Lookup(VAR name: tempstring): symbol;
  VAR h, i: INTEGER; p: symbol;
BEGIN
  (* Compute the hash function in |h| *)
  h := 0; i := 0;
  WHILE name[i] # ENDSTR DO
    h := (5 * h + ORD(name[i])) MOD MAXSYMBOLS; i := i+1 
  END;

  (* Search the hash table *)
  p := h+1;
  WHILE symtab[p].name # -1 DO
    IF StringEqual(name, symtab[p].name) THEN RETURN p END;
    p := p-1;
    IF p = 0 THEN p := MAXSYMBOLS END
  END;

  (* Not found: enter a new symbol *)
  (* Be careful to avoid overflow on 16 bit machines: *)
  IF nsymbols >= (MAXSYMBOLS DIV 10) * (HASHFACTOR DIV 10) THEN
    Out.Ln(); Out.String("Panic: "); Out.String("out of symbol space"); Out.Ln(); HALT(2)
  END;
  symtab[p].name := SaveString(name);
  symtab[p].arity := -1;
  symtab[p].action := 0; symtab[p].prok := NULL;
  RETURN p
END Lookup;

TYPE keyword = ARRAY OF CHAR;

(* |Enter| -- define a built-in symbol *)
PROCEDURE Enter(name: keyword; arity: INTEGER; action: INTEGER): symbol;
  VAR s: symbol; i: INTEGER; temp: tempstring;
BEGIN
  i := 0;
  WHILE name[i] # 0X DO
    temp[i] := name[i]; i := i+1 
  END;
  temp[i] := ENDSTR; s := Lookup(temp);
  symtab[s].arity := arity; symtab[s].action := action;
  RETURN s
END Enter;

(* Codes for built-in relations *)
CONST
  CUT = 1;  (* $!/0$ *)
  CALL = 2;  (* |call/1| *)
  PLUS = 3;  (* |plus/3| *)
  TIMES = 4;  (* |times/3| *)
  ISINT = 5;  (* |integer/1| *)
  ISCHAR = 6;  (* |char/1| *)
  NAFF = 7;  (* |not/1| *)
  EQUALITY = 8;  (* |=/2| *)
  FAIL = 9;  (* |false/0| *)
  PRINT = 10;  (* |print/1| *)
  NL = 11;  (* |nl/0| *)

(* |InitSymbols| -- initialize and define standard symbols *)
PROCEDURE InitSymbols();
  VAR i: INTEGER; dummy: symbol;
BEGIN
  nsymbols := 0;
  FOR i := 1 TO MAXSYMBOLS DO symtab[i].name := -1 END;
  cons   := Enter(":", 2, 0);
  cutsym := Enter("!", 0, CUT);
  eqsym  := Enter("=", 2, EQUALITY);
  nilsym := Enter("nil", 0, 0);
  notsym := Enter("not", 1, NAFF);
  node   := Enter("node", 2, 0);
  dummy  := Enter("call", 1, CALL);
  dummy  := Enter("plus", 3, PLUS);
  dummy  := Enter("times", 3, TIMES);
  dummy  := Enter("integer", 1, ISINT);
  dummy  := Enter("char", 1, ISCHAR);
  dummy  := Enter("false", 0, FAIL);
  dummy  := Enter("print", 1, PRINT);
  dummy  := Enter("nl", 0, NL)
END InitSymbols;

(* |AddClause| -- insert a clause at the end of its chain *)
PROCEDURE AddClause(c: clause);
  VAR s: symbol; p: clause;
BEGIN
  s := mem[mem[c+3]+1];
  IF symtab[s].action # 0 THEN
    Out.Ln(); Out.String("Error: "); Out.String("cannot add clauses to built-in relation "); run := FALSE;
    WriteString(symtab[s].name)
  ELSIF symtab[s].prok = NULL THEN
    symtab[s].prok := c
  ELSE
    p := symtab[s].prok;
    WHILE mem[p+2] # NULL DO p := mem[p+2] END;
    mem[p+2] := c
  END
END AddClause;

TYPE argbuf = ARRAY MAXARITY+1 OF term;

(* |MakeCompound| -- construct a compound term on the heap *)
PROCEDURE MakeCompound(fun: symbol; VAR arg: argbuf): term;
  VAR p: term; i, n: INTEGER;
BEGIN
  n := symtab[fun].arity;
  p := HeapAlloc(TERMSIZE+n);
  mem[p] := LSL(FUNC, 8) + TERMSIZE+n;
  mem[p+1] := fun;
  FOR i := 1 TO n DO mem[p+i+1] := arg[i] END;
  RETURN p
END MakeCompound;

(* |MakeNode| -- construct a compound term with up to 2 arguments *)
PROCEDURE MakeNode(fun: symbol; a1, a2: term): term;
  VAR arg: argbuf;
BEGIN
  arg[1] := a1; arg[2] := a2;
  RETURN MakeCompound(fun, arg)
END MakeNode;

VAR refnode: ARRAY MAXARITY+1 OF term;

(* |MakeRef| -- return a reference cell prepared earlier *)
PROCEDURE MakeRef(offset: INTEGER): term;
BEGIN
  RETURN refnode[offset]
END MakeRef;

(* |MakeInt| -- construct an integer node on the heap *)
PROCEDURE MakeInt(i: INTEGER): term;
  VAR p: term;
BEGIN
  p := HeapAlloc(TERMSIZE);
  mem[p] := LSL(INT, 8) + TERMSIZE;
  mem[p+1] := i; RETURN p
END MakeInt;

(* |MakeChar| -- construct a character node on the heap *)
PROCEDURE MakeChar(c: CHAR): term;
  VAR p: term;
BEGIN
  p := HeapAlloc(TERMSIZE);
  mem[p] := LSL(CHRCTR, 8) + TERMSIZE;
  mem[p+1] := ORD(c); RETURN p
END MakeChar;

(* |MakeString| -- construct a string as a Prolog list of chars *)
PROCEDURE MakeString(VAR s: tempstring): term;
  VAR p: term; i: INTEGER;
BEGIN
  i := StringLength(s);
  p := MakeNode(nilsym, NULL, NULL);
  WHILE i > 0 DO
    i := i-1; p := MakeNode(cons, MakeChar(s[i]), p)
  END;
  RETURN p
END MakeString;

(* |MakeClause| -- construct a clause on the heap *)
PROCEDURE MakeClause(nvars: INTEGER; head: term;
		    VAR body: argbuf; nbody: INTEGER): clause;
  VAR p: clause; i: INTEGER;
BEGIN
  p := HeapAlloc(CLSIZE + nbody + 1);
  mem[p] := nvars; mem[p+2] := NULL; mem[p+3] := head;
  FOR i := 1 TO nbody DO mem[(p+4)+i-1] := body[i] END;
  mem[(p+4)+nbody+1-1] := NULL;
  IF head = NULL THEN 
    mem[p+1] := 0
  ELSE 
    mem[p+1] := Key(head, NULL)
  END;
  RETURN p
END MakeClause;

(* operator priorities *)
CONST
  MAXPRIO = 2;  (* isolated term *)
  ARGPRIO = 2;  (* function arguments *)
  EQPRIO = 2;  (* equals sign *)
  CONSPRIO = 1;  (* colon *)

(* |IsString| -- check IF a list represents a string *)
PROCEDURE IsString(t: term; e: frame): BOOLEAN;
  CONST limit = 128;
  VAR i: INTEGER;
BEGIN
  i := 0; t := Deref(t, e);
  WHILE i < limit DO
    IF (LSR(mem[t], 8) # FUNC) OR (mem[t+1] # cons) THEN
      RETURN (LSR(mem[t], 8) = FUNC) & (mem[t+1] = nilsym)
    ELSIF LSR(mem[Deref(mem[t+1+1], e)], 8) # CHRCTR THEN
      RETURN FALSE
    ELSE
      i := i+1; t := Deref(mem[t+2+1], e) 
    END
  END;
  RETURN FALSE
END IsString;

(* |IsList| -- check IF a term is a proper list *)
PROCEDURE IsList(t: term; e: frame): BOOLEAN;
  CONST limit = 128;
  VAR i: INTEGER;
BEGIN
  i := 0; t := Deref(t, e);
  WHILE i < limit DO
    IF (LSR(mem[t], 8) # FUNC) OR (mem[t+1] # cons) THEN
      RETURN (LSR(mem[t], 8) = FUNC) & (mem[t+1] = nilsym)
    ELSE
      i := i+1; t := Deref(mem[t+2+1], e)
    END
  END;
  RETURN FALSE
END IsList;

(* |ShowString| -- print a list as a string *)
PROCEDURE ShowString(t: term; e: frame);
BEGIN
  t := Deref(t, e);
  Out.Char('"');
  WHILE mem[t+1] # nilsym DO
    Out.Char(CHR(mem[Deref(mem[t+1+1], e)+1]));
    t := Deref(mem[t+2+1], e)
  END;
  Out.Char('"')
END ShowString;

(* |PrintCompound| -- print a compound term *)
PROCEDURE PrintCompound(t: term; e: frame; prio: INTEGER);
  VAR f: symbol; i: INTEGER;
BEGIN
  f := mem[t+1];
  IF f = cons THEN
    (* |t| is a list: try printing as a string, or use infix : *)
    IF IsString(t, e) THEN
      ShowString(t, e)
    ELSE
      IF prio < CONSPRIO THEN Out.Char('(') END;
      PrintTerm(mem[t+1+1], e, CONSPRIO-1);
      Out.Char(':');
      PrintTerm(mem[t+2+1], e, CONSPRIO);
      IF prio < CONSPRIO THEN Out.Char(')') END
    END
  ELSIF f = eqsym THEN
    (* |t| is an equation: use infix = *)
    IF prio < EQPRIO THEN Out.Char('(') END;
    PrintTerm(mem[t+1+1], e, EQPRIO-1);
    Out.String(" = ");
    PrintTerm(mem[t+2+1], e, EQPRIO-1);
    IF prio < EQPRIO THEN Out.Char(')') END
  ELSIF f = notsym THEN
    (* |t| is a literal 'not P' *)
    Out.String("not ");
    PrintTerm(mem[t+1+1], e, MAXPRIO)
  ELSIF (f = node) & IsList(mem[t+2+1], e) THEN
    PrintNode(t, e)
  ELSE
    (* use ordinary notation *)
    WriteString(symtab[f].name);
    IF symtab[f].arity > 0 THEN
      Out.Char('(');
      PrintTerm(mem[t+1+1], e, ARGPRIO);
      FOR i := 2 TO symtab[f].arity DO
        Out.String(", ");
        PrintTerm(mem[t+i+1], e, ARGPRIO)
      END;
      Out.Char(')')
    END
  END
END PrintCompound;

(* |PrintNode| -- print and optree node *)
PROCEDURE PrintNode(t: term; e: frame);
  VAR u: term;
BEGIN
  Out.Char('<');
  PrintTerm(mem[t+1+1], e, MAXPRIO);
  u := Deref(mem[t+2+1], e);
  WHILE mem[u+1] # nilsym DO
    Out.String(", ");
    PrintTerm(mem[u+1+1], e, MAXPRIO);
    u := Deref(mem[u+2+1], e)
  END;
  Out.Char('>');
END PrintNode;

(* |PrintTerm| -- print a term *)
PROCEDURE PrintTerm(t: term; e: frame; prio: INTEGER);
BEGIN
  t := Deref(t, e);
  IF t = NULL THEN
    Out.String("*null-term*")
  ELSE
    CASE LSR(mem[t], 8) OF
      FUNC:
        PrintCompound(t, e, prio)
    | INT:
        Out.Int(mem[t+1], 0)
    | CHRCTR:
        Out.Char("'"); Out.Char(CHR(mem[t+1])); Out.Char("'")
    | CELL:
        IF (t >= gsp) THEN
          Out.Char('G'); Out.Int((MEMSIZE - t) DIV TERMSIZE, 0)
        ELSE
          Out.Char('L'); Out.Int((t - hp) DIV TERMSIZE, 0)
        END
    | REF:
        Out.Char('@'); Out.Int(mem[t+1], 0)
    ELSE
      Out.String("*unknown-term(tag="); 
      Out.Int(LSR(mem[t], 8), 0); Out.String(")*")
    END
  END
END PrintTerm;

(* |PrintClause| -- print a clause *)
PROCEDURE PrintClause(c: clause);
  VAR i: INTEGER;
BEGIN
  IF c = NULL THEN
    Out.String("*null-clause*"); Out.Ln();
  ELSE
    IF mem[c+3] # NULL THEN
      PrintTerm(mem[c+3], NULL, MAXPRIO);
      Out.Char(' ')
    END;
    Out.String(":- ");
    IF mem[(c+4)+1-1] # NULL THEN
      PrintTerm(mem[(c+4)+1-1], NULL, MAXPRIO);
      i := 2;
      WHILE mem[(c+4)+i-1] # NULL DO
	Out.String(", ");
	PrintTerm(mem[(c+4)+i-1], NULL, MAXPRIO);
	i := i+1
      END
    END;
    Out.Char('.'); Out.Ln()
  END
END PrintClause;

VAR
  token: INTEGER;  (* last token from input *)
  tokval: symbol;  (* IF |token = IDENT|, the identifier*)
  tokival: INTEGER;  (* IF |token = NUMBER|, the number *)
  toksval: tempstring;  (* IF |token = STRCON|, the string *)
  errflag: BOOLEAN;  (* whether recovering from an error *)
  errcount: INTEGER;  (* number of errors found so far *)

(* Possible values for |token|: *)
CONST
  IDENT = 1;  (* identifier: see |tokval| *)
  VARIABLE = 2;  (* variable: see |tokval| *)
  NUMBER = 3;  (* number: see |tokival| *)
  CHCON = 4;  (* char constant: see |tokival| *)
  STRCON = 5;  (* string constant: see |toksval| *)
  ARROW = 6;  (* |':-'| *)
  LPAR = 7;  (* |'('| *)
  RPAR = 8;  (* |')'| *)
  COMMA = 9;  (* |','| *)
  DOT = 10;  (* |'.'| *)
  COLON = 11;  (* |':'| *)
  EQUAL = 12;  (* |'='| *)
  NEGATE = 13;  (* |'not'| *)
  EOFTOK = 14;  (* end of file *)
  LANGLE = 15;  (* |'<'| *)
  RANGLE = 16;  (* |'>'| *)
  HASH = 17;  (* |'#'| *)

(* |ShowError| -- report error location *)
PROCEDURE ShowError();
BEGIN
  errflag := TRUE; errcount := errcount+1;
  Out.String("Line "); Out.Int(lineno, 0); Out.Char(' ');
  Out.String("Syntax error - ")
END ShowError;

(* |Recover| -- discard rest of input clause *)
PROCEDURE Recover();
  VAR ch: CHAR;
BEGIN
  IF errcount >= 20 THEN
    Out.String("Too many errors: I am giving up"); Out.Ln(); HALT(2) 
  END;
  IF token # DOT THEN
    REPEAT
      ch := GetChar()
    UNTIL (ch = '.') OR (ch = ENDFILE);
    token := DOT
  END
END Recover;

(* |Scan| -- read one symbol from |infile| into |token|. *)
PROCEDURE Scan();
  VAR ch, ch2: CHAR; i: INTEGER;
BEGIN
  ch := GetChar(); token := 0;
  WHILE token = 0 DO
    (* Loop after white-space or comment *)
    IF ch = ENDFILE THEN
      token := EOFTOK
    ELSIF (ch = ' ') OR (ch = TAB) OR (ch = ENDLINE) THEN
      ch := GetChar()
    ELSIF ((((ch >= 'A') & (ch <= 'Z')) OR (ch = '_')) OR ((ch >= 'a') & (ch <= 'z'))) THEN
      IF (((ch >= 'A') & (ch <= 'Z')) OR (ch = '_')) THEN 
	 token := VARIABLE
      ELSE 
	 token := IDENT
      END;
      i := 0;
      WHILE ((((ch >= 'A') & (ch <= 'Z')) OR (ch = '_')) OR ((ch >= 'a') & (ch <= 'z'))) OR ((ch >= '0') & (ch <= '9')) DO
        IF i > MAXSTRING THEN
          Out.Ln(); Out.String("Panic: "); Out.String("identifier too long"); Out.Ln(); HALT(2)
	END;
        toksval[i] := ch; ch := GetChar(); i := i+1
      END;
      PushBack(ch);
      toksval[i] := ENDSTR; tokval := Lookup(toksval);
      IF tokval = notsym THEN token := NEGATE END
    ELSIF ((ch >= '0') & (ch <= '9')) THEN
      token := NUMBER; tokival := 0;
      WHILE ((ch >= '0') & (ch <= '9')) DO
        tokival := 10 * tokival + (ORD(ch) - ORD('0'));
        ch := GetChar()
      END;
      PushBack(ch)
    ELSE
      CASE ch OF
        '(': token := LPAR
      | ')': token := RPAR
      | ',': token := COMMA
      | '.': token := DOT
      | '=': token := EQUAL
      | '<': token := LANGLE
      | '>': token := RANGLE
      | '#': token := HASH
      | '!': token := IDENT; tokval := cutsym
      | '/':
	  ch := GetChar();
	  IF ch # '*' THEN
	    IF ~errflag THEN ShowError(); Out.String("bad token /"); Out.Ln(); Recover() END
	  ELSE
	    ch2 := ' '; ch := GetChar();
	    WHILE (ch # ENDFILE) & ~((ch2 = '*') & (ch = '/')) DO
	      ch2 := ch; ch := GetChar() 
	    END;
	    IF ch = ENDFILE THEN
	      IF ~errflag THEN ShowError(); Out.String("end of file in comment"); Out.Ln(); Recover() END
	    ELSE
	      ch := GetChar()
	    END
	  END
      | ':':
	  ch := GetChar();
	  IF ch = '-' THEN
	    token := ARROW
	  ELSE
	    PushBack(ch); token := COLON 
	  END
      | "'":
	  token := CHCON; tokival := ORD(GetChar()); ch := GetChar();
	  IF ch # "'" THEN IF ~errflag THEN ShowError(); Out.String("missing quote"); Out.Ln(); Recover() END END
      | '"':
	  token := STRCON; i := 0; ch := GetChar();
	  WHILE (ch # '"') & (ch # ENDLINE) DO
	    toksval[i] := ch; ch := GetChar(); i := i+1 
	  END;
	  toksval[i] := ENDSTR;
	  IF ch = ENDLINE THEN
	    IF ~errflag THEN ShowError(); Out.String("unterminated string"); Out.Ln(); Recover() END;
	    PushBack(ch)
	  END
      ELSE
	IF ~errflag THEN ShowError(); Out.String("illegal character"); Out.Ln(); Recover() END; Out.Char(ch); Out.Ln()
      END
    END
  END
END Scan;

(* |PrintToken| -- print a token as a string *)
PROCEDURE PrintToken(t: INTEGER);
BEGIN
  CASE t OF
    IDENT:
      Out.String("identifier "); WriteString(symtab[tokval].name)
  | VARIABLE:
      Out.String("variable "); WriteString(symtab[tokval].name)
  | NUMBER: Out.String("number");
  | CHCON:  Out.String("char constant");
  | ARROW:  Out.String(":-");
  | LPAR:   Out.String("(");
  | RPAR:   Out.String(")");
  | COMMA:  Out.String(",");
  | DOT:    Out.String(".");
  | COLON:  Out.String(":");
  | EQUAL:  Out.String("=");
  | STRCON: Out.String("string constant")
  | LANGLE: Out.String("<")
  | RANGLE: Out.String(">")
  | HASH:   Out.String("#")
  ELSE
    Out.String("unknown token")
  END
END PrintToken;

VAR
  nvars: INTEGER;  (* no. of variables so far *)
  vartable: ARRAY MAXARITY+1 OF symbol;  (* names of the variables *)

(* |VarRep| -- look up a variable name *)
PROCEDURE VarRep(name: symbol): term;
  VAR i: INTEGER;
BEGIN
  IF nvars = MAXARITY THEN Out.Ln(); Out.String("Panic: "); Out.String("too many variables"); Out.Ln(); HALT(2) END;
  i := 1; vartable[nvars+1] := name;  (* sentinel *)
  WHILE name # vartable[i] DO i := i+1 END;
  IF i = nvars+1 THEN nvars := nvars+1 END;
  RETURN MakeRef(i)
END VarRep;

(* |ShowAnswer| -- display answer and get response *)
PROCEDURE ShowAnswer(bindings: frame);
  VAR i: INTEGER;
BEGIN
  IF nvars = 0 THEN
    Out.String("yes"); Out.Ln()
  ELSE
    FOR i := 1 TO nvars DO
      WriteString(symtab[vartable[i]].name); Out.String(" = ");
      PrintTerm((bindings+7+(i-1)*TERMSIZE), NULL, EQPRIO-1);
      Out.Ln()
    END
  END
END ShowAnswer;

(* |Eat| -- check for an expected token and discard it *)
PROCEDURE Eat(expected: INTEGER);
BEGIN
  IF token = expected THEN
    IF token # DOT THEN Scan() END
  ELSIF ~errflag THEN
    ShowError();
    Out.String("expected "); PrintToken(expected);
    Out.String(", found "); PrintToken(token); Out.Ln();
    Recover()
  END
END Eat;

(* |ParseCompound| -- parse a compound term *)
PROCEDURE ParseCompound(): term;
  VAR fun: symbol; arg: argbuf; n: INTEGER;
BEGIN
  fun := tokval; n := 0; Eat(IDENT);
  IF token = LPAR THEN
    Eat(LPAR); n := 1; arg[1] := ParseTerm();
    WHILE token = COMMA DO
      Eat(COMMA); n := n+1; arg[n] := ParseTerm()
    END;
    Eat(RPAR)
  END;
  IF symtab[fun].arity = -1 THEN
    symtab[fun].arity := n
  ELSIF symtab[fun].arity # n THEN
    IF ~errflag THEN ShowError(); Out.String("wrong number of args"); Out.Ln(); Recover() END
  END;
  RETURN MakeCompound(fun, arg)
END ParseCompound;

(* |ParsePrimary| -- parse a primary *)
PROCEDURE ParsePrimary(): term;
  VAR t: term;
BEGIN
  IF token = IDENT THEN t := ParseCompound()
  ELSIF token = VARIABLE THEN
    t := VarRep(tokval); Eat(VARIABLE)
  ELSIF token = NUMBER THEN
    t := MakeInt(tokival); Eat(NUMBER)
  ELSIF token = CHCON THEN
    t := MakeChar(CHR(tokival)); Eat(CHCON)
  ELSIF token = STRCON THEN
    t := MakeString(toksval); Eat(STRCON)
  ELSIF token = LPAR THEN
    Eat(LPAR); t := ParseTerm(); Eat(RPAR)
  ELSIF token = LANGLE THEN
    t := ParseNode()
  ELSE
    IF ~errflag THEN ShowError(); Out.String("expected a term"); Out.Ln(); Recover() END; t := NULL
  END;
  RETURN t
END ParsePrimary;

(* |ParseNode| -- parse an optree node *)
PROCEDURE ParseNode(): term;
  VAR tag, kids: term;
BEGIN
  Eat(LANGLE);
  tag := ParseTerm();
  kids := ParseKids();
  Eat(RANGLE);
  RETURN MakeNode(node, tag, kids)
END ParseNode;

(* |ParseKids| -- parse children of an optree node *)
PROCEDURE ParseKids(): term;
  VAR head, tail: term;
BEGIN
  IF token # COMMA THEN
    RETURN MakeNode(nilsym, NULL, NULL)
  ELSE
    Eat(COMMA);
    head := ParseTerm();
    tail := ParseKids();
    RETURN MakeNode(cons, head, tail)
  END
END ParseKids;    

(* |ParseFactor| -- parse a factor *)
PROCEDURE ParseFactor(): term;
  VAR t: term;
BEGIN
  t := ParsePrimary();
  IF token # COLON THEN
    RETURN t
  ELSE
    Eat(COLON);
    RETURN MakeNode(cons, t, ParseFactor())
  END
END ParseFactor;

(* |ParseTerm| -- parse a term *)
PROCEDURE ParseTerm(): term;
  VAR t: term;
 BEGIN
  t := ParseFactor();
  IF token # EQUAL THEN
    RETURN t
  ELSE
    Eat(EQUAL);
    RETURN MakeNode(eqsym, t, ParseFactor())
  END
END ParseTerm;

(* |CheckAtom| -- check that a literal is a compound term *)
PROCEDURE CheckAtom(a: term);
BEGIN
  IF LSR(mem[a], 8) # FUNC THEN
    IF ~errflag THEN ShowError(); Out.String("literal must be a compound term"); Out.Ln(); Recover() END
  END
END CheckAtom;

(* |ParseClause| -- parse a clause *)
PROCEDURE ParseClause(): clause;
  VAR head, t: term; 
    body: argbuf; 
    n: INTEGER;
    minus, more: BOOLEAN;
BEGIN
  IF token = HASH THEN
    Eat(HASH); head := NULL
  ELSE
    head := ParseTerm();
    CheckAtom(head)
  END;
  Eat(ARROW);
  n := 0;
  IF token # DOT THEN
    more := TRUE;
    WHILE more DO
      n := n+1; minus := FALSE;
      IF token = NEGATE THEN
	Eat(NEGATE); minus := TRUE 
      END;
      t := ParseTerm(); CheckAtom(t);
      IF minus THEN 
	body[n] := MakeNode(notsym, t, NULL)
      ELSE 
        body[n] := t
      END;
      IF token = COMMA THEN Eat(COMMA) ELSE more := FALSE END
    END
  END;
  Eat(DOT);

  IF errflag THEN 
    RETURN NULL
  ELSE 
    RETURN MakeClause(nvars, head, body, n)
  END
END ParseClause;

(* |ReadClause| -- read a clause from |infile| *)
PROCEDURE ReadClause(): clause;
  VAR c: clause;
BEGIN
  REPEAT
    hp := hmark; nvars := 0; errflag := FALSE;
    Scan();
    IF token = EOFTOK THEN 
      c := NULL
    ELSE 
      c := ParseClause()
    END
  UNTIL (~errflag) OR (token = EOFTOK);
  RETURN c
END ReadClause;

TYPE trail = ptr;

CONST TRAILSIZE = 3;

VAR trhead: trail;  (* start of the trail *)

(* |Save| -- add a variable to the trail IF it is critical *)
PROCEDURE Save(v: term);
  VAR p: trail;
BEGIN
  IF ((v < choice) OR (v >= mem[choice+4])) THEN
    p := GloAlloc(UNDO, TRAILSIZE);
    mem[p+1] := v; mem[p+2] := trhead; trhead := p
  END
END Save;

(* |Restore| -- undo bindings back to previous state *)
PROCEDURE Restore();
  VAR v: term;
BEGIN
  WHILE (trhead # mem[choice+5]) DO
    v := mem[trhead+1];
    IF v # NULL THEN mem[v+1] := NULL END;
    trhead := mem[trhead+2]
  END
END Restore;

(* |Commit| -- blank out trail entries not needed after cut *)
PROCEDURE Commit();
  VAR p: trail;
BEGIN
  p := trhead;
  WHILE (p # NULL) & (p < mem[choice+4]) DO
    IF (mem[p+1] # NULL) & ~((mem[p+1] < choice) OR (mem[p+1] >= mem[choice+4])) THEN
      mem[p+1] := NULL
    END;
    p := mem[p+2]
  END
END Commit;

(* |GloCopy| -- copy a term onto the global stack *)
PROCEDURE GloCopy(t: term; e: frame): term;
  VAR tt: term; i, n: INTEGER;
BEGIN
  t := Deref(t, e);
  IF (t >= gsp) THEN
    RETURN t
  ELSE
    CASE LSR(mem[t], 8) OF
      FUNC:
	n := symtab[mem[t+1]].arity;
	IF (t <= hp) & (n = 0) THEN 
	  RETURN t
	ELSE
	  tt := GloAlloc(FUNC, TERMSIZE+n);
	  mem[tt+1] := mem[t+1];
	  FOR i := 1 TO n DO
	    mem[tt+i+1] := GloCopy(mem[t+i+1], e)
	  END;
	  RETURN tt
	END
    | CELL:
        tt := GloAlloc(CELL, TERMSIZE);
        mem[tt+1] := NULL;
	Save(t); mem[t+1] := tt;
        RETURN tt
    ELSE
      RETURN t
    END
  END
END GloCopy;

(* |Share| -- bind two variables together *)
PROCEDURE Share(v1, v2: term);
BEGIN
  IF (v1 * (2 * ORD((v1 >= gsp)) - 1)) <= (v2 * (2 * ORD((v2 >= gsp)) - 1)) THEN
    Save(v1); mem[v1+1] := v2
  ELSE
    Save(v2); mem[v2+1] := v1 
  END
END Share;

(* |Unify| -- find and apply unifier for two terms *)
PROCEDURE Unify(t1: term; e1: frame; t2: term; e2: frame): BOOLEAN;
  VAR i: INTEGER; match: BOOLEAN;
BEGIN
  t1 := Deref(t1, e1); t2 := Deref(t2, e2);
  IF t1 = t2 THEN  (* Includes unifying a VAR with itself *)
    RETURN TRUE
  ELSIF (LSR(mem[t1], 8) = CELL) & (LSR(mem[t2], 8) = CELL) THEN
    Share(t1, t2); RETURN TRUE
  ELSIF LSR(mem[t1], 8) = CELL THEN
    Save(t1); mem[t1+1] := GloCopy(t2, e2); RETURN TRUE
  ELSIF LSR(mem[t2], 8) = CELL THEN
    Save(t2); mem[t2+1] := GloCopy(t1, e1); RETURN TRUE
  ELSIF LSR(mem[t1], 8) # LSR(mem[t2], 8) THEN
    RETURN FALSE
  ELSE
    CASE LSR(mem[t1], 8) OF
      FUNC:
        IF (mem[t1+1] # mem[t2+1]) THEN
          RETURN FALSE
        ELSE
          i := 1; match := TRUE;
          WHILE match & (i <= symtab[mem[t1+1]].arity) DO
            match := Unify(mem[t1+i+1], e1, mem[t2+i+1], e2);
            i := i+1
          END;
          RETURN match
        END
    | INT:
        RETURN (mem[t1+1] = mem[t2+1])
    | CHRCTR:
        RETURN (mem[t1+1] = mem[t2+1])
    ELSE
      Out.Ln(); Out.String("Panic: "); Out.String("bad tag" (*t_kind(t1):1, " in ", "Unify"*)); Out.Ln(); HALT(2)
    END
  END
END Unify;

(* |Key| -- unification key of a term *)
PROCEDURE Key(t: term; e: frame): INTEGER;
  VAR t0: term;
BEGIN
  (* The argument |t| must be a direct pointer to a compound term.
    The value returned is |key(t)|: IF |t1| and |t2| are unifiable,
    THEN |key(t1) = 0| or |key(t2) = 0| or |key(t1) = key(t2)|. *)

  IF t = NULL THEN Out.Ln(); Out.String("Panic: "); Out.String("Key"); Out.Ln(); HALT(2) END;
  IF LSR(mem[t], 8) # FUNC THEN Out.Ln(); Out.String("Panic: "); Out.String("bad tag" (*t_kind(t):1, " in ", "Key1"*)); Out.Ln(); HALT(2) END;

  IF symtab[mem[t+1]].arity = 0 THEN
    RETURN 0
  ELSE
    t0 := Deref(mem[t+1+1], e);
    CASE LSR(mem[t0], 8) OF
        FUNC:      RETURN mem[t0+1]
      | INT:       RETURN mem[t0+1] + 1
      | CHRCTR:    RETURN mem[t0+1] + 1
    ELSE
      RETURN 0
    END
  END
END Key;

(* |Search| -- find the first clause that might match *)
PROCEDURE Search(t: term; e: frame; p: clause): clause;
  VAR k: INTEGER;
BEGIN
  k := Key(t, e);
  IF k # 0 THEN
    WHILE (p # NULL) & (mem[p+1] # 0) & (mem[p+1] # k) DO
      p := mem[p+2]
    END
  END;
  RETURN p
END Search;

VAR ok: BOOLEAN;  (* whether execution succeeded *)

(* |PushFrame| -- create a new local stack frame *)
PROCEDURE PushFrame(nvars: INTEGER; retry: clause);
  VAR f: frame; i: INTEGER;
BEGIN
  f := LocAlloc((FRSIZE + (nvars)*TERMSIZE));
  mem[f] := current; mem[f+1] := goalframe;
  mem[f+2] := retry; mem[f+3] := choice;
  mem[f+4] := gsp; mem[f+5] := trhead;
  mem[f+6] := nvars;
  FOR i := 1 TO nvars DO
    mem[(f+7+(i-1)*TERMSIZE)] := LSL(CELL, 8) + TERMSIZE;
    mem[(f+7+(i-1)*TERMSIZE)+1] := NULL
  END;
  goalframe := f;
  IF retry # NULL THEN choice := goalframe END
END PushFrame;

(* |TroStep| -- perform a resolution step with tail-recursion *)
PROCEDURE TroStep();
  VAR temp: frame; oldsize, newsize, i: INTEGER;
BEGIN
  IF dflag THEN Out.String("(TRO)"); Out.Ln() END;

  oldsize := (FRSIZE + (mem[goalframe+6])*TERMSIZE); (* size of old frame *)
  newsize := (FRSIZE + (mem[prok])*TERMSIZE); (* size of new frame *)
  temp := LocAlloc(newsize);
  temp := goalframe + newsize; (* copy old frame here *)

  (* Copy the old frame: in reverse order in case of overlap *)
  FOR i := 1 TO oldsize DO 
    mem[temp+oldsize-i] := mem[goalframe+oldsize-i]
  END;

  (* Adjust internal pointers in the copy *)
  FOR i := 1 TO mem[goalframe+6] DO
    IF (LSR(mem[(temp+7+(i-1)*TERMSIZE)], 8) = CELL)
        & (mem[(temp+7+(i-1)*TERMSIZE)+1] # NULL)
        & (goalframe <= mem[(temp+7+(i-1)*TERMSIZE)+1])
        & (mem[(temp+7+(i-1)*TERMSIZE)+1] < goalframe + oldsize) THEN
      mem[(temp+7+(i-1)*TERMSIZE)+1] := mem[(temp+7+(i-1)*TERMSIZE)+1] + newsize
    END
  END;

  (* Overwrite the old frame with the new one *)
  mem[goalframe+6] := mem[prok];
  FOR i := 1 TO mem[goalframe+6] DO
    mem[(goalframe+7+(i-1)*TERMSIZE)] := LSL(CELL, 8) + TERMSIZE;
    mem[(goalframe+7+(i-1)*TERMSIZE)+1] := NULL
  END;

  (* Perform the resolution step *)
  ok := Unify(call, temp, mem[prok+3], goalframe);
  current := (prok+4);
  lsp := temp-1
END TroStep;

(* |Step| -- perform a resolution step *)
PROCEDURE Step();
  VAR retry: clause;
BEGIN
  IF symtab[mem[call+1]].action # 0 THEN
    ok := DoBuiltin(symtab[mem[call+1]].action)
  ELSIF prok = NULL THEN
    ok := FALSE
  ELSE
    retry := Search(call, goalframe, mem[prok+2]);
    IF (mem[(current)+1] = NULL) & (choice < goalframe)
    & (retry = NULL) & (goalframe # base) THEN
      TroStep()
    ELSE
      PushFrame(mem[prok], retry);
      ok := Unify(call, mem[goalframe+1], mem[prok+3], goalframe);
      current := (prok+4);
    END
  END
END Step;

(* |Unwind| -- return from completed clauses *)
PROCEDURE Unwind();
BEGIN
  WHILE (mem[current] = NULL) & (goalframe # base) DO
    IF dflag THEN 
    Out.String("Exit"); Out.String(": "); 
    PrintTerm(mem[mem[goalframe]], mem[goalframe+1], MAXPRIO); Out.Ln()
  END;
    current := (mem[goalframe])+1;
    IF goalframe > choice THEN lsp := goalframe-1 END;
    goalframe := mem[goalframe+1]
  END
END Unwind;

(* |Backtrack| -- roll back to the last choice-point *)
PROCEDURE Backtrack();
BEGIN
  Restore();
  current := mem[choice]; goalframe := mem[choice+1];
  call := Deref(mem[current], goalframe);
  prok := mem[choice+2]; gsp := mem[choice+4];
  lsp := choice-1; choice := mem[choice+3];
  IF dflag THEN 
    Out.String("Redo"); Out.String(": "); 
    PrintTerm(call, goalframe, MAXPRIO); Out.Ln()
  END;
END Backtrack;

(* |Resume| -- continue execution *)
PROCEDURE Resume(flag: BOOLEAN);
BEGIN
  ok := flag;
  WHILE run DO
    IF ok THEN
      IF mem[current] = NULL THEN RETURN END;
      call := Deref(mem[current], goalframe);
      IF dflag THEN 
    Out.String("Call"); Out.String(": "); 
    PrintTerm(call, goalframe, MAXPRIO); Out.Ln()
  END;
      IF (symtab[mem[call+1]].prok = NULL)
	  & (symtab[mem[call+1]].action = 0) THEN
	Out.Ln(); Out.String("Error: "); Out.String("call to undefined relation "); run := FALSE;
	WriteString(symtab[mem[call+1]].name);
	RETURN
      END;
      prok := Search(call, goalframe, symtab[mem[call+1]].prok)
    ELSE
      IF choice <= base THEN RETURN END;
      Backtrack()
    END;
    Step();
    IF ok THEN Unwind() END;
  END;
END Resume;

(* |Execute| -- solve a goal by SLD-resolution *)
PROCEDURE Execute(g: clause);
  VAR nsoln: INTEGER;
BEGIN
  lsp := hp; gsp := MEMSIZE+1; nsoln := 0;
  current := NULL; goalframe := NULL; choice := NULL; trhead := NULL;
  PushFrame(mem[g], NULL);
  choice := goalframe; base := goalframe; current := (g+4);
  run := TRUE;
  Resume(TRUE);
  IF ~run THEN RETURN END;
  WHILE ok DO
    nsoln := nsoln+1;
    ShowAnswer(base);
    Out.Ln();
    Resume(FALSE);
    IF ~run THEN RETURN END;
  END;

  IF nsoln = 0 THEN
    Out.String("no"); Out.Ln(); Out.Ln();
  END
END Execute;

VAR
  av: argbuf;  (* |GetArgs| puts arguments here *)
  callbody: ptr;  (* dummy clause body used by |call/1| *)

(* |GetArgs| -- set up |av| array *)
PROCEDURE GetArgs();
  VAR i: INTEGER;
BEGIN
  FOR i := 1 TO symtab[mem[call+1]].arity DO
    av[i] := Deref(mem[call+i+1], goalframe)
  END
END GetArgs;

PROCEDURE NewInt(n: INTEGER): term;
  VAR t: term;
BEGIN
  t := GloAlloc(INT, TERMSIZE);
  mem[t+1] := n;
  RETURN t
END NewInt;

(* |DoCut| -- built-in relation !/0 *)
PROCEDURE DoCut(): BOOLEAN;
BEGIN
  choice := mem[goalframe+3];
  lsp := goalframe + (FRSIZE + (mem[goalframe+6])*TERMSIZE) - 1;
  Commit();
  current := (current)+1;
  RETURN TRUE
END DoCut;

(* |DoCall| -- built-in relation |call/1| *)
PROCEDURE DoCall(): BOOLEAN;
BEGIN
  GetArgs();
  IF ~(LSR(mem[av[1]], 8) = FUNC) THEN
    Out.Ln(); Out.String("Error: "); Out.String("bad argument to call/1"); run := FALSE;
    RETURN FALSE
  ELSE
    PushFrame(1, NULL);
    mem[(goalframe+7+(1-1)*TERMSIZE)+1] :=
      GloCopy(av[1], mem[goalframe+1]);
    current := callbody;
    RETURN TRUE
  END
END DoCall;

(* |DoNot| -- built-in relation |not/1| *)
PROCEDURE DoNot(): BOOLEAN;
  VAR savebase: frame;
BEGIN
  GetArgs();
  IF ~(LSR(mem[av[1]], 8) = FUNC) THEN
    Out.Ln(); Out.String("Error: "); Out.String("bad argument to call/1"); run := FALSE;
    RETURN FALSE
  ELSE
    PushFrame(1, NULL);
    savebase := base; base := goalframe; choice := goalframe;
    mem[(goalframe+7+(1-1)*TERMSIZE)+1] :=
      GloCopy(av[1], mem[goalframe+1]);
    current := callbody;
    Resume(TRUE);
    choice := mem[base+3]; goalframe := mem[base+1];
    IF ~ok THEN
      current := (mem[base])+1;
      RETURN TRUE
    ELSE
      Commit();
      RETURN FALSE
    END;
    lsp := base-1; base := savebase
  END
END DoNot;

(* |DoPlus| -- built-in relation |plus/3| *)
PROCEDURE DoPlus(): BOOLEAN;
  VAR result: BOOLEAN;
BEGIN
  GetArgs();
  result := FALSE;
  IF (LSR(mem[av[1]], 8) = INT) & (LSR(mem[av[2]], 8) = INT) THEN
    result := Unify(av[3], goalframe, NewInt(mem[av[1]+1] + mem[av[2]+1]), NULL)
  ELSIF (LSR(mem[av[1]], 8) = INT) & (LSR(mem[av[3]], 8) = INT) THEN
    IF mem[av[1]+1] <= mem[av[3]+1] THEN
      result := Unify(av[2], goalframe, 
		      NewInt(mem[av[3]+1] - mem[av[1]+1]), NULL)
    END
  ELSIF (LSR(mem[av[2]], 8) = INT) & (LSR(mem[av[3]], 8) = INT) THEN
    IF mem[av[2]+1] <= mem[av[3]+1] THEN
      result := Unify(av[1], goalframe, NewInt(mem[av[3]+1] - mem[av[2]+1]), NULL)
    END
  ELSE
    Out.Ln(); Out.String("Error: "); Out.String("plus/3 needs at least two integers"); run := FALSE
  END;
  current := (current)+1;
  RETURN result
END DoPlus;

(* |DoTimes| -- built-in relation |times/3| *)
PROCEDURE DoTimes(): BOOLEAN;
  VAR result: BOOLEAN;
BEGIN
  GetArgs();
  result := FALSE;
  IF (LSR(mem[av[1]], 8) = INT) & (LSR(mem[av[2]], 8) = INT) THEN
    result := Unify(av[3], goalframe, 
		    NewInt(mem[av[1]+1] * mem[av[2]+1]), NULL)
  ELSIF (LSR(mem[av[1]], 8) = INT) & (LSR(mem[av[3]], 8) = INT) THEN
    IF mem[av[1]+1] # 0 THEN
      IF mem[av[3]+1] MOD mem[av[1]+1] = 0 THEN
        result := Unify(av[2], goalframe, 
		        NewInt(mem[av[3]+1] DIV mem[av[1]+1]), NULL)
      END
    END
  ELSIF (LSR(mem[av[2]], 8) = INT) & (LSR(mem[av[3]], 8) = INT) THEN
    IF mem[av[2]+1] # 0 THEN
      IF mem[av[3]+1] MOD mem[av[2]+1] = 0 THEN
        result := Unify(av[1], goalframe, 
			NewInt(mem[av[3]+1] DIV mem[av[2]+1]), NULL)
      END
    END
  ELSE
    Out.Ln(); Out.String("Error: "); Out.String("times/3 needs at least two integers"); run := FALSE
  END;
  current := (current)+1;
  RETURN result
END DoTimes;

(* |DoEqual| -- built-in relation |=/2| *)
PROCEDURE DoEqual(): BOOLEAN;
BEGIN
  GetArgs();
  current := (current)+1;
  RETURN Unify(av[1], goalframe, av[2], goalframe)
END DoEqual;

(* |DoInteger| -- built-in relation |integer/1| *)
PROCEDURE DoInteger(): BOOLEAN;
BEGIN
  GetArgs();
  current := (current)+1;
  RETURN (LSR(mem[av[1]], 8) = INT)
END DoInteger;

(* |DoChar| -- built-in relation |char/1| *)
PROCEDURE DoChar(): BOOLEAN;
BEGIN
  GetArgs();
  current := (current)+1;
  RETURN (LSR(mem[av[1]], 8) = CHRCTR)
END DoChar;

(* |DoPrint| -- built-in relation |print/1| *)
PROCEDURE DoPrint(): BOOLEAN;
BEGIN
  GetArgs();
  PrintTerm(av[1], goalframe, MAXPRIO);
  current := (current)+1;
  RETURN TRUE
END DoPrint;

(* |DoNl| -- built-in relation |nl/0| *)
PROCEDURE DoNl(): BOOLEAN;
BEGIN
  Out.Ln();
  current := (current)+1;
  RETURN TRUE
END DoNl;  

(* |DoBuiltin| -- switch for built-in relations *)
PROCEDURE DoBuiltin(action: INTEGER): BOOLEAN;
BEGIN
  CASE action OF
    CUT:      RETURN DoCut()
  | CALL:     RETURN DoCall()
  | PLUS:     RETURN DoPlus()
  | TIMES:    RETURN DoTimes()
  | ISINT:    RETURN DoInteger()
  | ISCHAR:   RETURN DoChar()
  | NAFF:     RETURN DoNot()
  | EQUALITY: RETURN DoEqual()
  | FAIL:     RETURN FALSE
  | PRINT:    RETURN DoPrint()
  | NL:	      RETURN DoNl()
  ELSE
    Out.Ln(); Out.String("Panic: "); Out.String("bad tag" (*action:1, " in ", "DoBuiltin"*)); Out.Ln(); HALT(2)
  END
END DoBuiltin;

(* |Initialize| -- initialize everything *)
PROCEDURE Initialize();
  VAR i: INTEGER; p: term;
BEGIN
  dflag := FALSE; errcount := 0;
  pbchar := ENDFILE; charptr := 0;
  hp := 0; InitSymbols();

  (* Set up the |refnode| array *)
  FOR i := 1 TO MAXARITY DO
    p := HeapAlloc(TERMSIZE);
    mem[p] := LSL(REF, 8) + TERMSIZE;
    mem[p+1] := i; refnode[i] := p
  END;

  (* The dummy clause $\it call(\sci p) \IF p$ is used by |call/1|. *)
  callbody := HeapAlloc(2);
  mem[callbody] := MakeRef(1);
  mem[(callbody)+1] := NULL
END Initialize;

(* |ReadFile| -- read and process clauses from an open file *)
PROCEDURE ReadFile();
  VAR c: clause;
BEGIN
  lineno := 1;
  REPEAT
    hmark := hp;
    c := ReadClause();
    IF c # NULL THEN
      IF dflag THEN PrintClause(c) END;	
      IF mem[c+3] # NULL THEN
        AddClause(c)
      ELSE
        Execute(c);
	hp := hmark
      END
    END
  UNTIL c = NULL
END ReadFile;

BEGIN  (* main program *)
  prog('subject(');
  prog('  <store,');
  prog('    <load,');
  prog('      <plusa,');
  prog('        <global(a)>,');
  prog('        <lsl, <load, <local(16)>>, <const(2)>>>>,');
  prog('    <local(20)>>');
  prog(') :- .');
                                                                
  prog('rule("*str", stmt, <store, reg, addr>) :- .');
  prog('rule("*ldr", reg,  <load, addr>) :- .');
  prog('rule("*addfp", reg, <local(N)>) :- .');
  prog('rule("local", addr, <local(N)>) :- .');
  prog('rule("*add", reg, <plusa, reg, rand>) :- .');
  prog('rule("index", addr, <plusa, reg, reg>) :- .');
  prog('rule("scale", addr,');
  prog('       <plusa, reg, <lsl, reg, <const(N)>>>) :- .');
  prog('rule("*global", reg, <global(X)>) :- .');
  prog('rule("*lsl", reg, <lsl, reg, rand>) :- .');
  prog('rule("lshiftc", rand, <lsl, reg, <const(N)>>) :- .');
  prog('rule("lshiftr", rand, <lsl, reg, reg>) :- .');
  prog('rule("*mov", reg, <const(N)>) :- .');
  prog('rule("const", rand, <const(N)>) :- .');
  prog('rule("reg", rand, reg) :- .');
  prog('rule("indir", addr, reg) :- .');
                                                                
  prog('use_rule(NT, Tree, node(Name, Kids)) :-');
  prog('  rule(Name, NT, RHS), match(RHS, Tree, Kids, nil).');
                                                                
  prog('match(NT, Tree, Parse:Kids0, Kids0) :-');
  prog('  use_rule(NT, Tree, Parse).');

  prog('match(node(W, PS), node(W, TS), Kids, Kids0) :-');
  prog('  matchall(PS, TS, Kids, Kids0).');
                                                                
  prog('matchall(nil, nil, Kids0, Kids0) :- .');
  prog('matchall(P:PS, T:TS, Kids, Kids0) :-');
  prog('  match(P, T, Kids, Kids1), matchall(PS, TS, Kids1, Kids0).');
                                                                
  prog('cost(node(X, TS), C) :-');
  prog('  opcost(X, A), allcosts(TS, B), plus(A, B, C).');
                                                                
  prog('allcosts(nil, 0) :- .');
  prog('allcosts(T:TS, C) :-');
  prog('  cost(T, A), allcosts(TS, B), plus(A, B, C).');
                                                                
  prog("opcost('*':_, 1) :- !.");
  prog('opcost(_, 0) :- .');
                                                                
  prog('answer(P, C) :-');
  prog('  subject(T), use_rule(stmt, T, P), cost(P, C).');
                                                                
  prog('min(N, P) :- min1(N, 0, P).');
  prog('min1(N, N, P) :- call(P), !.');
  prog('min1(N, N0, P) :- plus(N0, 1, N1), min1(N, N1, P).');
                                                                
  prog('# :- answer(P, C).');

  Initialize();
  ReadFile()
END tProlog.

(*<<
P = <"*str", <"*ldr", <"index", <"*global">, <"*lsl", <"*ldr", <"local">>, <"const">>>>, <"local">>
C = 5

P = <"*str", <"*ldr", <"index", <"*global">, <"*lsl", <"*ldr", <"local">>, <"const">>>>, <"indir", <"*addfp">>>
C = 6

P = <"*str", <"*ldr", <"index", <"*global">, <"*lsl", <"*ldr", <"local">>, <"reg", <"*mov">>>>>, <"local">>
C = 6

P = <"*str", <"*ldr", <"index", <"*global">, <"*lsl", <"*ldr", <"local">>, <"reg", <"*mov">>>>>, <"indir", <"*addfp">>>
C = 7

P = <"*str", <"*ldr", <"index", <"*global">, <"*lsl", <"*ldr", <"indir", <"*addfp">>>, <"const">>>>, <"local">>
C = 6

P = <"*str", <"*ldr", <"index", <"*global">, <"*lsl", <"*ldr", <"indir", <"*addfp">>>, <"const">>>>, <"indir", <"*addfp">>>
C = 7

P = <"*str", <"*ldr", <"index", <"*global">, <"*lsl", <"*ldr", <"indir", <"*addfp">>>, <"reg", <"*mov">>>>>, <"local">>
C = 7

P = <"*str", <"*ldr", <"index", <"*global">, <"*lsl", <"*ldr", <"indir", <"*addfp">>>, <"reg", <"*mov">>>>>, <"indir", <"*addfp">>>
C = 8

P = <"*str", <"*ldr", <"scale", <"*global">, <"*ldr", <"local">>>>, <"local">>
C = 4

P = <"*str", <"*ldr", <"scale", <"*global">, <"*ldr", <"local">>>>, <"indir", <"*addfp">>>
C = 5

P = <"*str", <"*ldr", <"scale", <"*global">, <"*ldr", <"indir", <"*addfp">>>>>, <"local">>
C = 5

P = <"*str", <"*ldr", <"scale", <"*global">, <"*ldr", <"indir", <"*addfp">>>>>, <"indir", <"*addfp">>>
C = 6

P = <"*str", <"*ldr", <"indir", <"*add", <"*global">, <"lshiftc", <"*ldr", <"local">>>>>>, <"local">>
C = 5

P = <"*str", <"*ldr", <"indir", <"*add", <"*global">, <"lshiftc", <"*ldr", <"local">>>>>>, <"indir", <"*addfp">>>
C = 6

P = <"*str", <"*ldr", <"indir", <"*add", <"*global">, <"lshiftc", <"*ldr", <"indir", <"*addfp">>>>>>>, <"local">>
C = 6

P = <"*str", <"*ldr", <"indir", <"*add", <"*global">, <"lshiftc", <"*ldr", <"indir", <"*addfp">>>>>>>, <"indir", <"*addfp">>>
C = 7

P = <"*str", <"*ldr", <"indir", <"*add", <"*global">, <"lshiftr", <"*ldr", <"local">>, <"*mov">>>>>, <"local">>
C = 6

P = <"*str", <"*ldr", <"indir", <"*add", <"*global">, <"lshiftr", <"*ldr", <"local">>, <"*mov">>>>>, <"indir", <"*addfp">>>
C = 7

P = <"*str", <"*ldr", <"indir", <"*add", <"*global">, <"lshiftr", <"*ldr", <"indir", <"*addfp">>>, <"*mov">>>>>, <"local">>
C = 7

P = <"*str", <"*ldr", <"indir", <"*add", <"*global">, <"lshiftr", <"*ldr", <"indir", <"*addfp">>>, <"*mov">>>>>, <"indir", <"*addfp">>>
C = 8

P = <"*str", <"*ldr", <"indir", <"*add", <"*global">, <"reg", <"*lsl", <"*ldr", <"local">>, <"const">>>>>>, <"local">>
C = 6

P = <"*str", <"*ldr", <"indir", <"*add", <"*global">, <"reg", <"*lsl", <"*ldr", <"local">>, <"const">>>>>>, <"indir", <"*addfp">>>
C = 7

P = <"*str", <"*ldr", <"indir", <"*add", <"*global">, <"reg", <"*lsl", <"*ldr", <"local">>, <"reg", <"*mov">>>>>>>, <"local">>
C = 7

P = <"*str", <"*ldr", <"indir", <"*add", <"*global">, <"reg", <"*lsl", <"*ldr", <"local">>, <"reg", <"*mov">>>>>>>, <"indir", <"*addfp">>>
C = 8

P = <"*str", <"*ldr", <"indir", <"*add", <"*global">, <"reg", <"*lsl", <"*ldr", <"indir", <"*addfp">>>, <"const">>>>>>, <"local">>
C = 7

P = <"*str", <"*ldr", <"indir", <"*add", <"*global">, <"reg", <"*lsl", <"*ldr", <"indir", <"*addfp">>>, <"const">>>>>>, <"indir", <"*addfp">>>
C = 8

P = <"*str", <"*ldr", <"indir", <"*add", <"*global">, <"reg", <"*lsl", <"*ldr", <"indir", <"*addfp">>>, <"reg", <"*mov">>>>>>>, <"local">>
C = 8

P = <"*str", <"*ldr", <"indir", <"*add", <"*global">, <"reg", <"*lsl", <"*ldr", <"indir", <"*addfp">>>, <"reg", <"*mov">>>>>>>, <"indir", <"*addfp">>>
C = 9

>>*)

(*[[
!! (SYMFILE #tProlog STAMP #tProlog.%main 1 #tProlog.m)
!! (CHKSUM STAMP)
!! 
MODULE tProlog STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tProlog.StringLength 4 3 0x00100001
! PROCEDURE StringLength(VAR s: tempstring): INTEGER;
!   i := 0;
CONST 0
STLW -4
LABEL L120
!   WHILE s[i] # ENDSTR DO i := i+1 END;
LDLW 12
LDLW -4
CONST 128
BOUND 42
LDIC
JEQZ L122
INCL -4
JUMP L120
LABEL L122
!   RETURN i
LDLW -4
RETURN
END

PROC tProlog.SaveString 8 4 0x00100001
! PROCEDURE SaveString(VAR s: tempstring): permstring;
!   IF charptr + StringLength(s) + 1 > MAXCHARS THEN
LDGW tProlog.charptr
LDLW 12
GLOBAL tProlog.StringLength
CALLW 1
PLUS
INC
CONST 2048
JLEQ L125
!     Out.Ln(); Out.String("Panic: "); Out.String("out of string space"); Out.Ln(); HALT(2)
GLOBAL Out.Ln
CALL 0
CONST 8
GLOBAL tProlog.%1
GLOBAL Out.String
CALL 2
CONST 20
GLOBAL tProlog.%2
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
CONST 2
GLOBAL HALT
CALL 1
LABEL L125
!   p := charptr; i := 0;
LDGW tProlog.charptr
STLW -4
CONST 0
STLW -8
LABEL L126
!     charbuf[charptr] := s[i]; charptr := charptr+1; i := i+1
LDLW 12
LDLW -8
CONST 128
BOUND 55
LDIC
GLOBAL tProlog.charbuf
LDGW tProlog.charptr
CONST 2048
BOUND 55
STIC
LDGW tProlog.charptr
INC
STGW tProlog.charptr
INCL -8
!   UNTIL charbuf[charptr-1] = ENDSTR;
GLOBAL tProlog.charbuf
LDGW tProlog.charptr
DEC
CONST 2048
BOUND 56
LDIC
JNEQZ L126
!   RETURN p
LDLW -4
RETURN
END

PROC tProlog.StringEqual 4 4 0x00100001
! PROCEDURE StringEqual(VAR s1: tempstring; s2: permstring): BOOLEAN;
!   i := 0;
CONST 0
STLW -4
LABEL L128
!   WHILE (s1[i] # ENDSTR) & (s1[i] = charbuf[s2+i]) DO i := i+1 END;
LDLW 12
LDLW -4
CONST 128
BOUND 65
LDIC
JEQZ L130
LDLW 12
LDLW -4
CONST 128
BOUND 65
LDIC
GLOBAL tProlog.charbuf
LDLW 16
LDLW -4
PLUS
CONST 2048
BOUND 65
LDIC
JNEQ L130
INCL -4
JUMP L128
LABEL L130
!   RETURN (s1[i] = charbuf[s2+i])
LDLW 12
LDLW -4
CONST 128
BOUND 66
LDIC
GLOBAL tProlog.charbuf
LDLW 16
LDLW -4
PLUS
CONST 2048
BOUND 66
LDIC
EQ
RETURN
END

PROC tProlog.WriteString 4 3 0
! PROCEDURE WriteString(s: permstring);
!   i := s;
LDLW 12
STLW -4
LABEL L132
!   WHILE charbuf[i] # ENDSTR DO
GLOBAL tProlog.charbuf
LDLW -4
CONST 2048
BOUND 74
LDIC
JEQZ L134
!     Out.Char(charbuf[i]); i := i+1
GLOBAL tProlog.charbuf
LDLW -4
CONST 2048
BOUND 75
LDIC
ALIGNC
GLOBAL Out.Char
CALL 1
INCL -4
JUMP L132
LABEL L134
RETURN
END

PROC tProlog.LocAlloc 4 3 0
! PROCEDURE LocAlloc(size: INTEGER): ptr;
!   IF lsp + size >= gsp THEN Out.Ln(); Out.String("Panic: "); Out.String("out of stack space"); Out.Ln(); HALT(2) END;
LDGW tProlog.lsp
LDLW 12
PLUS
LDGW tProlog.gsp
JLT L137
GLOBAL Out.Ln
CALL 0
CONST 8
GLOBAL tProlog.%1
GLOBAL Out.String
CALL 2
CONST 19
GLOBAL tProlog.%3
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
CONST 2
GLOBAL HALT
CALL 1
LABEL L137
!   p := lsp + 1; lsp := lsp + size; RETURN p
LDGW tProlog.lsp
INC
STLW -4
LDGW tProlog.lsp
LDLW 12
PLUS
STGW tProlog.lsp
LDLW -4
RETURN
END

PROC tProlog.GloAlloc 4 4 0
! PROCEDURE GloAlloc(kind, size: INTEGER): ptr;
!   IF gsp - size <= lsp THEN
LDGW tProlog.gsp
LDLW 16
MINUS
LDGW tProlog.lsp
JGT L140
!     Out.Ln(); Out.String("Panic: "); Out.String("out of stack space"); Out.Ln(); HALT(2)
GLOBAL Out.Ln
CALL 0
CONST 8
GLOBAL tProlog.%1
GLOBAL Out.String
CALL 2
CONST 19
GLOBAL tProlog.%3
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
CONST 2
GLOBAL HALT
CALL 1
LABEL L140
!   gsp := gsp - size; p := gsp;
LDGW tProlog.gsp
LDLW 16
MINUS
STGW tProlog.gsp
LDGW tProlog.gsp
STLW -4
!   mem[p] := LSL(kind, 8) + size;
LDLW 12
CONST 8
LSL
LDLW 16
PLUS
GLOBAL tProlog.mem
LDLW -4
CONST 25001
BOUND 115
STIW
!   RETURN p
LDLW -4
RETURN
END

PROC tProlog.HeapAlloc 4 3 0
! PROCEDURE HeapAlloc(size: INTEGER): ptr;
!   IF hp + size > MEMSIZE THEN Out.Ln(); Out.String("Panic: "); Out.String("out of heap space"); Out.Ln(); HALT(2) END;
LDGW tProlog.hp
LDLW 12
PLUS
CONST 25000
JLEQ L143
GLOBAL Out.Ln
CALL 0
CONST 8
GLOBAL tProlog.%1
GLOBAL Out.String
CALL 2
CONST 18
GLOBAL tProlog.%4
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
CONST 2
GLOBAL HALT
CALL 1
LABEL L143
!   p := hp + 1; hp := hp + size; RETURN p
LDGW tProlog.hp
INC
STLW -4
LDGW tProlog.hp
LDLW 12
PLUS
STGW tProlog.hp
LDLW -4
RETURN
END

PROC tProlog.prog 8 4 0
! PROCEDURE prog(line: ARRAY OF CHAR);
LOCAL 12
LDLW 16
FLEXCOPY
!   FOR i := 0 TO LEN(line)-2 DO
LDLW 16
CONST 2
MINUS
STLW -8
CONST 0
STLW -4
LABEL L144
LDLW -4
LDLW -8
JGT L145
!     infile[pin] := line[i]; pin := pin+1
LDLW 12
LDLW -4
LDLW 16
BOUND 133
LDIC
GLOBAL tProlog.infile
LDGW tProlog.pin
CONST 3000
BOUND 133
STIC
LDGW tProlog.pin
INC
STGW tProlog.pin
!   FOR i := 0 TO LEN(line)-2 DO
INCL -4
JUMP L144
LABEL L145
!   infile[pin] := ENDLINE; pin := pin+1
CONST 10
GLOBAL tProlog.infile
LDGW tProlog.pin
CONST 3000
BOUND 135
STIC
LDGW tProlog.pin
INC
STGW tProlog.pin
RETURN
END

PROC tProlog.rdchar 0 3 0x00100001
! PROCEDURE rdchar(VAR ch: CHAR);
!   IF pout >= pin THEN
LDGW tProlog.pout
LDGW tProlog.pin
JLT L148
!     ch := ENDFILE
CONST 127
LDLW 12
STOREC
RETURN
LABEL L148
!     ch := infile[pout]; pout := pout+1
GLOBAL tProlog.infile
LDGW tProlog.pout
CONST 3000
BOUND 143
LDIC
LDLW 12
STOREC
LDGW tProlog.pout
INC
STGW tProlog.pout
RETURN
END

PROC tProlog.GetChar 4 2 0
! PROCEDURE GetChar(): CHAR;
!   IF pbchar # ENDFILE THEN
LDGC tProlog.pbchar
CONST 127
JEQ L154
!     ch := pbchar; pbchar := ENDFILE
LDGC tProlog.pbchar
STLC -1
CONST 127
STGC tProlog.pbchar
JUMP L152
LABEL L154
!     rdchar(ch);
LOCAL -1
GLOBAL tProlog.rdchar
CALL 1
!     IF ch = ENDLINE THEN lineno := lineno+1 END
LDLC -1
CONST 10
JNEQ L152
LDGW tProlog.lineno
INC
STGW tProlog.lineno
LABEL L152
!   RETURN ch
LDLC -1
RETURN
END

PROC tProlog.PushBack 0 2 0
! PROCEDURE PushBack(ch: CHAR);
!   pbchar := ch
LDLC 12
STGC tProlog.pbchar
RETURN
END

PROC tProlog.Deref 0 4 0
! PROCEDURE Deref(t: term; e: frame): term;
!   IF t = NULL THEN Out.Ln(); Out.String("Panic: "); Out.String("Deref"); Out.Ln(); HALT(2) END;
LDLW 12
JNEQZ L157
GLOBAL Out.Ln
CALL 0
CONST 8
GLOBAL tProlog.%1
GLOBAL Out.String
CALL 2
CONST 6
GLOBAL tProlog.%5
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
CONST 2
GLOBAL HALT
CALL 1
LABEL L157
!   IF (LSR(mem[t], 8) = REF) & (e # NULL) THEN
GLOBAL tProlog.mem
LDLW 12
CONST 25001
BOUND 190
LDIW
CONST 8
LSR
CONST 5
JNEQ L160
LDLW 16
JEQZ L160
!     t := (e+7+(mem[t+1]-1)*TERMSIZE)
LDLW 16
CONST 7
PLUS
GLOBAL tProlog.mem
LDLW 12
INC
CONST 25001
BOUND 191
LDIW
DEC
CONST 2
TIMES
PLUS
STLW 12
LABEL L160
!   WHILE (LSR(mem[t], 8) = CELL) & (mem[t+1] # NULL) DO
GLOBAL tProlog.mem
LDLW 12
CONST 25001
BOUND 193
LDIW
CONST 8
LSR
CONST 4
JNEQ L164
GLOBAL tProlog.mem
LDLW 12
INC
CONST 25001
BOUND 193
LDIW
JEQZ L164
!     t := mem[t+1]
GLOBAL tProlog.mem
LDLW 12
INC
CONST 25001
BOUND 194
LDIW
STLW 12
JUMP L160
LABEL L164
!   RETURN t
LDLW 12
RETURN
END

PROC tProlog.Lookup 12 4 0x00100001
! PROCEDURE Lookup(VAR name: tempstring): symbol;
!   h := 0; i := 0;
CONST 0
STLW -4
CONST 0
STLW -8
LABEL L166
!   WHILE name[i] # ENDSTR DO
LDLW 12
LDLW -8
CONST 128
BOUND 218
LDIC
JEQZ L168
!     h := (5 * h + ORD(name[i])) MOD MAXSYMBOLS; i := i+1 
LDLW -4
CONST 5
TIMES
LDLW 12
LDLW -8
CONST 128
BOUND 219
LDIC
PLUS
CONST 511
MOD
STLW -4
INCL -8
JUMP L166
LABEL L168
!   p := h+1;
LDLW -4
INC
STLW -12
LABEL L169
!   WHILE symtab[p].name # -1 DO
GLOBAL tProlog.symtab
LDLW -12
CONST 512
BOUND 224
CONST 4
TIMES
LDIW
CONST -1
JEQ L171
!     IF StringEqual(name, symtab[p].name) THEN RETURN p END;
GLOBAL tProlog.symtab
LDLW -12
CONST 512
BOUND 225
CONST 4
TIMES
LDIW
LDLW 12
GLOBAL tProlog.StringEqual
CALLW 2
JEQZ L174
LDLW -12
RETURN
LABEL L174
!     p := p-1;
DECL -12
!     IF p = 0 THEN p := MAXSYMBOLS END
LDLW -12
JNEQZ L169
CONST 511
STLW -12
JUMP L169
LABEL L171
!   IF nsymbols >= (MAXSYMBOLS DIV 10) * (HASHFACTOR DIV 10) THEN
LDGW tProlog.nsymbols
CONST 459
JLT L180
!     Out.Ln(); Out.String("Panic: "); Out.String("out of symbol space"); Out.Ln(); HALT(2)
GLOBAL Out.Ln
CALL 0
CONST 8
GLOBAL tProlog.%1
GLOBAL Out.String
CALL 2
CONST 20
GLOBAL tProlog.%6
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
CONST 2
GLOBAL HALT
CALL 1
LABEL L180
!   symtab[p].name := SaveString(name);
LDLW 12
GLOBAL tProlog.SaveString
CALLW 1
GLOBAL tProlog.symtab
LDLW -12
CONST 512
BOUND 235
CONST 4
TIMES
STIW
!   symtab[p].arity := -1;
CONST -1
GLOBAL tProlog.symtab
LDLW -12
CONST 512
BOUND 236
CONST 16
TIMES
OFFSET
STNW 4
!   symtab[p].action := 0; symtab[p].prok := NULL;
CONST 0
GLOBAL tProlog.symtab
LDLW -12
CONST 512
BOUND 237
CONST 16
TIMES
OFFSET
STNW 8
CONST 0
GLOBAL tProlog.symtab
LDLW -12
CONST 512
BOUND 237
CONST 16
TIMES
OFFSET
STNW 12
!   RETURN p
LDLW -12
RETURN
END

PROC tProlog.Enter 136 4 0
! PROCEDURE Enter(name: keyword; arity: INTEGER; action: INTEGER): symbol;
LOCAL 12
LDLW 16
FLEXCOPY
!   i := 0;
CONST 0
STLW -8
LABEL L181
!   WHILE name[i] # 0X DO
LDLW 12
LDLW -8
LDLW 16
BOUND 248
LDIC
JEQZ L183
!     temp[i] := name[i]; i := i+1 
LDLW 12
LDLW -8
LDLW 16
BOUND 249
LDIC
LOCAL -136
LDLW -8
CONST 128
BOUND 249
STIC
INCL -8
JUMP L181
LABEL L183
!   temp[i] := ENDSTR; s := Lookup(temp);
CONST 0
LOCAL -136
LDLW -8
CONST 128
BOUND 251
STIC
LOCAL -136
GLOBAL tProlog.Lookup
CALLW 1
STLW -4
!   symtab[s].arity := arity; symtab[s].action := action;
LDLW 20
GLOBAL tProlog.symtab
LDLW -4
CONST 512
BOUND 252
CONST 16
TIMES
OFFSET
STNW 4
LDLW 24
GLOBAL tProlog.symtab
LDLW -4
CONST 512
BOUND 252
CONST 16
TIMES
OFFSET
STNW 8
!   RETURN s
LDLW -4
RETURN
END

PROC tProlog.InitSymbols 8 5 0
! PROCEDURE InitSymbols();
!   nsymbols := 0;
CONST 0
STGW tProlog.nsymbols
!   FOR i := 1 TO MAXSYMBOLS DO symtab[i].name := -1 END;
CONST 1
STLW -4
LABEL L184
LDLW -4
CONST 511
JGT L185
CONST -1
GLOBAL tProlog.symtab
LDLW -4
CONST 512
BOUND 275
CONST 4
TIMES
STIW
INCL -4
JUMP L184
LABEL L185
!   cons   := Enter(":", 2, 0);
CONST 0
CONST 2
CONST 2
GLOBAL tProlog.%110
GLOBAL tProlog.Enter
CALLW 4
STGW tProlog.cons
!   cutsym := Enter("!", 0, CUT);
CONST 1
CONST 0
CONST 2
GLOBAL tProlog.%111
GLOBAL tProlog.Enter
CALLW 4
STGW tProlog.cutsym
!   eqsym  := Enter("=", 2, EQUALITY);
CONST 8
CONST 2
CONST 2
GLOBAL tProlog.%112
GLOBAL tProlog.Enter
CALLW 4
STGW tProlog.eqsym
!   nilsym := Enter("nil", 0, 0);
CONST 0
CONST 0
CONST 4
GLOBAL tProlog.%7
GLOBAL tProlog.Enter
CALLW 4
STGW tProlog.nilsym
!   notsym := Enter("not", 1, NAFF);
CONST 7
CONST 1
CONST 4
GLOBAL tProlog.%8
GLOBAL tProlog.Enter
CALLW 4
STGW tProlog.notsym
!   node   := Enter("node", 2, 0);
CONST 0
CONST 2
CONST 5
GLOBAL tProlog.%9
GLOBAL tProlog.Enter
CALLW 4
STGW tProlog.node
!   dummy  := Enter("call", 1, CALL);
CONST 2
CONST 1
CONST 5
GLOBAL tProlog.%10
GLOBAL tProlog.Enter
CALLW 4
STLW -8
!   dummy  := Enter("plus", 3, PLUS);
CONST 3
CONST 3
CONST 5
GLOBAL tProlog.%11
GLOBAL tProlog.Enter
CALLW 4
STLW -8
!   dummy  := Enter("times", 3, TIMES);
CONST 4
CONST 3
CONST 6
GLOBAL tProlog.%12
GLOBAL tProlog.Enter
CALLW 4
STLW -8
!   dummy  := Enter("integer", 1, ISINT);
CONST 5
CONST 1
CONST 8
GLOBAL tProlog.%13
GLOBAL tProlog.Enter
CALLW 4
STLW -8
!   dummy  := Enter("char", 1, ISCHAR);
CONST 6
CONST 1
CONST 5
GLOBAL tProlog.%14
GLOBAL tProlog.Enter
CALLW 4
STLW -8
!   dummy  := Enter("false", 0, FAIL);
CONST 9
CONST 0
CONST 6
GLOBAL tProlog.%15
GLOBAL tProlog.Enter
CALLW 4
STLW -8
!   dummy  := Enter("print", 1, PRINT);
CONST 10
CONST 1
CONST 6
GLOBAL tProlog.%16
GLOBAL tProlog.Enter
CALLW 4
STLW -8
!   dummy  := Enter("nl", 0, NL)
CONST 11
CONST 0
CONST 3
GLOBAL tProlog.%17
GLOBAL tProlog.Enter
CALLW 4
STLW -8
RETURN
END

PROC tProlog.AddClause 8 4 0
! PROCEDURE AddClause(c: clause);
!   s := mem[mem[c+3]+1];
GLOBAL tProlog.mem
GLOBAL tProlog.mem
LDLW 12
CONST 3
PLUS
CONST 25001
BOUND 296
LDIW
INC
CONST 25001
BOUND 296
LDIW
STLW -4
!   IF symtab[s].action # 0 THEN
GLOBAL tProlog.symtab
LDLW -4
CONST 512
BOUND 297
CONST 16
TIMES
OFFSET
LDNW 8
JEQZ L191
!     Out.Ln(); Out.String("Error: "); Out.String("cannot add clauses to built-in relation "); run := FALSE;
GLOBAL Out.Ln
CALL 0
CONST 8
GLOBAL tProlog.%18
GLOBAL Out.String
CALL 2
CONST 41
GLOBAL tProlog.%19
GLOBAL Out.String
CALL 2
CONST 0
STGC tProlog.run
!     WriteString(symtab[s].name)
GLOBAL tProlog.symtab
LDLW -4
CONST 512
BOUND 299
CONST 4
TIMES
LDIW
GLOBAL tProlog.WriteString
CALL 1
RETURN
LABEL L191
!   ELSIF symtab[s].prok = NULL THEN
GLOBAL tProlog.symtab
LDLW -4
CONST 512
BOUND 300
CONST 16
TIMES
OFFSET
LDNW 12
JNEQZ L193
!     symtab[s].prok := c
LDLW 12
GLOBAL tProlog.symtab
LDLW -4
CONST 512
BOUND 301
CONST 16
TIMES
OFFSET
STNW 12
RETURN
LABEL L193
!     p := symtab[s].prok;
GLOBAL tProlog.symtab
LDLW -4
CONST 512
BOUND 303
CONST 16
TIMES
OFFSET
LDNW 12
STLW -8
LABEL L187
!     WHILE mem[p+2] # NULL DO p := mem[p+2] END;
GLOBAL tProlog.mem
LDLW -8
CONST 2
PLUS
CONST 25001
BOUND 304
LDIW
JEQZ L189
GLOBAL tProlog.mem
LDLW -8
CONST 2
PLUS
CONST 25001
BOUND 304
LDIW
STLW -8
JUMP L187
LABEL L189
!     mem[p+2] := c
LDLW 12
GLOBAL tProlog.mem
LDLW -8
CONST 2
PLUS
CONST 25001
BOUND 305
STIW
RETURN
END

PROC tProlog.MakeCompound 16 4 0x00200001
! PROCEDURE MakeCompound(fun: symbol; VAR arg: argbuf): term;
!   n := symtab[fun].arity;
GLOBAL tProlog.symtab
LDLW 12
CONST 512
BOUND 315
CONST 16
TIMES
OFFSET
LDNW 4
STLW -12
!   p := HeapAlloc(TERMSIZE+n);
LDLW -12
CONST 2
PLUS
GLOBAL tProlog.HeapAlloc
CALLW 1
STLW -4
!   mem[p] := LSL(FUNC, 8) + TERMSIZE+n;
LDLW -12
CONST 258
PLUS
GLOBAL tProlog.mem
LDLW -4
CONST 25001
BOUND 317
STIW
!   mem[p+1] := fun;
LDLW 12
GLOBAL tProlog.mem
LDLW -4
INC
CONST 25001
BOUND 318
STIW
!   FOR i := 1 TO n DO mem[p+i+1] := arg[i] END;
LDLW -12
STLW -16
CONST 1
STLW -8
LABEL L194
LDLW -8
LDLW -16
JGT L195
LDLW 16
LDLW -8
CONST 64
BOUND 319
LDIW
GLOBAL tProlog.mem
LDLW -4
LDLW -8
PLUS
INC
CONST 25001
BOUND 319
STIW
INCL -8
JUMP L194
LABEL L195
!   RETURN p
LDLW -4
RETURN
END

PROC tProlog.MakeNode 256 4 0
! PROCEDURE MakeNode(fun: symbol; a1, a2: term): term;
!   arg[1] := a1; arg[2] := a2;
LDLW 16
STLW -252
LDLW 20
STLW -248
!   RETURN MakeCompound(fun, arg)
LOCAL -256
LDLW 12
GLOBAL tProlog.MakeCompound
CALLW 2
RETURN
END

PROC tProlog.MakeRef 0 3 0
! PROCEDURE MakeRef(offset: INTEGER): term;
!   RETURN refnode[offset]
GLOBAL tProlog.refnode
LDLW 12
CONST 64
BOUND 336
LDIW
RETURN
END

PROC tProlog.MakeInt 4 4 0
! PROCEDURE MakeInt(i: INTEGER): term;
!   p := HeapAlloc(TERMSIZE);
CONST 2
GLOBAL tProlog.HeapAlloc
CALLW 1
STLW -4
!   mem[p] := LSL(INT, 8) + TERMSIZE;
CONST 514
GLOBAL tProlog.mem
LDLW -4
CONST 25001
BOUND 344
STIW
!   mem[p+1] := i; RETURN p
LDLW 12
GLOBAL tProlog.mem
LDLW -4
INC
CONST 25001
BOUND 345
STIW
LDLW -4
RETURN
END

PROC tProlog.MakeChar 4 4 0
! PROCEDURE MakeChar(c: CHAR): term;
!   p := HeapAlloc(TERMSIZE);
CONST 2
GLOBAL tProlog.HeapAlloc
CALLW 1
STLW -4
!   mem[p] := LSL(CHRCTR, 8) + TERMSIZE;
CONST 770
GLOBAL tProlog.mem
LDLW -4
CONST 25001
BOUND 353
STIW
!   mem[p+1] := ORD(c); RETURN p
LDLC 12
GLOBAL tProlog.mem
LDLW -4
INC
CONST 25001
BOUND 354
STIW
LDLW -4
RETURN
END

PROC tProlog.MakeString 8 4 0x00100001
! PROCEDURE MakeString(VAR s: tempstring): term;
!   i := StringLength(s);
LDLW 12
GLOBAL tProlog.StringLength
CALLW 1
STLW -8
!   p := MakeNode(nilsym, NULL, NULL);
CONST 0
CONST 0
LDGW tProlog.nilsym
GLOBAL tProlog.MakeNode
CALLW 3
STLW -4
LABEL L196
!   WHILE i > 0 DO
LDLW -8
JLEQZ L198
!     i := i-1; p := MakeNode(cons, MakeChar(s[i]), p)
DECL -8
LDLW -4
LDLW 12
LDLW -8
CONST 128
BOUND 364
LDIC
ALIGNC
GLOBAL tProlog.MakeChar
CALLW 1
LDGW tProlog.cons
GLOBAL tProlog.MakeNode
CALLW 3
STLW -4
JUMP L196
LABEL L198
!   RETURN p
LDLW -4
RETURN
END

PROC tProlog.MakeClause 12 4 0x00400001
! PROCEDURE MakeClause(nvars: INTEGER; head: term;
!   p := HeapAlloc(CLSIZE + nbody + 1);
LDLW 24
CONST 5
PLUS
GLOBAL tProlog.HeapAlloc
CALLW 1
STLW -4
!   mem[p] := nvars; mem[p+2] := NULL; mem[p+3] := head;
LDLW 12
GLOBAL tProlog.mem
LDLW -4
CONST 25001
BOUND 375
STIW
CONST 0
GLOBAL tProlog.mem
LDLW -4
CONST 2
PLUS
CONST 25001
BOUND 375
STIW
LDLW 16
GLOBAL tProlog.mem
LDLW -4
CONST 3
PLUS
CONST 25001
BOUND 375
STIW
!   FOR i := 1 TO nbody DO mem[(p+4)+i-1] := body[i] END;
LDLW 24
STLW -12
CONST 1
STLW -8
LABEL L199
LDLW -8
LDLW -12
JGT L200
LDLW 20
LDLW -8
CONST 64
BOUND 376
LDIW
GLOBAL tProlog.mem
LDLW -4
CONST 4
PLUS
LDLW -8
PLUS
DEC
CONST 25001
BOUND 376
STIW
INCL -8
JUMP L199
LABEL L200
!   mem[(p+4)+nbody+1-1] := NULL;
CONST 0
GLOBAL tProlog.mem
LDLW -4
CONST 4
PLUS
LDLW 24
PLUS
CONST 25001
BOUND 377
STIW
!   IF head = NULL THEN 
LDLW 16
JNEQZ L203
!     mem[p+1] := 0
CONST 0
GLOBAL tProlog.mem
LDLW -4
INC
CONST 25001
BOUND 379
STIW
JUMP L201
LABEL L203
!     mem[p+1] := Key(head, NULL)
CONST 0
LDLW 16
GLOBAL tProlog.Key
CALLW 2
GLOBAL tProlog.mem
LDLW -4
INC
CONST 25001
BOUND 381
STIW
LABEL L201
!   RETURN p
LDLW -4
RETURN
END

PROC tProlog.IsString 4 5 0
! PROCEDURE IsString(t: term; e: frame): BOOLEAN;
!   i := 0; t := Deref(t, e);
CONST 0
STLW -4
LDLW 16
LDLW 12
GLOBAL tProlog.Deref
CALLW 2
STLW 12
LABEL L204
!   WHILE i < limit DO
LDLW -4
CONST 128
JGEQ L206
!     IF (LSR(mem[t], 8) # FUNC) OR (mem[t+1] # cons) THEN
GLOBAL tProlog.mem
LDLW 12
CONST 25001
BOUND 400
LDIW
CONST 8
LSR
CONST 1
JNEQ L208
GLOBAL tProlog.mem
LDLW 12
INC
CONST 25001
BOUND 400
LDIW
LDGW tProlog.cons
JEQ L209
LABEL L208
!       RETURN (LSR(mem[t], 8) = FUNC) & (mem[t+1] = nilsym)
GLOBAL tProlog.mem
LDLW 12
CONST 25001
BOUND 401
LDIW
CONST 8
LSR
CONST 1
JEQ L211
CONST 0
RETURN
LABEL L211
GLOBAL tProlog.mem
LDLW 12
INC
CONST 25001
BOUND 401
LDIW
LDGW tProlog.nilsym
EQ
RETURN
LABEL L209
!     ELSIF LSR(mem[Deref(mem[t+1+1], e)], 8) # CHRCTR THEN
GLOBAL tProlog.mem
LDLW 16
GLOBAL tProlog.mem
LDLW 12
CONST 2
PLUS
CONST 25001
BOUND 402
LDIW
GLOBAL tProlog.Deref
CALLW 2
CONST 25001
BOUND 402
LDIW
CONST 8
LSR
CONST 3
JEQ L215
!       RETURN FALSE
CONST 0
RETURN
LABEL L215
!       i := i+1; t := Deref(mem[t+2+1], e) 
INCL -4
LDLW 16
GLOBAL tProlog.mem
LDLW 12
CONST 3
PLUS
CONST 25001
BOUND 405
LDIW
GLOBAL tProlog.Deref
CALLW 2
STLW 12
JUMP L204
LABEL L206
!   RETURN FALSE
CONST 0
RETURN
END

PROC tProlog.IsList 4 4 0
! PROCEDURE IsList(t: term; e: frame): BOOLEAN;
!   i := 0; t := Deref(t, e);
CONST 0
STLW -4
LDLW 16
LDLW 12
GLOBAL tProlog.Deref
CALLW 2
STLW 12
LABEL L216
!   WHILE i < limit DO
LDLW -4
CONST 128
JGEQ L218
!     IF (LSR(mem[t], 8) # FUNC) OR (mem[t+1] # cons) THEN
GLOBAL tProlog.mem
LDLW 12
CONST 25001
BOUND 418
LDIW
CONST 8
LSR
CONST 1
JNEQ L220
GLOBAL tProlog.mem
LDLW 12
INC
CONST 25001
BOUND 418
LDIW
LDGW tProlog.cons
JEQ L221
LABEL L220
!       RETURN (LSR(mem[t], 8) = FUNC) & (mem[t+1] = nilsym)
GLOBAL tProlog.mem
LDLW 12
CONST 25001
BOUND 419
LDIW
CONST 8
LSR
CONST 1
JEQ L223
CONST 0
RETURN
LABEL L223
GLOBAL tProlog.mem
LDLW 12
INC
CONST 25001
BOUND 419
LDIW
LDGW tProlog.nilsym
EQ
RETURN
LABEL L221
!       i := i+1; t := Deref(mem[t+2+1], e)
INCL -4
LDLW 16
GLOBAL tProlog.mem
LDLW 12
CONST 3
PLUS
CONST 25001
BOUND 421
LDIW
GLOBAL tProlog.Deref
CALLW 2
STLW 12
JUMP L216
LABEL L218
!   RETURN FALSE
CONST 0
RETURN
END

PROC tProlog.ShowString 0 5 0
! PROCEDURE ShowString(t: term; e: frame);
!   t := Deref(t, e);
LDLW 16
LDLW 12
GLOBAL tProlog.Deref
CALLW 2
STLW 12
!   Out.Char('"');
CONST 34
ALIGNC
GLOBAL Out.Char
CALL 1
LABEL L226
!   WHILE mem[t+1] # nilsym DO
GLOBAL tProlog.mem
LDLW 12
INC
CONST 25001
BOUND 432
LDIW
LDGW tProlog.nilsym
JEQ L228
!     Out.Char(CHR(mem[Deref(mem[t+1+1], e)+1]));
GLOBAL tProlog.mem
LDLW 16
GLOBAL tProlog.mem
LDLW 12
CONST 2
PLUS
CONST 25001
BOUND 433
LDIW
GLOBAL tProlog.Deref
CALLW 2
INC
CONST 25001
BOUND 433
LDIW
CONVNC
ALIGNC
GLOBAL Out.Char
CALL 1
!     t := Deref(mem[t+2+1], e)
LDLW 16
GLOBAL tProlog.mem
LDLW 12
CONST 3
PLUS
CONST 25001
BOUND 434
LDIW
GLOBAL tProlog.Deref
CALLW 2
STLW 12
JUMP L226
LABEL L228
!   Out.Char('"')
CONST 34
ALIGNC
GLOBAL Out.Char
CALL 1
RETURN
END

PROC tProlog.PrintCompound 12 5 0
! PROCEDURE PrintCompound(t: term; e: frame; prio: INTEGER);
!   f := mem[t+1];
GLOBAL tProlog.mem
LDLW 12
INC
CONST 25001
BOUND 443
LDIW
STLW -4
!   IF f = cons THEN
LDLW -4
LDGW tProlog.cons
JNEQ L236
!     IF IsString(t, e) THEN
LDLW 16
LDLW 12
GLOBAL tProlog.IsString
CALLW 2
JEQZ L245
!       ShowString(t, e)
LDLW 16
LDLW 12
GLOBAL tProlog.ShowString
CALL 2
RETURN
LABEL L245
!       IF prio < CONSPRIO THEN Out.Char('(') END;
LDLW 20
CONST 1
JGEQ L240
CONST 40
ALIGNC
GLOBAL Out.Char
CALL 1
LABEL L240
!       PrintTerm(mem[t+1+1], e, CONSPRIO-1);
CONST 0
LDLW 16
GLOBAL tProlog.mem
LDLW 12
CONST 2
PLUS
CONST 25001
BOUND 450
LDIW
GLOBAL tProlog.PrintTerm
CALL 3
!       Out.Char(':');
CONST 58
ALIGNC
GLOBAL Out.Char
CALL 1
!       PrintTerm(mem[t+2+1], e, CONSPRIO);
CONST 1
LDLW 16
GLOBAL tProlog.mem
LDLW 12
CONST 3
PLUS
CONST 25001
BOUND 452
LDIW
GLOBAL tProlog.PrintTerm
CALL 3
!       IF prio < CONSPRIO THEN Out.Char(')') END
LDLW 20
CONST 1
JGEQ L232
CONST 41
ALIGNC
GLOBAL Out.Char
CALL 1
RETURN
LABEL L236
!   ELSIF f = eqsym THEN
LDLW -4
LDGW tProlog.eqsym
JNEQ L247
!     IF prio < EQPRIO THEN Out.Char('(') END;
LDLW 20
CONST 2
JGEQ L250
CONST 40
ALIGNC
GLOBAL Out.Char
CALL 1
LABEL L250
!     PrintTerm(mem[t+1+1], e, EQPRIO-1);
CONST 1
LDLW 16
GLOBAL tProlog.mem
LDLW 12
CONST 2
PLUS
CONST 25001
BOUND 458
LDIW
GLOBAL tProlog.PrintTerm
CALL 3
!     Out.String(" = ");
CONST 4
GLOBAL tProlog.%20
GLOBAL Out.String
CALL 2
!     PrintTerm(mem[t+2+1], e, EQPRIO-1);
CONST 1
LDLW 16
GLOBAL tProlog.mem
LDLW 12
CONST 3
PLUS
CONST 25001
BOUND 460
LDIW
GLOBAL tProlog.PrintTerm
CALL 3
!     IF prio < EQPRIO THEN Out.Char(')') END
LDLW 20
CONST 2
JGEQ L232
CONST 41
ALIGNC
GLOBAL Out.Char
CALL 1
RETURN
LABEL L247
!   ELSIF f = notsym THEN
LDLW -4
LDGW tProlog.notsym
JNEQ L255
!     Out.String("not ");
CONST 5
GLOBAL tProlog.%21
GLOBAL Out.String
CALL 2
!     PrintTerm(mem[t+1+1], e, MAXPRIO)
CONST 2
LDLW 16
GLOBAL tProlog.mem
LDLW 12
CONST 2
PLUS
CONST 25001
BOUND 465
LDIW
GLOBAL tProlog.PrintTerm
CALL 3
RETURN
LABEL L255
!   ELSIF (f = node) & IsList(mem[t+2+1], e) THEN
LDLW -4
LDGW tProlog.node
JNEQ L257
LDLW 16
GLOBAL tProlog.mem
LDLW 12
CONST 3
PLUS
CONST 25001
BOUND 466
LDIW
GLOBAL tProlog.IsList
CALLW 2
JEQZ L257
!     PrintNode(t, e)
LDLW 16
LDLW 12
GLOBAL tProlog.PrintNode
CALL 2
RETURN
LABEL L257
!     WriteString(symtab[f].name);
GLOBAL tProlog.symtab
LDLW -4
CONST 512
BOUND 470
CONST 4
TIMES
LDIW
GLOBAL tProlog.WriteString
CALL 1
!     IF symtab[f].arity > 0 THEN
GLOBAL tProlog.symtab
LDLW -4
CONST 512
BOUND 471
CONST 16
TIMES
OFFSET
LDNW 4
JLEQZ L232
!       Out.Char('(');
CONST 40
ALIGNC
GLOBAL Out.Char
CALL 1
!       PrintTerm(mem[t+1+1], e, ARGPRIO);
CONST 2
LDLW 16
GLOBAL tProlog.mem
LDLW 12
CONST 2
PLUS
CONST 25001
BOUND 473
LDIW
GLOBAL tProlog.PrintTerm
CALL 3
!       FOR i := 2 TO symtab[f].arity DO
GLOBAL tProlog.symtab
LDLW -4
CONST 512
BOUND 474
CONST 16
TIMES
OFFSET
LDNW 4
STLW -12
CONST 2
STLW -8
LABEL L233
LDLW -8
LDLW -12
JGT L234
!         Out.String(", ");
CONST 3
GLOBAL tProlog.%22
GLOBAL Out.String
CALL 2
!         PrintTerm(mem[t+i+1], e, ARGPRIO)
CONST 2
LDLW 16
GLOBAL tProlog.mem
LDLW 12
LDLW -8
PLUS
INC
CONST 25001
BOUND 476
LDIW
GLOBAL tProlog.PrintTerm
CALL 3
!       FOR i := 2 TO symtab[f].arity DO
INCL -8
JUMP L233
LABEL L234
!       Out.Char(')')
CONST 41
ALIGNC
GLOBAL Out.Char
CALL 1
LABEL L232
RETURN
END

PROC tProlog.PrintNode 4 5 0
! PROCEDURE PrintNode(t: term; e: frame);
!   Out.Char('<');
CONST 60
ALIGNC
GLOBAL Out.Char
CALL 1
!   PrintTerm(mem[t+1+1], e, MAXPRIO);
CONST 2
LDLW 16
GLOBAL tProlog.mem
LDLW 12
CONST 2
PLUS
CONST 25001
BOUND 488
LDIW
GLOBAL tProlog.PrintTerm
CALL 3
!   u := Deref(mem[t+2+1], e);
LDLW 16
GLOBAL tProlog.mem
LDLW 12
CONST 3
PLUS
CONST 25001
BOUND 489
LDIW
GLOBAL tProlog.Deref
CALLW 2
STLW -4
LABEL L259
!   WHILE mem[u+1] # nilsym DO
GLOBAL tProlog.mem
LDLW -4
INC
CONST 25001
BOUND 490
LDIW
LDGW tProlog.nilsym
JEQ L261
!     Out.String(", ");
CONST 3
GLOBAL tProlog.%22
GLOBAL Out.String
CALL 2
!     PrintTerm(mem[u+1+1], e, MAXPRIO);
CONST 2
LDLW 16
GLOBAL tProlog.mem
LDLW -4
CONST 2
PLUS
CONST 25001
BOUND 492
LDIW
GLOBAL tProlog.PrintTerm
CALL 3
!     u := Deref(mem[u+2+1], e)
LDLW 16
GLOBAL tProlog.mem
LDLW -4
CONST 3
PLUS
CONST 25001
BOUND 493
LDIW
GLOBAL tProlog.Deref
CALLW 2
STLW -4
JUMP L259
LABEL L261
!   Out.Char('>');
CONST 62
ALIGNC
GLOBAL Out.Char
CALL 1
RETURN
END

PROC tProlog.PrintTerm 0 4 0
! PROCEDURE PrintTerm(t: term; e: frame; prio: INTEGER);
!   t := Deref(t, e);
LDLW 16
LDLW 12
GLOBAL tProlog.Deref
CALLW 2
STLW 12
!   IF t = NULL THEN
LDLW 12
JNEQZ L274
!     Out.String("*null-term*")
CONST 12
GLOBAL tProlog.%23
GLOBAL Out.String
CALL 2
RETURN
LABEL L274
!     CASE LSR(mem[t], 8) OF
GLOBAL tProlog.mem
LDLW 12
CONST 25001
BOUND 505
LDIW
CONST 8
LSR
DEC
JCASE 5
CASEL L265
CASEL L266
CASEL L267
CASEL L268
CASEL L269
JUMP L263
LABEL L265
!         PrintCompound(t, e, prio)
LDLW 20
LDLW 16
LDLW 12
GLOBAL tProlog.PrintCompound
CALL 3
RETURN
LABEL L266
!         Out.Int(mem[t+1], 0)
CONST 0
GLOBAL tProlog.mem
LDLW 12
INC
CONST 25001
BOUND 509
LDIW
GLOBAL Out.Int
CALL 2
RETURN
LABEL L267
!         Out.Char("'"); Out.Char(CHR(mem[t+1])); Out.Char("'")
CONST 39
ALIGNC
GLOBAL Out.Char
CALL 1
GLOBAL tProlog.mem
LDLW 12
INC
CONST 25001
BOUND 511
LDIW
CONVNC
ALIGNC
GLOBAL Out.Char
CALL 1
CONST 39
ALIGNC
GLOBAL Out.Char
CALL 1
RETURN
LABEL L268
!         IF (t >= gsp) THEN
LDLW 12
LDGW tProlog.gsp
JLT L272
!           Out.Char('G'); Out.Int((MEMSIZE - t) DIV TERMSIZE, 0)
CONST 71
ALIGNC
GLOBAL Out.Char
CALL 1
CONST 0
CONST 25000
LDLW 12
MINUS
CONST 2
DIV
GLOBAL Out.Int
CALL 2
RETURN
LABEL L272
!           Out.Char('L'); Out.Int((t - hp) DIV TERMSIZE, 0)
CONST 76
ALIGNC
GLOBAL Out.Char
CALL 1
CONST 0
LDLW 12
LDGW tProlog.hp
MINUS
CONST 2
DIV
GLOBAL Out.Int
CALL 2
RETURN
LABEL L269
!         Out.Char('@'); Out.Int(mem[t+1], 0)
CONST 64
ALIGNC
GLOBAL Out.Char
CALL 1
CONST 0
GLOBAL tProlog.mem
LDLW 12
INC
CONST 25001
BOUND 519
LDIW
GLOBAL Out.Int
CALL 2
RETURN
LABEL L263
!       Out.String("*unknown-term(tag="); 
CONST 19
GLOBAL tProlog.%24
GLOBAL Out.String
CALL 2
!       Out.Int(LSR(mem[t], 8), 0); Out.String(")*")
CONST 0
GLOBAL tProlog.mem
LDLW 12
CONST 25001
BOUND 522
LDIW
CONST 8
LSR
GLOBAL Out.Int
CALL 2
CONST 3
GLOBAL tProlog.%25
GLOBAL Out.String
CALL 2
RETURN
END

PROC tProlog.PrintClause 4 5 0
! PROCEDURE PrintClause(c: clause);
!   IF c = NULL THEN
LDLW 12
JNEQZ L286
!     Out.String("*null-clause*"); Out.Ln();
CONST 14
GLOBAL tProlog.%26
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
LABEL L286
!     IF mem[c+3] # NULL THEN
GLOBAL tProlog.mem
LDLW 12
CONST 3
PLUS
CONST 25001
BOUND 534
LDIW
JEQZ L278
!       PrintTerm(mem[c+3], NULL, MAXPRIO);
CONST 2
CONST 0
GLOBAL tProlog.mem
LDLW 12
CONST 3
PLUS
CONST 25001
BOUND 535
LDIW
GLOBAL tProlog.PrintTerm
CALL 3
!       Out.Char(' ')
CONST 32
ALIGNC
GLOBAL Out.Char
CALL 1
LABEL L278
!     Out.String(":- ");
CONST 4
GLOBAL tProlog.%27
GLOBAL Out.String
CALL 2
!     IF mem[(c+4)+1-1] # NULL THEN
GLOBAL tProlog.mem
LDLW 12
CONST 4
PLUS
CONST 25001
BOUND 539
LDIW
JEQZ L281
!       PrintTerm(mem[(c+4)+1-1], NULL, MAXPRIO);
CONST 2
CONST 0
GLOBAL tProlog.mem
LDLW 12
CONST 4
PLUS
CONST 25001
BOUND 540
LDIW
GLOBAL tProlog.PrintTerm
CALL 3
!       i := 2;
CONST 2
STLW -4
LABEL L282
!       WHILE mem[(c+4)+i-1] # NULL DO
GLOBAL tProlog.mem
LDLW 12
CONST 4
PLUS
LDLW -4
PLUS
DEC
CONST 25001
BOUND 542
LDIW
JEQZ L281
! 	Out.String(", ");
CONST 3
GLOBAL tProlog.%22
GLOBAL Out.String
CALL 2
! 	PrintTerm(mem[(c+4)+i-1], NULL, MAXPRIO);
CONST 2
CONST 0
GLOBAL tProlog.mem
LDLW 12
CONST 4
PLUS
LDLW -4
PLUS
DEC
CONST 25001
BOUND 544
LDIW
GLOBAL tProlog.PrintTerm
CALL 3
! 	i := i+1
INCL -4
JUMP L282
LABEL L281
!     Out.Char('.'); Out.Ln()
CONST 46
ALIGNC
GLOBAL Out.Char
CALL 1
GLOBAL Out.Ln
CALL 0
RETURN
END

PROC tProlog.ShowError 0 3 0
! PROCEDURE ShowError();
!   errflag := TRUE; errcount := errcount+1;
CONST 1
STGC tProlog.errflag
LDGW tProlog.errcount
INC
STGW tProlog.errcount
!   Out.String("Line "); Out.Int(lineno, 0); Out.Char(' ');
CONST 6
GLOBAL tProlog.%28
GLOBAL Out.String
CALL 2
CONST 0
LDGW tProlog.lineno
GLOBAL Out.Int
CALL 2
CONST 32
ALIGNC
GLOBAL Out.Char
CALL 1
!   Out.String("Syntax error - ")
CONST 16
GLOBAL tProlog.%29
GLOBAL Out.String
CALL 2
RETURN
END

PROC tProlog.Recover 4 3 0
! PROCEDURE Recover();
!   IF errcount >= 20 THEN
LDGW tProlog.errcount
CONST 20
JLT L289
!     Out.String("Too many errors: I am giving up"); Out.Ln(); HALT(2) 
CONST 32
GLOBAL tProlog.%30
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
CONST 2
GLOBAL HALT
CALL 1
LABEL L289
!   IF token # DOT THEN
LDGW tProlog.token
CONST 10
JEQ L292
LABEL L293
!       ch := GetChar()
GLOBAL tProlog.GetChar
CALLW 0
STLC -1
!     UNTIL (ch = '.') OR (ch = ENDFILE);
LDLC -1
CONST 46
JEQ L294
LDLC -1
CONST 127
JNEQ L293
LABEL L294
!     token := DOT
CONST 10
STGW tProlog.token
LABEL L292
RETURN
END

PROC tProlog.Scan 8 4 0
! PROCEDURE Scan();
!   ch := GetChar(); token := 0;
GLOBAL tProlog.GetChar
CALLW 0
STLC -1
CONST 0
STGW tProlog.token
LABEL L296
!   WHILE token = 0 DO
LDGW tProlog.token
JNEQZ L298
!     IF ch = ENDFILE THEN
LDLC -1
CONST 127
JNEQ L356
!       token := EOFTOK
CONST 14
STGW tProlog.token
JUMP L296
LABEL L356
!     ELSIF (ch = ' ') OR (ch = TAB) OR (ch = ENDLINE) THEN
LDLC -1
CONST 32
JEQ L357
LDLC -1
CONST 9
JEQ L357
LDLC -1
CONST 10
JNEQ L358
LABEL L357
!       ch := GetChar()
GLOBAL tProlog.GetChar
CALLW 0
STLC -1
JUMP L296
LABEL L358
!     ELSIF ((((ch >= 'A') & (ch <= 'Z')) OR (ch = '_')) OR ((ch >= 'a') & (ch <= 'z'))) THEN
LDLC -1
CONST 65
JLT L385
LDLC -1
CONST 90
JLEQ L361
LABEL L385
LDLC -1
CONST 95
JEQ L361
LDLC -1
CONST 97
JLT L362
LDLC -1
CONST 122
JGT L362
LABEL L361
!       IF (((ch >= 'A') & (ch <= 'Z')) OR (ch = '_')) THEN 
LDLC -1
CONST 65
JLT L366
LDLC -1
CONST 90
JLEQ L364
LABEL L366
LDLC -1
CONST 95
JNEQ L365
LABEL L364
! 	 token := VARIABLE
CONST 2
STGW tProlog.token
JUMP L363
LABEL L365
! 	 token := IDENT
CONST 1
STGW tProlog.token
LABEL L363
!       i := 0;
CONST 0
STLW -8
LABEL L368
!       WHILE ((((ch >= 'A') & (ch <= 'Z')) OR (ch = '_')) OR ((ch >= 'a') & (ch <= 'z'))) OR ((ch >= '0') & (ch <= '9')) DO
LDLC -1
CONST 65
JLT L378
LDLC -1
CONST 90
JLEQ L369
LABEL L378
LDLC -1
CONST 95
JEQ L369
LDLC -1
CONST 97
JLT L374
LDLC -1
CONST 122
JLEQ L369
LABEL L374
LDLC -1
CONST 48
JLT L370
LDLC -1
CONST 57
JGT L370
LABEL L369
!         IF i > MAXSTRING THEN
LDLW -8
CONST 128
JLEQ L373
!           Out.Ln(); Out.String("Panic: "); Out.String("identifier too long"); Out.Ln(); HALT(2)
GLOBAL Out.Ln
CALL 0
CONST 8
GLOBAL tProlog.%1
GLOBAL Out.String
CALL 2
CONST 20
GLOBAL tProlog.%31
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
CONST 2
GLOBAL HALT
CALL 1
LABEL L373
!         toksval[i] := ch; ch := GetChar(); i := i+1
LDLC -1
GLOBAL tProlog.toksval
LDLW -8
CONST 128
BOUND 625
STIC
GLOBAL tProlog.GetChar
CALLW 0
STLC -1
INCL -8
JUMP L368
LABEL L370
!       PushBack(ch);
LDLC -1
ALIGNC
GLOBAL tProlog.PushBack
CALL 1
!       toksval[i] := ENDSTR; tokval := Lookup(toksval);
CONST 0
GLOBAL tProlog.toksval
LDLW -8
CONST 128
BOUND 628
STIC
GLOBAL tProlog.toksval
GLOBAL tProlog.Lookup
CALLW 1
STGW tProlog.tokval
!       IF tokval = notsym THEN token := NEGATE END
LDGW tProlog.tokval
LDGW tProlog.notsym
JNEQ L296
CONST 13
STGW tProlog.token
JUMP L296
LABEL L362
!     ELSIF ((ch >= '0') & (ch <= '9')) THEN
LDLC -1
CONST 48
JLT L388
LDLC -1
CONST 57
JGT L388
!       token := NUMBER; tokival := 0;
CONST 3
STGW tProlog.token
CONST 0
STGW tProlog.tokival
LABEL L389
!       WHILE ((ch >= '0') & (ch <= '9')) DO
LDLC -1
CONST 48
JLT L391
LDLC -1
CONST 57
JGT L391
!         tokival := 10 * tokival + (ORD(ch) - ORD('0'));
LDGW tProlog.tokival
CONST 10
TIMES
LDLC -1
CONST 48
MINUS
PLUS
STGW tProlog.tokival
!         ch := GetChar()
GLOBAL tProlog.GetChar
CALLW 0
STLC -1
JUMP L389
LABEL L391
!       PushBack(ch)
LDLC -1
ALIGNC
GLOBAL tProlog.PushBack
CALL 1
JUMP L296
LABEL L388
!       CASE ch OF
LDLC -1
CONST 58
TESTGEQ L354
CONST 33
MINUS
JCASE 15
CASEL L310
CASEL L314
CASEL L309
CASEL L300
CASEL L300
CASEL L300
CASEL L313
CASEL L302
CASEL L303
CASEL L300
CASEL L300
CASEL L304
CASEL L300
CASEL L305
CASEL L311
JUMP L300
LABEL L354
CONST 58
MINUS
JCASE 5
CASEL L312
CASEL L300
CASEL L307
CASEL L306
CASEL L308
JUMP L300
LABEL L302
!         '(': token := LPAR
CONST 7
STGW tProlog.token
JUMP L296
LABEL L303
!       | ')': token := RPAR
CONST 8
STGW tProlog.token
JUMP L296
LABEL L304
!       | ',': token := COMMA
CONST 9
STGW tProlog.token
JUMP L296
LABEL L305
!       | '.': token := DOT
CONST 10
STGW tProlog.token
JUMP L296
LABEL L306
!       | '=': token := EQUAL
CONST 12
STGW tProlog.token
JUMP L296
LABEL L307
!       | '<': token := LANGLE
CONST 15
STGW tProlog.token
JUMP L296
LABEL L308
!       | '>': token := RANGLE
CONST 16
STGW tProlog.token
JUMP L296
LABEL L309
!       | '#': token := HASH
CONST 17
STGW tProlog.token
JUMP L296
LABEL L310
!       | '!': token := IDENT; tokval := cutsym
CONST 1
STGW tProlog.token
LDGW tProlog.cutsym
STGW tProlog.tokval
JUMP L296
LABEL L311
! 	  ch := GetChar();
GLOBAL tProlog.GetChar
CALLW 0
STLC -1
! 	  IF ch # '*' THEN
LDLC -1
CONST 42
JEQ L331
! 	    IF ~errflag THEN ShowError(); Out.String("bad token /"); Out.Ln(); Recover() END
LDGC tProlog.errflag
JNEQZ L296
GLOBAL tProlog.ShowError
CALL 0
CONST 12
GLOBAL tProlog.%32
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
GLOBAL tProlog.Recover
CALL 0
JUMP L296
LABEL L331
! 	    ch2 := ' '; ch := GetChar();
CONST 32
STLC -2
GLOBAL tProlog.GetChar
CALLW 0
STLC -1
LABEL L319
! 	    WHILE (ch # ENDFILE) & ~((ch2 = '*') & (ch = '/')) DO
LDLC -1
CONST 127
JEQ L321
LDLC -2
CONST 42
JNEQ L320
LDLC -1
CONST 47
JEQ L321
LABEL L320
! 	      ch2 := ch; ch := GetChar() 
LDLC -1
STLC -2
GLOBAL tProlog.GetChar
CALLW 0
STLC -1
JUMP L319
LABEL L321
! 	    IF ch = ENDFILE THEN
LDLC -1
CONST 127
JNEQ L326
! 	      IF ~errflag THEN ShowError(); Out.String("end of file in comment"); Out.Ln(); Recover() END
LDGC tProlog.errflag
JNEQZ L296
GLOBAL tProlog.ShowError
CALL 0
CONST 23
GLOBAL tProlog.%33
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
GLOBAL tProlog.Recover
CALL 0
JUMP L296
LABEL L326
! 	      ch := GetChar()
GLOBAL tProlog.GetChar
CALLW 0
STLC -1
JUMP L296
LABEL L312
! 	  ch := GetChar();
GLOBAL tProlog.GetChar
CALLW 0
STLC -1
! 	  IF ch = '-' THEN
LDLC -1
CONST 45
JNEQ L337
! 	    token := ARROW
CONST 6
STGW tProlog.token
JUMP L296
LABEL L337
! 	    PushBack(ch); token := COLON 
LDLC -1
ALIGNC
GLOBAL tProlog.PushBack
CALL 1
CONST 11
STGW tProlog.token
JUMP L296
LABEL L313
! 	  token := CHCON; tokival := ORD(GetChar()); ch := GetChar();
CONST 4
STGW tProlog.token
GLOBAL tProlog.GetChar
CALLW 0
STGW tProlog.tokival
GLOBAL tProlog.GetChar
CALLW 0
STLC -1
! 	  IF ch # "'" THEN IF ~errflag THEN ShowError(); Out.String("missing quote"); Out.Ln(); Recover() END END
LDLC -1
CONST 39
JEQ L296
LDGC tProlog.errflag
JNEQZ L296
GLOBAL tProlog.ShowError
CALL 0
CONST 14
GLOBAL tProlog.%34
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
GLOBAL tProlog.Recover
CALL 0
JUMP L296
LABEL L314
! 	  token := STRCON; i := 0; ch := GetChar();
CONST 5
STGW tProlog.token
CONST 0
STLW -8
GLOBAL tProlog.GetChar
CALLW 0
STLC -1
LABEL L344
! 	  WHILE (ch # '"') & (ch # ENDLINE) DO
LDLC -1
CONST 34
JEQ L346
LDLC -1
CONST 10
JEQ L346
! 	    toksval[i] := ch; ch := GetChar(); i := i+1 
LDLC -1
GLOBAL tProlog.toksval
LDLW -8
CONST 128
BOUND 676
STIC
GLOBAL tProlog.GetChar
CALLW 0
STLC -1
INCL -8
JUMP L344
LABEL L346
! 	  toksval[i] := ENDSTR;
CONST 0
GLOBAL tProlog.toksval
LDLW -8
CONST 128
BOUND 678
STIC
! 	  IF ch = ENDLINE THEN
LDLC -1
CONST 10
JNEQ L296
! 	    IF ~errflag THEN ShowError(); Out.String("unterminated string"); Out.Ln(); Recover() END;
LDGC tProlog.errflag
JNEQZ L353
GLOBAL tProlog.ShowError
CALL 0
CONST 20
GLOBAL tProlog.%35
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
GLOBAL tProlog.Recover
CALL 0
LABEL L353
! 	    PushBack(ch)
LDLC -1
ALIGNC
GLOBAL tProlog.PushBack
CALL 1
JUMP L296
LABEL L300
! 	IF ~errflag THEN ShowError(); Out.String("illegal character"); Out.Ln(); Recover() END; Out.Char(ch); Out.Ln()
LDGC tProlog.errflag
JNEQZ L317
GLOBAL tProlog.ShowError
CALL 0
CONST 18
GLOBAL tProlog.%36
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
GLOBAL tProlog.Recover
CALL 0
LABEL L317
LDLC -1
ALIGNC
GLOBAL Out.Char
CALL 1
GLOBAL Out.Ln
CALL 0
JUMP L296
LABEL L298
RETURN
END

PROC tProlog.PrintToken 0 3 0
! PROCEDURE PrintToken(t: INTEGER);
!   CASE t OF
LDLW 12
DEC
JCASE 17
CASEL L396
CASEL L397
CASEL L398
CASEL L399
CASEL L407
CASEL L400
CASEL L401
CASEL L402
CASEL L403
CASEL L404
CASEL L405
CASEL L406
CASEL L394
CASEL L394
CASEL L408
CASEL L409
CASEL L410
JUMP L394
LABEL L396
!       Out.String("identifier "); WriteString(symtab[tokval].name)
CONST 12
GLOBAL tProlog.%37
GLOBAL Out.String
CALL 2
GLOBAL tProlog.symtab
LDGW tProlog.tokval
CONST 512
BOUND 695
CONST 4
TIMES
LDIW
GLOBAL tProlog.WriteString
CALL 1
RETURN
LABEL L397
!       Out.String("variable "); WriteString(symtab[tokval].name)
CONST 10
GLOBAL tProlog.%38
GLOBAL Out.String
CALL 2
GLOBAL tProlog.symtab
LDGW tProlog.tokval
CONST 512
BOUND 697
CONST 4
TIMES
LDIW
GLOBAL tProlog.WriteString
CALL 1
RETURN
LABEL L398
!   | NUMBER: Out.String("number");
CONST 7
GLOBAL tProlog.%39
GLOBAL Out.String
CALL 2
RETURN
LABEL L399
!   | CHCON:  Out.String("char constant");
CONST 14
GLOBAL tProlog.%40
GLOBAL Out.String
CALL 2
RETURN
LABEL L400
!   | ARROW:  Out.String(":-");
CONST 3
GLOBAL tProlog.%41
GLOBAL Out.String
CALL 2
RETURN
LABEL L401
!   | LPAR:   Out.String("(");
CONST 2
GLOBAL tProlog.%113
GLOBAL Out.String
CALL 2
RETURN
LABEL L402
!   | RPAR:   Out.String(")");
CONST 2
GLOBAL tProlog.%114
GLOBAL Out.String
CALL 2
RETURN
LABEL L403
!   | COMMA:  Out.String(",");
CONST 2
GLOBAL tProlog.%115
GLOBAL Out.String
CALL 2
RETURN
LABEL L404
!   | DOT:    Out.String(".");
CONST 2
GLOBAL tProlog.%116
GLOBAL Out.String
CALL 2
RETURN
LABEL L405
!   | COLON:  Out.String(":");
CONST 2
GLOBAL tProlog.%110
GLOBAL Out.String
CALL 2
RETURN
LABEL L406
!   | EQUAL:  Out.String("=");
CONST 2
GLOBAL tProlog.%112
GLOBAL Out.String
CALL 2
RETURN
LABEL L407
!   | STRCON: Out.String("string constant")
CONST 16
GLOBAL tProlog.%42
GLOBAL Out.String
CALL 2
RETURN
LABEL L408
!   | LANGLE: Out.String("<")
CONST 2
GLOBAL tProlog.%117
GLOBAL Out.String
CALL 2
RETURN
LABEL L409
!   | RANGLE: Out.String(">")
CONST 2
GLOBAL tProlog.%118
GLOBAL Out.String
CALL 2
RETURN
LABEL L410
!   | HASH:   Out.String("#")
CONST 2
GLOBAL tProlog.%119
GLOBAL Out.String
CALL 2
RETURN
LABEL L394
!     Out.String("unknown token")
CONST 14
GLOBAL tProlog.%43
GLOBAL Out.String
CALL 2
RETURN
END

PROC tProlog.VarRep 4 4 0
! PROCEDURE VarRep(name: symbol): term;
!   IF nvars = MAXARITY THEN Out.Ln(); Out.String("Panic: "); Out.String("too many variables"); Out.Ln(); HALT(2) END;
LDGW tProlog.nvars
CONST 63
JNEQ L413
GLOBAL Out.Ln
CALL 0
CONST 8
GLOBAL tProlog.%1
GLOBAL Out.String
CALL 2
CONST 19
GLOBAL tProlog.%44
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
CONST 2
GLOBAL HALT
CALL 1
LABEL L413
!   i := 1; vartable[nvars+1] := name;  (* sentinel *)
CONST 1
STLW -4
LDLW 12
GLOBAL tProlog.vartable
LDGW tProlog.nvars
INC
CONST 64
BOUND 725
STIW
LABEL L414
!   WHILE name # vartable[i] DO i := i+1 END;
LDLW 12
GLOBAL tProlog.vartable
LDLW -4
CONST 64
BOUND 726
LDIW
JEQ L416
INCL -4
JUMP L414
LABEL L416
!   IF i = nvars+1 THEN nvars := nvars+1 END;
LDLW -4
LDGW tProlog.nvars
INC
JNEQ L419
LDGW tProlog.nvars
INC
STGW tProlog.nvars
LABEL L419
!   RETURN MakeRef(i)
LDLW -4
GLOBAL tProlog.MakeRef
CALLW 1
RETURN
END

PROC tProlog.ShowAnswer 8 5 0
! PROCEDURE ShowAnswer(bindings: frame);
!   IF nvars = 0 THEN
LDGW tProlog.nvars
JNEQZ L424
!     Out.String("yes"); Out.Ln()
CONST 4
GLOBAL tProlog.%45
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
LABEL L424
!     FOR i := 1 TO nvars DO
LDGW tProlog.nvars
STLW -8
CONST 1
STLW -4
LABEL L421
LDLW -4
LDLW -8
JGT L422
!       WriteString(symtab[vartable[i]].name); Out.String(" = ");
GLOBAL tProlog.symtab
GLOBAL tProlog.vartable
LDLW -4
CONST 64
BOUND 739
LDIW
CONST 512
BOUND 739
CONST 4
TIMES
LDIW
GLOBAL tProlog.WriteString
CALL 1
CONST 4
GLOBAL tProlog.%20
GLOBAL Out.String
CALL 2
!       PrintTerm((bindings+7+(i-1)*TERMSIZE), NULL, EQPRIO-1);
CONST 1
CONST 0
LDLW 12
CONST 7
PLUS
LDLW -4
DEC
CONST 2
TIMES
PLUS
GLOBAL tProlog.PrintTerm
CALL 3
!       Out.Ln()
GLOBAL Out.Ln
CALL 0
!     FOR i := 1 TO nvars DO
INCL -4
JUMP L421
LABEL L422
RETURN
END

PROC tProlog.Eat 0 3 0
! PROCEDURE Eat(expected: INTEGER);
!   IF token = expected THEN
LDGW tProlog.token
LDLW 12
JNEQ L427
!     IF token # DOT THEN Scan() END
LDGW tProlog.token
CONST 10
JEQ L432
GLOBAL tProlog.Scan
CALL 0
RETURN
LABEL L427
!   ELSIF ~errflag THEN
LDGC tProlog.errflag
JNEQZ L432
!     ShowError();
GLOBAL tProlog.ShowError
CALL 0
!     Out.String("expected "); PrintToken(expected);
CONST 10
GLOBAL tProlog.%46
GLOBAL Out.String
CALL 2
LDLW 12
GLOBAL tProlog.PrintToken
CALL 1
!     Out.String(", found "); PrintToken(token); Out.Ln();
CONST 9
GLOBAL tProlog.%47
GLOBAL Out.String
CALL 2
LDGW tProlog.token
GLOBAL tProlog.PrintToken
CALL 1
GLOBAL Out.Ln
CALL 0
!     Recover()
GLOBAL tProlog.Recover
CALL 0
LABEL L432
RETURN
END

PROC tProlog.ParseCompound 264 4 0
! PROCEDURE ParseCompound(): term;
!   fun := tokval; n := 0; Eat(IDENT);
LDGW tProlog.tokval
STLW -4
CONST 0
STLW -264
CONST 1
GLOBAL tProlog.Eat
CALL 1
!   IF token = LPAR THEN
LDGW tProlog.token
CONST 7
JNEQ L435
!     Eat(LPAR); n := 1; arg[1] := ParseTerm();
CONST 7
GLOBAL tProlog.Eat
CALL 1
CONST 1
STLW -264
GLOBAL tProlog.ParseTerm
CALLW 0
STLW -256
LABEL L436
!     WHILE token = COMMA DO
LDGW tProlog.token
CONST 9
JNEQ L438
!       Eat(COMMA); n := n+1; arg[n] := ParseTerm()
CONST 9
GLOBAL tProlog.Eat
CALL 1
INCL -264
GLOBAL tProlog.ParseTerm
CALLW 0
LOCAL -260
LDLW -264
CONST 64
BOUND 767
STIW
JUMP L436
LABEL L438
!     Eat(RPAR)
CONST 8
GLOBAL tProlog.Eat
CALL 1
LABEL L435
!   IF symtab[fun].arity = -1 THEN
GLOBAL tProlog.symtab
LDLW -4
CONST 512
BOUND 771
CONST 16
TIMES
OFFSET
LDNW 4
CONST -1
JNEQ L441
!     symtab[fun].arity := n
LDLW -264
GLOBAL tProlog.symtab
LDLW -4
CONST 512
BOUND 772
CONST 16
TIMES
OFFSET
STNW 4
JUMP L443
LABEL L441
!   ELSIF symtab[fun].arity # n THEN
GLOBAL tProlog.symtab
LDLW -4
CONST 512
BOUND 773
CONST 16
TIMES
OFFSET
LDNW 4
LDLW -264
JEQ L443
!     IF ~errflag THEN ShowError(); Out.String("wrong number of args"); Out.Ln(); Recover() END
LDGC tProlog.errflag
JNEQZ L443
GLOBAL tProlog.ShowError
CALL 0
CONST 21
GLOBAL tProlog.%48
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
GLOBAL tProlog.Recover
CALL 0
LABEL L443
!   RETURN MakeCompound(fun, arg)
LOCAL -260
LDLW -4
GLOBAL tProlog.MakeCompound
CALLW 2
RETURN
END

PROC tProlog.ParsePrimary 4 3 0
! PROCEDURE ParsePrimary(): term;
!   IF token = IDENT THEN t := ParseCompound()
LDGW tProlog.token
CONST 1
JNEQ L452
GLOBAL tProlog.ParseCompound
CALLW 0
STLW -4
JUMP L447
LABEL L452
!   ELSIF token = VARIABLE THEN
LDGW tProlog.token
CONST 2
JNEQ L454
!     t := VarRep(tokval); Eat(VARIABLE)
LDGW tProlog.tokval
GLOBAL tProlog.VarRep
CALLW 1
STLW -4
CONST 2
GLOBAL tProlog.Eat
CALL 1
JUMP L447
LABEL L454
!   ELSIF token = NUMBER THEN
LDGW tProlog.token
CONST 3
JNEQ L456
!     t := MakeInt(tokival); Eat(NUMBER)
LDGW tProlog.tokival
GLOBAL tProlog.MakeInt
CALLW 1
STLW -4
CONST 3
GLOBAL tProlog.Eat
CALL 1
JUMP L447
LABEL L456
!   ELSIF token = CHCON THEN
LDGW tProlog.token
CONST 4
JNEQ L458
!     t := MakeChar(CHR(tokival)); Eat(CHCON)
LDGW tProlog.tokival
CONVNC
ALIGNC
GLOBAL tProlog.MakeChar
CALLW 1
STLW -4
CONST 4
GLOBAL tProlog.Eat
CALL 1
JUMP L447
LABEL L458
!   ELSIF token = STRCON THEN
LDGW tProlog.token
CONST 5
JNEQ L460
!     t := MakeString(toksval); Eat(STRCON)
GLOBAL tProlog.toksval
GLOBAL tProlog.MakeString
CALLW 1
STLW -4
CONST 5
GLOBAL tProlog.Eat
CALL 1
JUMP L447
LABEL L460
!   ELSIF token = LPAR THEN
LDGW tProlog.token
CONST 7
JNEQ L462
!     Eat(LPAR); t := ParseTerm(); Eat(RPAR)
CONST 7
GLOBAL tProlog.Eat
CALL 1
GLOBAL tProlog.ParseTerm
CALLW 0
STLW -4
CONST 8
GLOBAL tProlog.Eat
CALL 1
JUMP L447
LABEL L462
!   ELSIF token = LANGLE THEN
LDGW tProlog.token
CONST 15
JNEQ L464
!     t := ParseNode()
GLOBAL tProlog.ParseNode
CALLW 0
STLW -4
JUMP L447
LABEL L464
!     IF ~errflag THEN ShowError(); Out.String("expected a term"); Out.Ln(); Recover() END; t := NULL
LDGC tProlog.errflag
JNEQZ L450
GLOBAL tProlog.ShowError
CALL 0
CONST 16
GLOBAL tProlog.%49
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
GLOBAL tProlog.Recover
CALL 0
LABEL L450
CONST 0
STLW -4
LABEL L447
!   RETURN t
LDLW -4
RETURN
END

PROC tProlog.ParseNode 8 4 0
! PROCEDURE ParseNode(): term;
!   Eat(LANGLE);
CONST 15
GLOBAL tProlog.Eat
CALL 1
!   tag := ParseTerm();
GLOBAL tProlog.ParseTerm
CALLW 0
STLW -4
!   kids := ParseKids();
GLOBAL tProlog.ParseKids
CALLW 0
STLW -8
!   Eat(RANGLE);
CONST 16
GLOBAL tProlog.Eat
CALL 1
!   RETURN MakeNode(node, tag, kids)
LDLW -8
LDLW -4
LDGW tProlog.node
GLOBAL tProlog.MakeNode
CALLW 3
RETURN
END

PROC tProlog.ParseKids 8 4 0
! PROCEDURE ParseKids(): term;
!   IF token # COMMA THEN
LDGW tProlog.token
CONST 9
JEQ L467
!     RETURN MakeNode(nilsym, NULL, NULL)
CONST 0
CONST 0
LDGW tProlog.nilsym
GLOBAL tProlog.MakeNode
CALLW 3
RETURN
LABEL L467
!     Eat(COMMA);
CONST 9
GLOBAL tProlog.Eat
CALL 1
!     head := ParseTerm();
GLOBAL tProlog.ParseTerm
CALLW 0
STLW -4
!     tail := ParseKids();
GLOBAL tProlog.ParseKids
CALLW 0
STLW -8
!     RETURN MakeNode(cons, head, tail)
LDLW -8
LDLW -4
LDGW tProlog.cons
GLOBAL tProlog.MakeNode
CALLW 3
RETURN
END

PROC tProlog.ParseFactor 4 4 0
! PROCEDURE ParseFactor(): term;
!   t := ParsePrimary();
GLOBAL tProlog.ParsePrimary
CALLW 0
STLW -4
!   IF token # COLON THEN
LDGW tProlog.token
CONST 11
JEQ L470
!     RETURN t
LDLW -4
RETURN
LABEL L470
!     Eat(COLON);
CONST 11
GLOBAL tProlog.Eat
CALL 1
!     RETURN MakeNode(cons, t, ParseFactor())
GLOBAL tProlog.ParseFactor
CALLW 0
LDLW -4
LDGW tProlog.cons
GLOBAL tProlog.MakeNode
CALLW 3
RETURN
END

PROC tProlog.ParseTerm 4 4 0
! PROCEDURE ParseTerm(): term;
!   t := ParseFactor();
GLOBAL tProlog.ParseFactor
CALLW 0
STLW -4
!   IF token # EQUAL THEN
LDGW tProlog.token
CONST 12
JEQ L473
!     RETURN t
LDLW -4
RETURN
LABEL L473
!     Eat(EQUAL);
CONST 12
GLOBAL tProlog.Eat
CALL 1
!     RETURN MakeNode(eqsym, t, ParseFactor())
GLOBAL tProlog.ParseFactor
CALLW 0
LDLW -4
LDGW tProlog.eqsym
GLOBAL tProlog.MakeNode
CALLW 3
RETURN
END

PROC tProlog.CheckAtom 0 3 0
! PROCEDURE CheckAtom(a: term);
!   IF LSR(mem[a], 8) # FUNC THEN
GLOBAL tProlog.mem
LDLW 12
CONST 25001
BOUND 856
LDIW
CONST 8
LSR
CONST 1
JEQ L476
!     IF ~errflag THEN ShowError(); Out.String("literal must be a compound term"); Out.Ln(); Recover() END
LDGC tProlog.errflag
JNEQZ L476
GLOBAL tProlog.ShowError
CALL 0
CONST 32
GLOBAL tProlog.%50
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
GLOBAL tProlog.Recover
CALL 0
LABEL L476
RETURN
END

PROC tProlog.ParseClause 272 5 0
! PROCEDURE ParseClause(): clause;
!   IF token = HASH THEN
LDGW tProlog.token
CONST 17
JNEQ L482
!     Eat(HASH); head := NULL
CONST 17
GLOBAL tProlog.Eat
CALL 1
CONST 0
STLW -4
JUMP L480
LABEL L482
!     head := ParseTerm();
GLOBAL tProlog.ParseTerm
CALLW 0
STLW -4
!     CheckAtom(head)
LDLW -4
GLOBAL tProlog.CheckAtom
CALL 1
LABEL L480
!   Eat(ARROW);
CONST 6
GLOBAL tProlog.Eat
CALL 1
!   n := 0;
CONST 0
STLW -268
!   IF token # DOT THEN
LDGW tProlog.token
CONST 10
JEQ L485
!     more := TRUE;
CONST 1
STLC -270
LABEL L486
!     WHILE more DO
LDLC -270
JEQZ L485
!       n := n+1; minus := FALSE;
INCL -268
CONST 0
STLC -269
!       IF token = NEGATE THEN
LDGW tProlog.token
CONST 13
JNEQ L491
! 	Eat(NEGATE); minus := TRUE 
CONST 13
GLOBAL tProlog.Eat
CALL 1
CONST 1
STLC -269
LABEL L491
!       t := ParseTerm(); CheckAtom(t);
GLOBAL tProlog.ParseTerm
CALLW 0
STLW -8
LDLW -8
GLOBAL tProlog.CheckAtom
CALL 1
!       IF minus THEN 
LDLC -269
JEQZ L494
! 	body[n] := MakeNode(notsym, t, NULL)
CONST 0
LDLW -8
LDGW tProlog.notsym
GLOBAL tProlog.MakeNode
CALLW 3
LOCAL -264
LDLW -268
CONST 64
BOUND 885
STIW
JUMP L492
LABEL L494
!         body[n] := t
LDLW -8
LOCAL -264
LDLW -268
CONST 64
BOUND 887
STIW
LABEL L492
!       IF token = COMMA THEN Eat(COMMA) ELSE more := FALSE END
LDGW tProlog.token
CONST 9
JNEQ L497
CONST 9
GLOBAL tProlog.Eat
CALL 1
JUMP L486
LABEL L497
CONST 0
STLC -270
JUMP L486
LABEL L485
!   Eat(DOT);
CONST 10
GLOBAL tProlog.Eat
CALL 1
!   IF errflag THEN 
LDGC tProlog.errflag
JEQZ L500
!     RETURN NULL
CONST 0
RETURN
LABEL L500
!     RETURN MakeClause(nvars, head, body, n)
LDLW -268
LOCAL -264
LDLW -4
LDGW tProlog.nvars
GLOBAL tProlog.MakeClause
CALLW 4
RETURN
END

PROC tProlog.ReadClause 4 2 0
! PROCEDURE ReadClause(): clause;
LABEL L501
!     hp := hmark; nvars := 0; errflag := FALSE;
LDGW tProlog.hmark
STGW tProlog.hp
CONST 0
STGW tProlog.nvars
CONST 0
STGC tProlog.errflag
!     Scan();
GLOBAL tProlog.Scan
CALL 0
!     IF token = EOFTOK THEN 
LDGW tProlog.token
CONST 14
JNEQ L506
!       c := NULL
CONST 0
STLW -4
JUMP L504
LABEL L506
!       c := ParseClause()
GLOBAL tProlog.ParseClause
CALLW 0
STLW -4
LABEL L504
!   UNTIL (~errflag) OR (token = EOFTOK);
LDGC tProlog.errflag
JEQZ L502
LDGW tProlog.token
CONST 14
JNEQ L501
LABEL L502
!   RETURN c
LDLW -4
RETURN
END

PROC tProlog.Save 4 4 0
! PROCEDURE Save(v: term);
!   IF ((v < choice) OR (v >= mem[choice+4])) THEN
LDLW 12
LDGW tProlog.choice
JLT L508
LDLW 12
GLOBAL tProlog.mem
LDGW tProlog.choice
CONST 4
PLUS
CONST 25001
BOUND 927
LDIW
JLT L509
LABEL L508
!     p := GloAlloc(UNDO, TRAILSIZE);
CONST 3
CONST 6
GLOBAL tProlog.GloAlloc
CALLW 2
STLW -4
!     mem[p+1] := v; mem[p+2] := trhead; trhead := p
LDLW 12
GLOBAL tProlog.mem
LDLW -4
INC
CONST 25001
BOUND 929
STIW
LDGW tProlog.trhead
GLOBAL tProlog.mem
LDLW -4
CONST 2
PLUS
CONST 25001
BOUND 929
STIW
LDLW -4
STGW tProlog.trhead
LABEL L509
RETURN
END

PROC tProlog.Restore 4 4 0
! PROCEDURE Restore();
LABEL L511
!   WHILE (trhead # mem[choice+5]) DO
LDGW tProlog.trhead
GLOBAL tProlog.mem
LDGW tProlog.choice
CONST 5
PLUS
CONST 25001
BOUND 937
LDIW
JEQ L513
!     v := mem[trhead+1];
GLOBAL tProlog.mem
LDGW tProlog.trhead
INC
CONST 25001
BOUND 938
LDIW
STLW -4
!     IF v # NULL THEN mem[v+1] := NULL END;
LDLW -4
JEQZ L516
CONST 0
GLOBAL tProlog.mem
LDLW -4
INC
CONST 25001
BOUND 939
STIW
LABEL L516
!     trhead := mem[trhead+2]
GLOBAL tProlog.mem
LDGW tProlog.trhead
CONST 2
PLUS
CONST 25001
BOUND 940
LDIW
STGW tProlog.trhead
JUMP L511
LABEL L513
RETURN
END

PROC tProlog.Commit 4 4 0
! PROCEDURE Commit();
!   p := trhead;
LDGW tProlog.trhead
STLW -4
LABEL L517
!   WHILE (p # NULL) & (p < mem[choice+4]) DO
LDLW -4
JEQZ L519
LDLW -4
GLOBAL tProlog.mem
LDGW tProlog.choice
CONST 4
PLUS
CONST 25001
BOUND 949
LDIW
JGEQ L519
!     IF (mem[p+1] # NULL) & ~((mem[p+1] < choice) OR (mem[p+1] >= mem[choice+4])) THEN
GLOBAL tProlog.mem
LDLW -4
INC
CONST 25001
BOUND 950
LDIW
JEQZ L522
GLOBAL tProlog.mem
LDLW -4
INC
CONST 25001
BOUND 950
LDIW
LDGW tProlog.choice
JLT L522
GLOBAL tProlog.mem
LDLW -4
INC
CONST 25001
BOUND 950
LDIW
GLOBAL tProlog.mem
LDGW tProlog.choice
CONST 4
PLUS
CONST 25001
BOUND 950
LDIW
JGEQ L522
!       mem[p+1] := NULL
CONST 0
GLOBAL tProlog.mem
LDLW -4
INC
CONST 25001
BOUND 951
STIW
LABEL L522
!     p := mem[p+2]
GLOBAL tProlog.mem
LDLW -4
CONST 2
PLUS
CONST 25001
BOUND 953
LDIW
STLW -4
JUMP L517
LABEL L519
RETURN
END

PROC tProlog.GloCopy 16 4 0
! PROCEDURE GloCopy(t: term; e: frame): term;
!   t := Deref(t, e);
LDLW 16
LDLW 12
GLOBAL tProlog.Deref
CALLW 2
STLW 12
!   IF (t >= gsp) THEN
LDLW 12
LDGW tProlog.gsp
JLT L538
!     RETURN t
LDLW 12
RETURN
LABEL L538
!     CASE LSR(mem[t], 8) OF
GLOBAL tProlog.mem
LDLW 12
CONST 25001
BOUND 965
LDIW
CONST 8
LSR
DEC
JCASE 4
CASEL L529
CASEL L527
CASEL L527
CASEL L530
JUMP L527
LABEL L529
! 	n := symtab[mem[t+1]].arity;
GLOBAL tProlog.symtab
GLOBAL tProlog.mem
LDLW 12
INC
CONST 25001
BOUND 967
LDIW
CONST 512
BOUND 967
CONST 16
TIMES
OFFSET
LDNW 4
STLW -12
! 	IF (t <= hp) & (n = 0) THEN 
LDLW 12
LDGW tProlog.hp
JGT L535
LDLW -12
JNEQZ L535
! 	  RETURN t
LDLW 12
RETURN
LABEL L535
! 	  tt := GloAlloc(FUNC, TERMSIZE+n);
LDLW -12
CONST 2
PLUS
CONST 1
GLOBAL tProlog.GloAlloc
CALLW 2
STLW -4
! 	  mem[tt+1] := mem[t+1];
GLOBAL tProlog.mem
LDLW 12
INC
CONST 25001
BOUND 972
LDIW
GLOBAL tProlog.mem
LDLW -4
INC
CONST 25001
BOUND 972
STIW
! 	  FOR i := 1 TO n DO
LDLW -12
STLW -16
CONST 1
STLW -8
LABEL L532
LDLW -8
LDLW -16
JGT L533
! 	    mem[tt+i+1] := GloCopy(mem[t+i+1], e)
LDLW 16
GLOBAL tProlog.mem
LDLW 12
LDLW -8
PLUS
INC
CONST 25001
BOUND 974
LDIW
GLOBAL tProlog.GloCopy
CALLW 2
GLOBAL tProlog.mem
LDLW -4
LDLW -8
PLUS
INC
CONST 25001
BOUND 974
STIW
! 	  FOR i := 1 TO n DO
INCL -8
JUMP L532
LABEL L533
! 	  RETURN tt
LDLW -4
RETURN
LABEL L530
!         tt := GloAlloc(CELL, TERMSIZE);
CONST 2
CONST 4
GLOBAL tProlog.GloAlloc
CALLW 2
STLW -4
!         mem[tt+1] := NULL;
CONST 0
GLOBAL tProlog.mem
LDLW -4
INC
CONST 25001
BOUND 980
STIW
! 	Save(t); mem[t+1] := tt;
LDLW 12
GLOBAL tProlog.Save
CALL 1
LDLW -4
GLOBAL tProlog.mem
LDLW 12
INC
CONST 25001
BOUND 981
STIW
!         RETURN tt
LDLW -4
RETURN
LABEL L527
!       RETURN t
LDLW 12
RETURN
END

PROC tProlog.Share 0 4 0
! PROCEDURE Share(v1, v2: term);
!   IF (v1 * (2 * ORD((v1 >= gsp)) - 1)) <= (v2 * (2 * ORD((v2 >= gsp)) - 1)) THEN
LDLW 12
LDLW 12
LDGW tProlog.gsp
GEQ
CONST 2
TIMES
DEC
TIMES
LDLW 16
LDLW 16
LDGW tProlog.gsp
GEQ
CONST 2
TIMES
DEC
TIMES
JGT L541
!     Save(v1); mem[v1+1] := v2
LDLW 12
GLOBAL tProlog.Save
CALL 1
LDLW 16
GLOBAL tProlog.mem
LDLW 12
INC
CONST 25001
BOUND 993
STIW
RETURN
LABEL L541
!     Save(v2); mem[v2+1] := v1 
LDLW 16
GLOBAL tProlog.Save
CALL 1
LDLW 12
GLOBAL tProlog.mem
LDLW 16
INC
CONST 25001
BOUND 995
STIW
RETURN
END

PROC tProlog.Unify 8 6 0
! PROCEDURE Unify(t1: term; e1: frame; t2: term; e2: frame): BOOLEAN;
!   t1 := Deref(t1, e1); t2 := Deref(t2, e2);
LDLW 16
LDLW 12
GLOBAL tProlog.Deref
CALLW 2
STLW 12
LDLW 24
LDLW 20
GLOBAL tProlog.Deref
CALLW 2
STLW 20
!   IF t1 = t2 THEN  (* Includes unifying a VAR with itself *)
LDLW 12
LDLW 20
JNEQ L556
!     RETURN TRUE
CONST 1
RETURN
LABEL L556
!   ELSIF (LSR(mem[t1], 8) = CELL) & (LSR(mem[t2], 8) = CELL) THEN
GLOBAL tProlog.mem
LDLW 12
CONST 25001
BOUND 1006
LDIW
CONST 8
LSR
CONST 4
JNEQ L558
GLOBAL tProlog.mem
LDLW 20
CONST 25001
BOUND 1006
LDIW
CONST 8
LSR
CONST 4
JNEQ L558
!     Share(t1, t2); RETURN TRUE
LDLW 20
LDLW 12
GLOBAL tProlog.Share
CALL 2
CONST 1
RETURN
LABEL L558
!   ELSIF LSR(mem[t1], 8) = CELL THEN
GLOBAL tProlog.mem
LDLW 12
CONST 25001
BOUND 1008
LDIW
CONST 8
LSR
CONST 4
JNEQ L561
!     Save(t1); mem[t1+1] := GloCopy(t2, e2); RETURN TRUE
LDLW 12
GLOBAL tProlog.Save
CALL 1
LDLW 24
LDLW 20
GLOBAL tProlog.GloCopy
CALLW 2
GLOBAL tProlog.mem
LDLW 12
INC
CONST 25001
BOUND 1009
STIW
CONST 1
RETURN
LABEL L561
!   ELSIF LSR(mem[t2], 8) = CELL THEN
GLOBAL tProlog.mem
LDLW 20
CONST 25001
BOUND 1010
LDIW
CONST 8
LSR
CONST 4
JNEQ L563
!     Save(t2); mem[t2+1] := GloCopy(t1, e1); RETURN TRUE
LDLW 20
GLOBAL tProlog.Save
CALL 1
LDLW 16
LDLW 12
GLOBAL tProlog.GloCopy
CALLW 2
GLOBAL tProlog.mem
LDLW 20
INC
CONST 25001
BOUND 1011
STIW
CONST 1
RETURN
LABEL L563
!   ELSIF LSR(mem[t1], 8) # LSR(mem[t2], 8) THEN
GLOBAL tProlog.mem
LDLW 12
CONST 25001
BOUND 1012
LDIW
CONST 8
LSR
GLOBAL tProlog.mem
LDLW 20
CONST 25001
BOUND 1012
LDIW
CONST 8
LSR
JEQ L565
!     RETURN FALSE
CONST 0
RETURN
LABEL L565
!     CASE LSR(mem[t1], 8) OF
GLOBAL tProlog.mem
LDLW 12
CONST 25001
BOUND 1015
LDIW
CONST 8
LSR
DEC
JCASE 3
CASEL L545
CASEL L546
CASEL L547
JUMP L543
LABEL L545
!         IF (mem[t1+1] # mem[t2+1]) THEN
GLOBAL tProlog.mem
LDLW 12
INC
CONST 25001
BOUND 1017
LDIW
GLOBAL tProlog.mem
LDLW 20
INC
CONST 25001
BOUND 1017
LDIW
JEQ L554
!           RETURN FALSE
CONST 0
RETURN
LABEL L554
!           i := 1; match := TRUE;
CONST 1
STLW -4
CONST 1
STLC -5
LABEL L549
!           WHILE match & (i <= symtab[mem[t1+1]].arity) DO
LDLC -5
JEQZ L551
LDLW -4
GLOBAL tProlog.symtab
GLOBAL tProlog.mem
LDLW 12
INC
CONST 25001
BOUND 1021
LDIW
CONST 512
BOUND 1021
CONST 16
TIMES
OFFSET
LDNW 4
JGT L551
!             match := Unify(mem[t1+i+1], e1, mem[t2+i+1], e2);
LDLW 24
GLOBAL tProlog.mem
LDLW 20
LDLW -4
PLUS
INC
CONST 25001
BOUND 1022
LDIW
LDLW 16
GLOBAL tProlog.mem
LDLW 12
LDLW -4
PLUS
INC
CONST 25001
BOUND 1022
LDIW
GLOBAL tProlog.Unify
CALLW 4
STLC -5
!             i := i+1
INCL -4
JUMP L549
LABEL L551
!           RETURN match
LDLC -5
RETURN
LABEL L546
!         RETURN (mem[t1+1] = mem[t2+1])
GLOBAL tProlog.mem
LDLW 12
INC
CONST 25001
BOUND 1028
LDIW
GLOBAL tProlog.mem
LDLW 20
INC
CONST 25001
BOUND 1028
LDIW
EQ
RETURN
LABEL L547
!         RETURN (mem[t1+1] = mem[t2+1])
GLOBAL tProlog.mem
LDLW 12
INC
CONST 25001
BOUND 1030
LDIW
GLOBAL tProlog.mem
LDLW 20
INC
CONST 25001
BOUND 1030
LDIW
EQ
RETURN
LABEL L543
!       Out.Ln(); Out.String("Panic: "); Out.String("bad tag" (*t_kind(t1):1, " in ", "Unify"*)); Out.Ln(); HALT(2)
GLOBAL Out.Ln
CALL 0
CONST 8
GLOBAL tProlog.%1
GLOBAL Out.String
CALL 2
CONST 8
GLOBAL tProlog.%51
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
CONST 2
GLOBAL HALT
CALL 1
ERROR E_RETURN 1000
END

PROC tProlog.Key 4 4 0
! PROCEDURE Key(t: term; e: frame): INTEGER;
!   IF t = NULL THEN Out.Ln(); Out.String("Panic: "); Out.String("Key"); Out.Ln(); HALT(2) END;
LDLW 12
JNEQZ L568
GLOBAL Out.Ln
CALL 0
CONST 8
GLOBAL tProlog.%1
GLOBAL Out.String
CALL 2
CONST 4
GLOBAL tProlog.%52
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
CONST 2
GLOBAL HALT
CALL 1
LABEL L568
!   IF LSR(mem[t], 8) # FUNC THEN Out.Ln(); Out.String("Panic: "); Out.String("bad tag" (*t_kind(t):1, " in ", "Key1"*)); Out.Ln(); HALT(2) END;
GLOBAL tProlog.mem
LDLW 12
CONST 25001
BOUND 1046
LDIW
CONST 8
LSR
CONST 1
JEQ L571
GLOBAL Out.Ln
CALL 0
CONST 8
GLOBAL tProlog.%1
GLOBAL Out.String
CALL 2
CONST 8
GLOBAL tProlog.%51
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
CONST 2
GLOBAL HALT
CALL 1
LABEL L571
!   IF symtab[mem[t+1]].arity = 0 THEN
GLOBAL tProlog.symtab
GLOBAL tProlog.mem
LDLW 12
INC
CONST 25001
BOUND 1048
LDIW
CONST 512
BOUND 1048
CONST 16
TIMES
OFFSET
LDNW 4
JNEQZ L579
!     RETURN 0
CONST 0
RETURN
LABEL L579
!     t0 := Deref(mem[t+1+1], e);
LDLW 16
GLOBAL tProlog.mem
LDLW 12
CONST 2
PLUS
CONST 25001
BOUND 1051
LDIW
GLOBAL tProlog.Deref
CALLW 2
STLW -4
!     CASE LSR(mem[t0], 8) OF
GLOBAL tProlog.mem
LDLW -4
CONST 25001
BOUND 1052
LDIW
CONST 8
LSR
DEC
JCASE 3
CASEL L575
CASEL L576
CASEL L577
JUMP L573
LABEL L575
!         FUNC:      RETURN mem[t0+1]
GLOBAL tProlog.mem
LDLW -4
INC
CONST 25001
BOUND 1053
LDIW
RETURN
LABEL L576
!       | INT:       RETURN mem[t0+1] + 1
GLOBAL tProlog.mem
LDLW -4
INC
CONST 25001
BOUND 1054
LDIW
INC
RETURN
LABEL L577
!       | CHRCTR:    RETURN mem[t0+1] + 1
GLOBAL tProlog.mem
LDLW -4
INC
CONST 25001
BOUND 1055
LDIW
INC
RETURN
LABEL L573
!       RETURN 0
CONST 0
RETURN
END

PROC tProlog.Search 4 3 0
! PROCEDURE Search(t: term; e: frame; p: clause): clause;
!   k := Key(t, e);
LDLW 16
LDLW 12
GLOBAL tProlog.Key
CALLW 2
STLW -4
!   IF k # 0 THEN
LDLW -4
JEQZ L582
LABEL L583
!     WHILE (p # NULL) & (mem[p+1] # 0) & (mem[p+1] # k) DO
LDLW 20
JEQZ L582
GLOBAL tProlog.mem
LDLW 20
INC
CONST 25001
BOUND 1068
LDIW
JEQZ L582
GLOBAL tProlog.mem
LDLW 20
INC
CONST 25001
BOUND 1068
LDIW
LDLW -4
JEQ L582
!       p := mem[p+2]
GLOBAL tProlog.mem
LDLW 20
CONST 2
PLUS
CONST 25001
BOUND 1069
LDIW
STLW 20
JUMP L583
LABEL L582
!   RETURN p
LDLW 20
RETURN
END

PROC tProlog.PushFrame 12 5 0
! PROCEDURE PushFrame(nvars: INTEGER; retry: clause);
!   f := LocAlloc((FRSIZE + (nvars)*TERMSIZE));
LDLW 12
CONST 2
TIMES
CONST 7
PLUS
GLOBAL tProlog.LocAlloc
CALLW 1
STLW -4
!   mem[f] := current; mem[f+1] := goalframe;
LDGW tProlog.current
GLOBAL tProlog.mem
LDLW -4
CONST 25001
BOUND 1082
STIW
LDGW tProlog.goalframe
GLOBAL tProlog.mem
LDLW -4
INC
CONST 25001
BOUND 1082
STIW
!   mem[f+2] := retry; mem[f+3] := choice;
LDLW 16
GLOBAL tProlog.mem
LDLW -4
CONST 2
PLUS
CONST 25001
BOUND 1083
STIW
LDGW tProlog.choice
GLOBAL tProlog.mem
LDLW -4
CONST 3
PLUS
CONST 25001
BOUND 1083
STIW
!   mem[f+4] := gsp; mem[f+5] := trhead;
LDGW tProlog.gsp
GLOBAL tProlog.mem
LDLW -4
CONST 4
PLUS
CONST 25001
BOUND 1084
STIW
LDGW tProlog.trhead
GLOBAL tProlog.mem
LDLW -4
CONST 5
PLUS
CONST 25001
BOUND 1084
STIW
!   mem[f+6] := nvars;
LDLW 12
GLOBAL tProlog.mem
LDLW -4
CONST 6
PLUS
CONST 25001
BOUND 1085
STIW
!   FOR i := 1 TO nvars DO
LDLW 12
STLW -12
CONST 1
STLW -8
LABEL L588
LDLW -8
LDLW -12
JGT L589
!     mem[(f+7+(i-1)*TERMSIZE)] := LSL(CELL, 8) + TERMSIZE;
CONST 1026
GLOBAL tProlog.mem
LDLW -4
CONST 7
PLUS
LDLW -8
DEC
CONST 2
TIMES
PLUS
CONST 25001
BOUND 1087
STIW
!     mem[(f+7+(i-1)*TERMSIZE)+1] := NULL
CONST 0
GLOBAL tProlog.mem
LDLW -4
CONST 7
PLUS
LDLW -8
DEC
CONST 2
TIMES
PLUS
INC
CONST 25001
BOUND 1088
STIW
!   FOR i := 1 TO nvars DO
INCL -8
JUMP L588
LABEL L589
!   goalframe := f;
LDLW -4
STGW tProlog.goalframe
!   IF retry # NULL THEN choice := goalframe END
LDLW 16
JEQZ L592
LDGW tProlog.goalframe
STGW tProlog.choice
LABEL L592
RETURN
END

PROC tProlog.TroStep 28 5 0
! PROCEDURE TroStep();
!   IF dflag THEN Out.String("(TRO)"); Out.Ln() END;
LDGC tProlog.dflag
JEQZ L595
CONST 6
GLOBAL tProlog.%53
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
LABEL L595
!   oldsize := (FRSIZE + (mem[goalframe+6])*TERMSIZE); (* size of old frame *)
GLOBAL tProlog.mem
LDGW tProlog.goalframe
CONST 6
PLUS
CONST 25001
BOUND 1100
LDIW
CONST 2
TIMES
CONST 7
PLUS
STLW -8
!   newsize := (FRSIZE + (mem[prok])*TERMSIZE); (* size of new frame *)
GLOBAL tProlog.mem
LDGW tProlog.prok
CONST 25001
BOUND 1101
LDIW
CONST 2
TIMES
CONST 7
PLUS
STLW -12
!   temp := LocAlloc(newsize);
LDLW -12
GLOBAL tProlog.LocAlloc
CALLW 1
STLW -4
!   temp := goalframe + newsize; (* copy old frame here *)
LDGW tProlog.goalframe
LDLW -12
PLUS
STLW -4
!   FOR i := 1 TO oldsize DO 
LDLW -8
STLW -20
CONST 1
STLW -16
LABEL L596
LDLW -16
LDLW -20
JGT L597
!     mem[temp+oldsize-i] := mem[goalframe+oldsize-i]
GLOBAL tProlog.mem
LDGW tProlog.goalframe
LDLW -8
PLUS
LDLW -16
MINUS
CONST 25001
BOUND 1107
LDIW
GLOBAL tProlog.mem
LDLW -4
LDLW -8
PLUS
LDLW -16
MINUS
CONST 25001
BOUND 1107
STIW
!   FOR i := 1 TO oldsize DO 
INCL -16
JUMP L596
LABEL L597
!   FOR i := 1 TO mem[goalframe+6] DO
GLOBAL tProlog.mem
LDGW tProlog.goalframe
CONST 6
PLUS
CONST 25001
BOUND 1111
LDIW
STLW -24
CONST 1
STLW -16
LABEL L598
LDLW -16
LDLW -24
JGT L599
!     IF (LSR(mem[(temp+7+(i-1)*TERMSIZE)], 8) = CELL)
GLOBAL tProlog.mem
LDLW -4
CONST 7
PLUS
LDLW -16
DEC
CONST 2
TIMES
PLUS
CONST 25001
BOUND 1112
LDIW
CONST 8
LSR
CONST 4
JNEQ L602
GLOBAL tProlog.mem
LDLW -4
CONST 7
PLUS
LDLW -16
DEC
CONST 2
TIMES
PLUS
INC
CONST 25001
BOUND 1113
LDIW
JEQZ L602
LDGW tProlog.goalframe
GLOBAL tProlog.mem
LDLW -4
CONST 7
PLUS
LDLW -16
DEC
CONST 2
TIMES
PLUS
INC
CONST 25001
BOUND 1114
LDIW
JGT L602
GLOBAL tProlog.mem
LDLW -4
CONST 7
PLUS
LDLW -16
DEC
CONST 2
TIMES
PLUS
INC
CONST 25001
BOUND 1115
LDIW
LDGW tProlog.goalframe
LDLW -8
PLUS
JGEQ L602
!       mem[(temp+7+(i-1)*TERMSIZE)+1] := mem[(temp+7+(i-1)*TERMSIZE)+1] + newsize
GLOBAL tProlog.mem
LDLW -4
CONST 7
PLUS
LDLW -16
DEC
CONST 2
TIMES
PLUS
INC
CONST 25001
BOUND 1116
LDIW
LDLW -12
PLUS
GLOBAL tProlog.mem
LDLW -4
CONST 7
PLUS
LDLW -16
DEC
CONST 2
TIMES
PLUS
INC
CONST 25001
BOUND 1116
STIW
LABEL L602
!   FOR i := 1 TO mem[goalframe+6] DO
INCL -16
JUMP L598
LABEL L599
!   mem[goalframe+6] := mem[prok];
GLOBAL tProlog.mem
LDGW tProlog.prok
CONST 25001
BOUND 1121
LDIW
GLOBAL tProlog.mem
LDGW tProlog.goalframe
CONST 6
PLUS
CONST 25001
BOUND 1121
STIW
!   FOR i := 1 TO mem[goalframe+6] DO
GLOBAL tProlog.mem
LDGW tProlog.goalframe
CONST 6
PLUS
CONST 25001
BOUND 1122
LDIW
STLW -28
CONST 1
STLW -16
LABEL L606
LDLW -16
LDLW -28
JGT L607
!     mem[(goalframe+7+(i-1)*TERMSIZE)] := LSL(CELL, 8) + TERMSIZE;
CONST 1026
GLOBAL tProlog.mem
LDGW tProlog.goalframe
CONST 7
PLUS
LDLW -16
DEC
CONST 2
TIMES
PLUS
CONST 25001
BOUND 1123
STIW
!     mem[(goalframe+7+(i-1)*TERMSIZE)+1] := NULL
CONST 0
GLOBAL tProlog.mem
LDGW tProlog.goalframe
CONST 7
PLUS
LDLW -16
DEC
CONST 2
TIMES
PLUS
INC
CONST 25001
BOUND 1124
STIW
!   FOR i := 1 TO mem[goalframe+6] DO
INCL -16
JUMP L606
LABEL L607
!   ok := Unify(call, temp, mem[prok+3], goalframe);
LDGW tProlog.goalframe
GLOBAL tProlog.mem
LDGW tProlog.prok
CONST 3
PLUS
CONST 25001
BOUND 1128
LDIW
LDLW -4
LDGW tProlog.call
GLOBAL tProlog.Unify
CALLW 4
STGC tProlog.ok
!   current := (prok+4);
LDGW tProlog.prok
CONST 4
PLUS
STGW tProlog.current
!   lsp := temp-1
LDLW -4
DEC
STGW tProlog.lsp
RETURN
END

PROC tProlog.Step 4 5 0
! PROCEDURE Step();
!   IF symtab[mem[call+1]].action # 0 THEN
GLOBAL tProlog.symtab
GLOBAL tProlog.mem
LDGW tProlog.call
INC
CONST 25001
BOUND 1137
LDIW
CONST 512
BOUND 1137
CONST 16
TIMES
OFFSET
LDNW 8
JEQZ L616
!     ok := DoBuiltin(symtab[mem[call+1]].action)
GLOBAL tProlog.symtab
GLOBAL tProlog.mem
LDGW tProlog.call
INC
CONST 25001
BOUND 1138
LDIW
CONST 512
BOUND 1138
CONST 16
TIMES
OFFSET
LDNW 8
GLOBAL tProlog.DoBuiltin
CALLW 1
STGC tProlog.ok
RETURN
LABEL L616
!   ELSIF prok = NULL THEN
LDGW tProlog.prok
JNEQZ L618
!     ok := FALSE
CONST 0
STGC tProlog.ok
RETURN
LABEL L618
!     retry := Search(call, goalframe, mem[prok+2]);
GLOBAL tProlog.mem
LDGW tProlog.prok
CONST 2
PLUS
CONST 25001
BOUND 1142
LDIW
LDGW tProlog.goalframe
LDGW tProlog.call
GLOBAL tProlog.Search
CALLW 3
STLW -4
!     IF (mem[(current)+1] = NULL) & (choice < goalframe)
GLOBAL tProlog.mem
LDGW tProlog.current
INC
CONST 25001
BOUND 1143
LDIW
JNEQZ L611
LDGW tProlog.choice
LDGW tProlog.goalframe
JGEQ L611
LDLW -4
JNEQZ L611
LDGW tProlog.goalframe
LDGW tProlog.base
JEQ L611
!       TroStep()
GLOBAL tProlog.TroStep
CALL 0
RETURN
LABEL L611
!       PushFrame(mem[prok], retry);
LDLW -4
GLOBAL tProlog.mem
LDGW tProlog.prok
CONST 25001
BOUND 1147
LDIW
GLOBAL tProlog.PushFrame
CALL 2
!       ok := Unify(call, mem[goalframe+1], mem[prok+3], goalframe);
LDGW tProlog.goalframe
GLOBAL tProlog.mem
LDGW tProlog.prok
CONST 3
PLUS
CONST 25001
BOUND 1148
LDIW
GLOBAL tProlog.mem
LDGW tProlog.goalframe
INC
CONST 25001
BOUND 1148
LDIW
LDGW tProlog.call
GLOBAL tProlog.Unify
CALLW 4
STGC tProlog.ok
!       current := (prok+4);
LDGW tProlog.prok
CONST 4
PLUS
STGW tProlog.current
RETURN
END

PROC tProlog.Unwind 0 6 0
! PROCEDURE Unwind();
LABEL L619
!   WHILE (mem[current] = NULL) & (goalframe # base) DO
GLOBAL tProlog.mem
LDGW tProlog.current
CONST 25001
BOUND 1157
LDIW
JNEQZ L621
LDGW tProlog.goalframe
LDGW tProlog.base
JEQ L621
!     IF dflag THEN 
LDGC tProlog.dflag
JEQZ L624
!     Out.String("Exit"); Out.String(": "); 
CONST 5
GLOBAL tProlog.%54
GLOBAL Out.String
CALL 2
CONST 3
GLOBAL tProlog.%55
GLOBAL Out.String
CALL 2
!     PrintTerm(mem[mem[goalframe]], mem[goalframe+1], MAXPRIO); Out.Ln()
CONST 2
GLOBAL tProlog.mem
LDGW tProlog.goalframe
INC
CONST 25001
BOUND 1160
LDIW
GLOBAL tProlog.mem
GLOBAL tProlog.mem
LDGW tProlog.goalframe
CONST 25001
BOUND 1160
LDIW
CONST 25001
BOUND 1160
LDIW
GLOBAL tProlog.PrintTerm
CALL 3
GLOBAL Out.Ln
CALL 0
LABEL L624
!     current := (mem[goalframe])+1;
GLOBAL tProlog.mem
LDGW tProlog.goalframe
CONST 25001
BOUND 1162
LDIW
INC
STGW tProlog.current
!     IF goalframe > choice THEN lsp := goalframe-1 END;
LDGW tProlog.goalframe
LDGW tProlog.choice
JLEQ L627
LDGW tProlog.goalframe
DEC
STGW tProlog.lsp
LABEL L627
!     goalframe := mem[goalframe+1]
GLOBAL tProlog.mem
LDGW tProlog.goalframe
INC
CONST 25001
BOUND 1164
LDIW
STGW tProlog.goalframe
JUMP L619
LABEL L621
RETURN
END

PROC tProlog.Backtrack 0 4 0
! PROCEDURE Backtrack();
!   Restore();
GLOBAL tProlog.Restore
CALL 0
!   current := mem[choice]; goalframe := mem[choice+1];
GLOBAL tProlog.mem
LDGW tProlog.choice
CONST 25001
BOUND 1172
LDIW
STGW tProlog.current
GLOBAL tProlog.mem
LDGW tProlog.choice
INC
CONST 25001
BOUND 1172
LDIW
STGW tProlog.goalframe
!   call := Deref(mem[current], goalframe);
LDGW tProlog.goalframe
GLOBAL tProlog.mem
LDGW tProlog.current
CONST 25001
BOUND 1173
LDIW
GLOBAL tProlog.Deref
CALLW 2
STGW tProlog.call
!   prok := mem[choice+2]; gsp := mem[choice+4];
GLOBAL tProlog.mem
LDGW tProlog.choice
CONST 2
PLUS
CONST 25001
BOUND 1174
LDIW
STGW tProlog.prok
GLOBAL tProlog.mem
LDGW tProlog.choice
CONST 4
PLUS
CONST 25001
BOUND 1174
LDIW
STGW tProlog.gsp
!   lsp := choice-1; choice := mem[choice+3];
LDGW tProlog.choice
DEC
STGW tProlog.lsp
GLOBAL tProlog.mem
LDGW tProlog.choice
CONST 3
PLUS
CONST 25001
BOUND 1175
LDIW
STGW tProlog.choice
!   IF dflag THEN 
LDGC tProlog.dflag
JEQZ L631
!     Out.String("Redo"); Out.String(": "); 
CONST 5
GLOBAL tProlog.%56
GLOBAL Out.String
CALL 2
CONST 3
GLOBAL tProlog.%55
GLOBAL Out.String
CALL 2
!     PrintTerm(call, goalframe, MAXPRIO); Out.Ln()
CONST 2
LDGW tProlog.goalframe
LDGW tProlog.call
GLOBAL tProlog.PrintTerm
CALL 3
GLOBAL Out.Ln
CALL 0
LABEL L631
RETURN
END

PROC tProlog.Resume 0 4 0
! PROCEDURE Resume(flag: BOOLEAN);
!   ok := flag;
LDLC 12
STGC tProlog.ok
LABEL L632
!   WHILE run DO
LDGC tProlog.run
JEQZ L634
!     IF ok THEN
LDGC tProlog.ok
JEQZ L640
!       IF mem[current] = NULL THEN RETURN END;
GLOBAL tProlog.mem
LDGW tProlog.current
CONST 25001
BOUND 1188
LDIW
JNEQZ L643
RETURN
LABEL L643
!       call := Deref(mem[current], goalframe);
LDGW tProlog.goalframe
GLOBAL tProlog.mem
LDGW tProlog.current
CONST 25001
BOUND 1189
LDIW
GLOBAL tProlog.Deref
CALLW 2
STGW tProlog.call
!       IF dflag THEN 
LDGC tProlog.dflag
JEQZ L646
!     Out.String("Call"); Out.String(": "); 
CONST 5
GLOBAL tProlog.%57
GLOBAL Out.String
CALL 2
CONST 3
GLOBAL tProlog.%55
GLOBAL Out.String
CALL 2
!     PrintTerm(call, goalframe, MAXPRIO); Out.Ln()
CONST 2
LDGW tProlog.goalframe
LDGW tProlog.call
GLOBAL tProlog.PrintTerm
CALL 3
GLOBAL Out.Ln
CALL 0
LABEL L646
!       IF (symtab[mem[call+1]].prok = NULL)
GLOBAL tProlog.symtab
GLOBAL tProlog.mem
LDGW tProlog.call
INC
CONST 25001
BOUND 1194
LDIW
CONST 512
BOUND 1194
CONST 16
TIMES
OFFSET
LDNW 12
JNEQZ L649
GLOBAL tProlog.symtab
GLOBAL tProlog.mem
LDGW tProlog.call
INC
CONST 25001
BOUND 1195
LDIW
CONST 512
BOUND 1195
CONST 16
TIMES
OFFSET
LDNW 8
JNEQZ L649
! 	Out.Ln(); Out.String("Error: "); Out.String("call to undefined relation "); run := FALSE;
GLOBAL Out.Ln
CALL 0
CONST 8
GLOBAL tProlog.%18
GLOBAL Out.String
CALL 2
CONST 28
GLOBAL tProlog.%58
GLOBAL Out.String
CALL 2
CONST 0
STGC tProlog.run
! 	WriteString(symtab[mem[call+1]].name);
GLOBAL tProlog.symtab
GLOBAL tProlog.mem
LDGW tProlog.call
INC
CONST 25001
BOUND 1197
LDIW
CONST 512
BOUND 1197
CONST 4
TIMES
LDIW
GLOBAL tProlog.WriteString
CALL 1
! 	RETURN
RETURN
LABEL L649
!       prok := Search(call, goalframe, symtab[mem[call+1]].prok)
GLOBAL tProlog.symtab
GLOBAL tProlog.mem
LDGW tProlog.call
INC
CONST 25001
BOUND 1200
LDIW
CONST 512
BOUND 1200
CONST 16
TIMES
OFFSET
LDNW 12
LDGW tProlog.goalframe
LDGW tProlog.call
GLOBAL tProlog.Search
CALLW 3
STGW tProlog.prok
JUMP L635
LABEL L640
!       IF choice <= base THEN RETURN END;
LDGW tProlog.choice
LDGW tProlog.base
JGT L638
RETURN
LABEL L638
!       Backtrack()
GLOBAL tProlog.Backtrack
CALL 0
LABEL L635
!     Step();
GLOBAL tProlog.Step
CALL 0
!     IF ok THEN Unwind() END;
LDGC tProlog.ok
JEQZ L632
GLOBAL tProlog.Unwind
CALL 0
JUMP L632
LABEL L634
RETURN
END

PROC tProlog.Execute 4 4 0
! PROCEDURE Execute(g: clause);
!   lsp := hp; gsp := MEMSIZE+1; nsoln := 0;
LDGW tProlog.hp
STGW tProlog.lsp
CONST 25001
STGW tProlog.gsp
CONST 0
STLW -4
!   current := NULL; goalframe := NULL; choice := NULL; trhead := NULL;
CONST 0
STGW tProlog.current
CONST 0
STGW tProlog.goalframe
CONST 0
STGW tProlog.choice
CONST 0
STGW tProlog.trhead
!   PushFrame(mem[g], NULL);
CONST 0
GLOBAL tProlog.mem
LDLW 12
CONST 25001
BOUND 1216
LDIW
GLOBAL tProlog.PushFrame
CALL 2
!   choice := goalframe; base := goalframe; current := (g+4);
LDGW tProlog.goalframe
STGW tProlog.choice
LDGW tProlog.goalframe
STGW tProlog.base
LDLW 12
CONST 4
PLUS
STGW tProlog.current
!   run := TRUE;
CONST 1
STGC tProlog.run
!   Resume(TRUE);
CONST 1
ALIGNC
GLOBAL tProlog.Resume
CALL 1
!   IF ~run THEN RETURN END;
LDGC tProlog.run
JNEQZ L656
RETURN
LABEL L656
!   WHILE ok DO
LDGC tProlog.ok
JEQZ L659
!     nsoln := nsoln+1;
INCL -4
!     ShowAnswer(base);
LDGW tProlog.base
GLOBAL tProlog.ShowAnswer
CALL 1
!     Out.Ln();
GLOBAL Out.Ln
CALL 0
!     Resume(FALSE);
CONST 0
ALIGNC
GLOBAL tProlog.Resume
CALL 1
!     IF ~run THEN RETURN END;
LDGC tProlog.run
JNEQZ L656
RETURN
LABEL L659
!   IF nsoln = 0 THEN
LDLW -4
JNEQZ L665
!     Out.String("no"); Out.Ln(); Out.Ln();
CONST 3
GLOBAL tProlog.%59
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
GLOBAL Out.Ln
CALL 0
LABEL L665
RETURN
END

PROC tProlog.GetArgs 8 4 0
! PROCEDURE GetArgs();
!   FOR i := 1 TO symtab[mem[call+1]].arity DO
GLOBAL tProlog.symtab
GLOBAL tProlog.mem
LDGW tProlog.call
INC
CONST 25001
BOUND 1242
LDIW
CONST 512
BOUND 1242
CONST 16
TIMES
OFFSET
LDNW 4
STLW -8
CONST 1
STLW -4
LABEL L666
LDLW -4
LDLW -8
JGT L667
!     av[i] := Deref(mem[call+i+1], goalframe)
LDGW tProlog.goalframe
GLOBAL tProlog.mem
LDGW tProlog.call
LDLW -4
PLUS
INC
CONST 25001
BOUND 1243
LDIW
GLOBAL tProlog.Deref
CALLW 2
GLOBAL tProlog.av
LDLW -4
CONST 64
BOUND 1243
STIW
!   FOR i := 1 TO symtab[mem[call+1]].arity DO
INCL -4
JUMP L666
LABEL L667
RETURN
END

PROC tProlog.NewInt 4 4 0
! PROCEDURE NewInt(n: INTEGER): term;
!   t := GloAlloc(INT, TERMSIZE);
CONST 2
CONST 2
GLOBAL tProlog.GloAlloc
CALLW 2
STLW -4
!   mem[t+1] := n;
LDLW 12
GLOBAL tProlog.mem
LDLW -4
INC
CONST 25001
BOUND 1251
STIW
!   RETURN t
LDLW -4
RETURN
END

PROC tProlog.DoCut 0 4 0
! PROCEDURE DoCut(): BOOLEAN;
!   choice := mem[goalframe+3];
GLOBAL tProlog.mem
LDGW tProlog.goalframe
CONST 3
PLUS
CONST 25001
BOUND 1258
LDIW
STGW tProlog.choice
!   lsp := goalframe + (FRSIZE + (mem[goalframe+6])*TERMSIZE) - 1;
LDGW tProlog.goalframe
GLOBAL tProlog.mem
LDGW tProlog.goalframe
CONST 6
PLUS
CONST 25001
BOUND 1259
LDIW
CONST 2
TIMES
CONST 7
PLUS
PLUS
DEC
STGW tProlog.lsp
!   Commit();
GLOBAL tProlog.Commit
CALL 0
!   current := (current)+1;
LDGW tProlog.current
INC
STGW tProlog.current
!   RETURN TRUE
CONST 1
RETURN
END

PROC tProlog.DoCall 0 4 0
! PROCEDURE DoCall(): BOOLEAN;
!   GetArgs();
GLOBAL tProlog.GetArgs
CALL 0
!   IF ~(LSR(mem[av[1]], 8) = FUNC) THEN
GLOBAL tProlog.mem
GLOBAL tProlog.av
LDNW 4
CONST 25001
BOUND 1269
LDIW
CONST 8
LSR
CONST 1
JEQ L670
!     Out.Ln(); Out.String("Error: "); Out.String("bad argument to call/1"); run := FALSE;
GLOBAL Out.Ln
CALL 0
CONST 8
GLOBAL tProlog.%18
GLOBAL Out.String
CALL 2
CONST 23
GLOBAL tProlog.%60
GLOBAL Out.String
CALL 2
CONST 0
STGC tProlog.run
!     RETURN FALSE
CONST 0
RETURN
LABEL L670
!     PushFrame(1, NULL);
CONST 0
CONST 1
GLOBAL tProlog.PushFrame
CALL 2
!     mem[(goalframe+7+(1-1)*TERMSIZE)+1] :=
GLOBAL tProlog.mem
LDGW tProlog.goalframe
INC
CONST 25001
BOUND 1275
LDIW
GLOBAL tProlog.av
LDNW 4
GLOBAL tProlog.GloCopy
CALLW 2
GLOBAL tProlog.mem
LDGW tProlog.goalframe
CONST 8
PLUS
CONST 25001
BOUND 1274
STIW
!     current := callbody;
LDGW tProlog.callbody
STGW tProlog.current
!     RETURN TRUE
CONST 1
RETURN
END

PROC tProlog.DoNot 4 4 0
! PROCEDURE DoNot(): BOOLEAN;
!   GetArgs();
GLOBAL tProlog.GetArgs
CALL 0
!   IF ~(LSR(mem[av[1]], 8) = FUNC) THEN
GLOBAL tProlog.mem
GLOBAL tProlog.av
LDNW 4
CONST 25001
BOUND 1286
LDIW
CONST 8
LSR
CONST 1
JEQ L676
!     Out.Ln(); Out.String("Error: "); Out.String("bad argument to call/1"); run := FALSE;
GLOBAL Out.Ln
CALL 0
CONST 8
GLOBAL tProlog.%18
GLOBAL Out.String
CALL 2
CONST 23
GLOBAL tProlog.%60
GLOBAL Out.String
CALL 2
CONST 0
STGC tProlog.run
!     RETURN FALSE
CONST 0
RETURN
LABEL L676
!     PushFrame(1, NULL);
CONST 0
CONST 1
GLOBAL tProlog.PushFrame
CALL 2
!     savebase := base; base := goalframe; choice := goalframe;
LDGW tProlog.base
STLW -4
LDGW tProlog.goalframe
STGW tProlog.base
LDGW tProlog.goalframe
STGW tProlog.choice
!     mem[(goalframe+7+(1-1)*TERMSIZE)+1] :=
GLOBAL tProlog.mem
LDGW tProlog.goalframe
INC
CONST 25001
BOUND 1293
LDIW
GLOBAL tProlog.av
LDNW 4
GLOBAL tProlog.GloCopy
CALLW 2
GLOBAL tProlog.mem
LDGW tProlog.goalframe
CONST 8
PLUS
CONST 25001
BOUND 1292
STIW
!     current := callbody;
LDGW tProlog.callbody
STGW tProlog.current
!     Resume(TRUE);
CONST 1
ALIGNC
GLOBAL tProlog.Resume
CALL 1
!     choice := mem[base+3]; goalframe := mem[base+1];
GLOBAL tProlog.mem
LDGW tProlog.base
CONST 3
PLUS
CONST 25001
BOUND 1296
LDIW
STGW tProlog.choice
GLOBAL tProlog.mem
LDGW tProlog.base
INC
CONST 25001
BOUND 1296
LDIW
STGW tProlog.goalframe
!     IF ~ok THEN
LDGC tProlog.ok
JNEQZ L674
!       current := (mem[base])+1;
GLOBAL tProlog.mem
LDGW tProlog.base
CONST 25001
BOUND 1298
LDIW
INC
STGW tProlog.current
!       RETURN TRUE
CONST 1
RETURN
LABEL L674
!       Commit();
GLOBAL tProlog.Commit
CALL 0
!       RETURN FALSE
CONST 0
RETURN
END

PROC tProlog.DoPlus 4 6 0
! PROCEDURE DoPlus(): BOOLEAN;
!   GetArgs();
GLOBAL tProlog.GetArgs
CALL 0
!   result := FALSE;
CONST 0
STLC -1
!   IF (LSR(mem[av[1]], 8) = INT) & (LSR(mem[av[2]], 8) = INT) THEN
GLOBAL tProlog.mem
GLOBAL tProlog.av
LDNW 4
CONST 25001
BOUND 1314
LDIW
CONST 8
LSR
CONST 2
JNEQ L679
GLOBAL tProlog.mem
GLOBAL tProlog.av
LDNW 8
CONST 25001
BOUND 1314
LDIW
CONST 8
LSR
CONST 2
JNEQ L679
!     result := Unify(av[3], goalframe, NewInt(mem[av[1]+1] + mem[av[2]+1]), NULL)
CONST 0
GLOBAL tProlog.mem
GLOBAL tProlog.av
LDNW 4
INC
CONST 25001
BOUND 1315
LDIW
GLOBAL tProlog.mem
GLOBAL tProlog.av
LDNW 8
INC
CONST 25001
BOUND 1315
LDIW
PLUS
GLOBAL tProlog.NewInt
CALLW 1
LDGW tProlog.goalframe
GLOBAL tProlog.av
LDNW 12
GLOBAL tProlog.Unify
CALLW 4
STLC -1
JUMP L677
LABEL L679
!   ELSIF (LSR(mem[av[1]], 8) = INT) & (LSR(mem[av[3]], 8) = INT) THEN
GLOBAL tProlog.mem
GLOBAL tProlog.av
LDNW 4
CONST 25001
BOUND 1316
LDIW
CONST 8
LSR
CONST 2
JNEQ L682
GLOBAL tProlog.mem
GLOBAL tProlog.av
LDNW 12
CONST 25001
BOUND 1316
LDIW
CONST 8
LSR
CONST 2
JNEQ L682
!     IF mem[av[1]+1] <= mem[av[3]+1] THEN
GLOBAL tProlog.mem
GLOBAL tProlog.av
LDNW 4
INC
CONST 25001
BOUND 1317
LDIW
GLOBAL tProlog.mem
GLOBAL tProlog.av
LDNW 12
INC
CONST 25001
BOUND 1317
LDIW
JGT L677
!       result := Unify(av[2], goalframe, 
CONST 0
GLOBAL tProlog.mem
GLOBAL tProlog.av
LDNW 12
INC
CONST 25001
BOUND 1319
LDIW
GLOBAL tProlog.mem
GLOBAL tProlog.av
LDNW 4
INC
CONST 25001
BOUND 1319
LDIW
MINUS
GLOBAL tProlog.NewInt
CALLW 1
LDGW tProlog.goalframe
GLOBAL tProlog.av
LDNW 8
GLOBAL tProlog.Unify
CALLW 4
STLC -1
JUMP L677
LABEL L682
!   ELSIF (LSR(mem[av[2]], 8) = INT) & (LSR(mem[av[3]], 8) = INT) THEN
GLOBAL tProlog.mem
GLOBAL tProlog.av
LDNW 8
CONST 25001
BOUND 1321
LDIW
CONST 8
LSR
CONST 2
JNEQ L688
GLOBAL tProlog.mem
GLOBAL tProlog.av
LDNW 12
CONST 25001
BOUND 1321
LDIW
CONST 8
LSR
CONST 2
JNEQ L688
!     IF mem[av[2]+1] <= mem[av[3]+1] THEN
GLOBAL tProlog.mem
GLOBAL tProlog.av
LDNW 8
INC
CONST 25001
BOUND 1322
LDIW
GLOBAL tProlog.mem
GLOBAL tProlog.av
LDNW 12
INC
CONST 25001
BOUND 1322
LDIW
JGT L677
!       result := Unify(av[1], goalframe, NewInt(mem[av[3]+1] - mem[av[2]+1]), NULL)
CONST 0
GLOBAL tProlog.mem
GLOBAL tProlog.av
LDNW 12
INC
CONST 25001
BOUND 1323
LDIW
GLOBAL tProlog.mem
GLOBAL tProlog.av
LDNW 8
INC
CONST 25001
BOUND 1323
LDIW
MINUS
GLOBAL tProlog.NewInt
CALLW 1
LDGW tProlog.goalframe
GLOBAL tProlog.av
LDNW 4
GLOBAL tProlog.Unify
CALLW 4
STLC -1
JUMP L677
LABEL L688
!     Out.Ln(); Out.String("Error: "); Out.String("plus/3 needs at least two integers"); run := FALSE
GLOBAL Out.Ln
CALL 0
CONST 8
GLOBAL tProlog.%18
GLOBAL Out.String
CALL 2
CONST 35
GLOBAL tProlog.%61
GLOBAL Out.String
CALL 2
CONST 0
STGC tProlog.run
LABEL L677
!   current := (current)+1;
LDGW tProlog.current
INC
STGW tProlog.current
!   RETURN result
LDLC -1
RETURN
END

PROC tProlog.DoTimes 4 6 0
! PROCEDURE DoTimes(): BOOLEAN;
!   GetArgs();
GLOBAL tProlog.GetArgs
CALL 0
!   result := FALSE;
CONST 0
STLC -1
!   IF (LSR(mem[av[1]], 8) = INT) & (LSR(mem[av[2]], 8) = INT) THEN
GLOBAL tProlog.mem
GLOBAL tProlog.av
LDNW 4
CONST 25001
BOUND 1338
LDIW
CONST 8
LSR
CONST 2
JNEQ L695
GLOBAL tProlog.mem
GLOBAL tProlog.av
LDNW 8
CONST 25001
BOUND 1338
LDIW
CONST 8
LSR
CONST 2
JNEQ L695
!     result := Unify(av[3], goalframe, 
CONST 0
GLOBAL tProlog.mem
GLOBAL tProlog.av
LDNW 4
INC
CONST 25001
BOUND 1340
LDIW
GLOBAL tProlog.mem
GLOBAL tProlog.av
LDNW 8
INC
CONST 25001
BOUND 1340
LDIW
TIMES
GLOBAL tProlog.NewInt
CALLW 1
LDGW tProlog.goalframe
GLOBAL tProlog.av
LDNW 12
GLOBAL tProlog.Unify
CALLW 4
STLC -1
JUMP L693
LABEL L695
!   ELSIF (LSR(mem[av[1]], 8) = INT) & (LSR(mem[av[3]], 8) = INT) THEN
GLOBAL tProlog.mem
GLOBAL tProlog.av
LDNW 4
CONST 25001
BOUND 1341
LDIW
CONST 8
LSR
CONST 2
JNEQ L698
GLOBAL tProlog.mem
GLOBAL tProlog.av
LDNW 12
CONST 25001
BOUND 1341
LDIW
CONST 8
LSR
CONST 2
JNEQ L698
!     IF mem[av[1]+1] # 0 THEN
GLOBAL tProlog.mem
GLOBAL tProlog.av
LDNW 4
INC
CONST 25001
BOUND 1342
LDIW
JEQZ L693
!       IF mem[av[3]+1] MOD mem[av[1]+1] = 0 THEN
GLOBAL tProlog.mem
GLOBAL tProlog.av
LDNW 12
INC
CONST 25001
BOUND 1343
LDIW
GLOBAL tProlog.mem
GLOBAL tProlog.av
LDNW 4
INC
CONST 25001
BOUND 1343
LDIW
ZCHECK 1343
MOD
JNEQZ L693
!         result := Unify(av[2], goalframe, 
CONST 0
GLOBAL tProlog.mem
GLOBAL tProlog.av
LDNW 12
INC
CONST 25001
BOUND 1345
LDIW
GLOBAL tProlog.mem
GLOBAL tProlog.av
LDNW 4
INC
CONST 25001
BOUND 1345
LDIW
ZCHECK 1345
DIV
GLOBAL tProlog.NewInt
CALLW 1
LDGW tProlog.goalframe
GLOBAL tProlog.av
LDNW 8
GLOBAL tProlog.Unify
CALLW 4
STLC -1
JUMP L693
LABEL L698
!   ELSIF (LSR(mem[av[2]], 8) = INT) & (LSR(mem[av[3]], 8) = INT) THEN
GLOBAL tProlog.mem
GLOBAL tProlog.av
LDNW 8
CONST 25001
BOUND 1348
LDIW
CONST 8
LSR
CONST 2
JNEQ L707
GLOBAL tProlog.mem
GLOBAL tProlog.av
LDNW 12
CONST 25001
BOUND 1348
LDIW
CONST 8
LSR
CONST 2
JNEQ L707
!     IF mem[av[2]+1] # 0 THEN
GLOBAL tProlog.mem
GLOBAL tProlog.av
LDNW 8
INC
CONST 25001
BOUND 1349
LDIW
JEQZ L693
!       IF mem[av[3]+1] MOD mem[av[2]+1] = 0 THEN
GLOBAL tProlog.mem
GLOBAL tProlog.av
LDNW 12
INC
CONST 25001
BOUND 1350
LDIW
GLOBAL tProlog.mem
GLOBAL tProlog.av
LDNW 8
INC
CONST 25001
BOUND 1350
LDIW
ZCHECK 1350
MOD
JNEQZ L693
!         result := Unify(av[1], goalframe, 
CONST 0
GLOBAL tProlog.mem
GLOBAL tProlog.av
LDNW 12
INC
CONST 25001
BOUND 1352
LDIW
GLOBAL tProlog.mem
GLOBAL tProlog.av
LDNW 8
INC
CONST 25001
BOUND 1352
LDIW
ZCHECK 1352
DIV
GLOBAL tProlog.NewInt
CALLW 1
LDGW tProlog.goalframe
GLOBAL tProlog.av
LDNW 4
GLOBAL tProlog.Unify
CALLW 4
STLC -1
JUMP L693
LABEL L707
!     Out.Ln(); Out.String("Error: "); Out.String("times/3 needs at least two integers"); run := FALSE
GLOBAL Out.Ln
CALL 0
CONST 8
GLOBAL tProlog.%18
GLOBAL Out.String
CALL 2
CONST 36
GLOBAL tProlog.%62
GLOBAL Out.String
CALL 2
CONST 0
STGC tProlog.run
LABEL L693
!   current := (current)+1;
LDGW tProlog.current
INC
STGW tProlog.current
!   RETURN result
LDLC -1
RETURN
END

PROC tProlog.DoEqual 0 6 0
! PROCEDURE DoEqual(): BOOLEAN;
!   GetArgs();
GLOBAL tProlog.GetArgs
CALL 0
!   current := (current)+1;
LDGW tProlog.current
INC
STGW tProlog.current
!   RETURN Unify(av[1], goalframe, av[2], goalframe)
LDGW tProlog.goalframe
GLOBAL tProlog.av
LDNW 8
LDGW tProlog.goalframe
GLOBAL tProlog.av
LDNW 4
GLOBAL tProlog.Unify
CALLW 4
RETURN
END

PROC tProlog.DoInteger 0 4 0
! PROCEDURE DoInteger(): BOOLEAN;
!   GetArgs();
GLOBAL tProlog.GetArgs
CALL 0
!   current := (current)+1;
LDGW tProlog.current
INC
STGW tProlog.current
!   RETURN (LSR(mem[av[1]], 8) = INT)
GLOBAL tProlog.mem
GLOBAL tProlog.av
LDNW 4
CONST 25001
BOUND 1375
LDIW
CONST 8
LSR
CONST 2
EQ
RETURN
END

PROC tProlog.DoChar 0 4 0
! PROCEDURE DoChar(): BOOLEAN;
!   GetArgs();
GLOBAL tProlog.GetArgs
CALL 0
!   current := (current)+1;
LDGW tProlog.current
INC
STGW tProlog.current
!   RETURN (LSR(mem[av[1]], 8) = CHRCTR)
GLOBAL tProlog.mem
GLOBAL tProlog.av
LDNW 4
CONST 25001
BOUND 1383
LDIW
CONST 8
LSR
CONST 3
EQ
RETURN
END

PROC tProlog.DoPrint 0 5 0
! PROCEDURE DoPrint(): BOOLEAN;
!   GetArgs();
GLOBAL tProlog.GetArgs
CALL 0
!   PrintTerm(av[1], goalframe, MAXPRIO);
CONST 2
LDGW tProlog.goalframe
GLOBAL tProlog.av
LDNW 4
GLOBAL tProlog.PrintTerm
CALL 3
!   current := (current)+1;
LDGW tProlog.current
INC
STGW tProlog.current
!   RETURN TRUE
CONST 1
RETURN
END

PROC tProlog.DoNl 0 2 0
! PROCEDURE DoNl(): BOOLEAN;
!   Out.Ln();
GLOBAL Out.Ln
CALL 0
!   current := (current)+1;
LDGW tProlog.current
INC
STGW tProlog.current
!   RETURN TRUE
CONST 1
RETURN
END

PROC tProlog.DoBuiltin 0 3 0
! PROCEDURE DoBuiltin(action: INTEGER): BOOLEAN;
!   CASE action OF
LDLW 12
DEC
JCASE 11
CASEL L717
CASEL L718
CASEL L719
CASEL L720
CASEL L721
CASEL L722
CASEL L723
CASEL L724
CASEL L725
CASEL L726
CASEL L727
JUMP L715
LABEL L717
!     CUT:      RETURN DoCut()
GLOBAL tProlog.DoCut
CALLW 0
RETURN
LABEL L718
!   | CALL:     RETURN DoCall()
GLOBAL tProlog.DoCall
CALLW 0
RETURN
LABEL L719
!   | PLUS:     RETURN DoPlus()
GLOBAL tProlog.DoPlus
CALLW 0
RETURN
LABEL L720
!   | TIMES:    RETURN DoTimes()
GLOBAL tProlog.DoTimes
CALLW 0
RETURN
LABEL L721
!   | ISINT:    RETURN DoInteger()
GLOBAL tProlog.DoInteger
CALLW 0
RETURN
LABEL L722
!   | ISCHAR:   RETURN DoChar()
GLOBAL tProlog.DoChar
CALLW 0
RETURN
LABEL L723
!   | NAFF:     RETURN DoNot()
GLOBAL tProlog.DoNot
CALLW 0
RETURN
LABEL L724
!   | EQUALITY: RETURN DoEqual()
GLOBAL tProlog.DoEqual
CALLW 0
RETURN
LABEL L725
!   | FAIL:     RETURN FALSE
CONST 0
RETURN
LABEL L726
!   | PRINT:    RETURN DoPrint()
GLOBAL tProlog.DoPrint
CALLW 0
RETURN
LABEL L727
!   | NL:	      RETURN DoNl()
GLOBAL tProlog.DoNl
CALLW 0
RETURN
LABEL L715
!     Out.Ln(); Out.String("Panic: "); Out.String("bad tag" (*action:1, " in ", "DoBuiltin"*)); Out.Ln(); HALT(2)
GLOBAL Out.Ln
CALL 0
CONST 8
GLOBAL tProlog.%1
GLOBAL Out.String
CALL 2
CONST 8
GLOBAL tProlog.%51
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
CONST 2
GLOBAL HALT
CALL 1
ERROR E_RETURN 1404
END

PROC tProlog.Initialize 8 4 0
! PROCEDURE Initialize();
!   dflag := FALSE; errcount := 0;
CONST 0
STGC tProlog.dflag
CONST 0
STGW tProlog.errcount
!   pbchar := ENDFILE; charptr := 0;
CONST 127
STGC tProlog.pbchar
CONST 0
STGW tProlog.charptr
!   hp := 0; InitSymbols();
CONST 0
STGW tProlog.hp
GLOBAL tProlog.InitSymbols
CALL 0
!   FOR i := 1 TO MAXARITY DO
CONST 1
STLW -4
LABEL L728
LDLW -4
CONST 63
JGT L729
!     p := HeapAlloc(TERMSIZE);
CONST 2
GLOBAL tProlog.HeapAlloc
CALLW 1
STLW -8
!     mem[p] := LSL(REF, 8) + TERMSIZE;
CONST 1282
GLOBAL tProlog.mem
LDLW -8
CONST 25001
BOUND 1434
STIW
!     mem[p+1] := i; refnode[i] := p
LDLW -4
GLOBAL tProlog.mem
LDLW -8
INC
CONST 25001
BOUND 1435
STIW
LDLW -8
GLOBAL tProlog.refnode
LDLW -4
CONST 64
BOUND 1435
STIW
!   FOR i := 1 TO MAXARITY DO
INCL -4
JUMP L728
LABEL L729
!   callbody := HeapAlloc(2);
CONST 2
GLOBAL tProlog.HeapAlloc
CALLW 1
STGW tProlog.callbody
!   mem[callbody] := MakeRef(1);
CONST 1
GLOBAL tProlog.MakeRef
CALLW 1
GLOBAL tProlog.mem
LDGW tProlog.callbody
CONST 25001
BOUND 1440
STIW
!   mem[(callbody)+1] := NULL
CONST 0
GLOBAL tProlog.mem
LDGW tProlog.callbody
INC
CONST 25001
BOUND 1441
STIW
RETURN
END

PROC tProlog.ReadFile 4 3 0
! PROCEDURE ReadFile();
!   lineno := 1;
CONST 1
STGW tProlog.lineno
LABEL L730
!     hmark := hp;
LDGW tProlog.hp
STGW tProlog.hmark
!     c := ReadClause();
GLOBAL tProlog.ReadClause
CALLW 0
STLW -4
!     IF c # NULL THEN
LDLW -4
JEQZ L734
!       IF dflag THEN PrintClause(c) END;	
LDGC tProlog.dflag
JEQZ L737
LDLW -4
GLOBAL tProlog.PrintClause
CALL 1
LABEL L737
!       IF mem[c+3] # NULL THEN
GLOBAL tProlog.mem
LDLW -4
CONST 3
PLUS
CONST 25001
BOUND 1454
LDIW
JEQZ L740
!         AddClause(c)
LDLW -4
GLOBAL tProlog.AddClause
CALL 1
JUMP L734
LABEL L740
!         Execute(c);
LDLW -4
GLOBAL tProlog.Execute
CALL 1
! 	hp := hmark
LDGW tProlog.hmark
STGW tProlog.hp
LABEL L734
!   UNTIL c = NULL
LDLW -4
JNEQZ L730
RETURN
END

PROC tProlog.%main 0 3 0
!   prog('subject(');
CONST 9
GLOBAL tProlog.%63
GLOBAL tProlog.prog
CALL 2
!   prog('  <store,');
CONST 10
GLOBAL tProlog.%64
GLOBAL tProlog.prog
CALL 2
!   prog('    <load,');
CONST 11
GLOBAL tProlog.%65
GLOBAL tProlog.prog
CALL 2
!   prog('      <plusa,');
CONST 14
GLOBAL tProlog.%66
GLOBAL tProlog.prog
CALL 2
!   prog('        <global(a)>,');
CONST 21
GLOBAL tProlog.%67
GLOBAL tProlog.prog
CALL 2
!   prog('        <lsl, <load, <local(16)>>, <const(2)>>>>,');
CONST 50
GLOBAL tProlog.%68
GLOBAL tProlog.prog
CALL 2
!   prog('    <local(20)>>');
CONST 17
GLOBAL tProlog.%69
GLOBAL tProlog.prog
CALL 2
!   prog(') :- .');
CONST 7
GLOBAL tProlog.%70
GLOBAL tProlog.prog
CALL 2
!   prog('rule("*str", stmt, <store, reg, addr>) :- .');
CONST 44
GLOBAL tProlog.%71
GLOBAL tProlog.prog
CALL 2
!   prog('rule("*ldr", reg,  <load, addr>) :- .');
CONST 38
GLOBAL tProlog.%72
GLOBAL tProlog.prog
CALL 2
!   prog('rule("*addfp", reg, <local(N)>) :- .');
CONST 37
GLOBAL tProlog.%73
GLOBAL tProlog.prog
CALL 2
!   prog('rule("local", addr, <local(N)>) :- .');
CONST 37
GLOBAL tProlog.%74
GLOBAL tProlog.prog
CALL 2
!   prog('rule("*add", reg, <plusa, reg, rand>) :- .');
CONST 43
GLOBAL tProlog.%75
GLOBAL tProlog.prog
CALL 2
!   prog('rule("index", addr, <plusa, reg, reg>) :- .');
CONST 44
GLOBAL tProlog.%76
GLOBAL tProlog.prog
CALL 2
!   prog('rule("scale", addr,');
CONST 20
GLOBAL tProlog.%77
GLOBAL tProlog.prog
CALL 2
!   prog('       <plusa, reg, <lsl, reg, <const(N)>>>) :- .');
CONST 50
GLOBAL tProlog.%78
GLOBAL tProlog.prog
CALL 2
!   prog('rule("*global", reg, <global(X)>) :- .');
CONST 39
GLOBAL tProlog.%79
GLOBAL tProlog.prog
CALL 2
!   prog('rule("*lsl", reg, <lsl, reg, rand>) :- .');
CONST 41
GLOBAL tProlog.%80
GLOBAL tProlog.prog
CALL 2
!   prog('rule("lshiftc", rand, <lsl, reg, <const(N)>>) :- .');
CONST 51
GLOBAL tProlog.%81
GLOBAL tProlog.prog
CALL 2
!   prog('rule("lshiftr", rand, <lsl, reg, reg>) :- .');
CONST 44
GLOBAL tProlog.%82
GLOBAL tProlog.prog
CALL 2
!   prog('rule("*mov", reg, <const(N)>) :- .');
CONST 35
GLOBAL tProlog.%83
GLOBAL tProlog.prog
CALL 2
!   prog('rule("const", rand, <const(N)>) :- .');
CONST 37
GLOBAL tProlog.%84
GLOBAL tProlog.prog
CALL 2
!   prog('rule("reg", rand, reg) :- .');
CONST 28
GLOBAL tProlog.%85
GLOBAL tProlog.prog
CALL 2
!   prog('rule("indir", addr, reg) :- .');
CONST 30
GLOBAL tProlog.%86
GLOBAL tProlog.prog
CALL 2
!   prog('use_rule(NT, Tree, node(Name, Kids)) :-');
CONST 40
GLOBAL tProlog.%87
GLOBAL tProlog.prog
CALL 2
!   prog('  rule(Name, NT, RHS), match(RHS, Tree, Kids, nil).');
CONST 52
GLOBAL tProlog.%88
GLOBAL tProlog.prog
CALL 2
!   prog('match(NT, Tree, Parse:Kids0, Kids0) :-');
CONST 39
GLOBAL tProlog.%89
GLOBAL tProlog.prog
CALL 2
!   prog('  use_rule(NT, Tree, Parse).');
CONST 29
GLOBAL tProlog.%90
GLOBAL tProlog.prog
CALL 2
!   prog('match(node(W, PS), node(W, TS), Kids, Kids0) :-');
CONST 48
GLOBAL tProlog.%91
GLOBAL tProlog.prog
CALL 2
!   prog('  matchall(PS, TS, Kids, Kids0).');
CONST 33
GLOBAL tProlog.%92
GLOBAL tProlog.prog
CALL 2
!   prog('matchall(nil, nil, Kids0, Kids0) :- .');
CONST 38
GLOBAL tProlog.%93
GLOBAL tProlog.prog
CALL 2
!   prog('matchall(P:PS, T:TS, Kids, Kids0) :-');
CONST 37
GLOBAL tProlog.%94
GLOBAL tProlog.prog
CALL 2
!   prog('  match(P, T, Kids, Kids1), matchall(PS, TS, Kids1, Kids0).');
CONST 60
GLOBAL tProlog.%95
GLOBAL tProlog.prog
CALL 2
!   prog('cost(node(X, TS), C) :-');
CONST 24
GLOBAL tProlog.%96
GLOBAL tProlog.prog
CALL 2
!   prog('  opcost(X, A), allcosts(TS, B), plus(A, B, C).');
CONST 48
GLOBAL tProlog.%97
GLOBAL tProlog.prog
CALL 2
!   prog('allcosts(nil, 0) :- .');
CONST 22
GLOBAL tProlog.%98
GLOBAL tProlog.prog
CALL 2
!   prog('allcosts(T:TS, C) :-');
CONST 21
GLOBAL tProlog.%99
GLOBAL tProlog.prog
CALL 2
!   prog('  cost(T, A), allcosts(TS, B), plus(A, B, C).');
CONST 46
GLOBAL tProlog.%100
GLOBAL tProlog.prog
CALL 2
!   prog("opcost('*':_, 1) :- !.");
CONST 23
GLOBAL tProlog.%101
GLOBAL tProlog.prog
CALL 2
!   prog('opcost(_, 0) :- .');
CONST 18
GLOBAL tProlog.%102
GLOBAL tProlog.prog
CALL 2
!   prog('answer(P, C) :-');
CONST 16
GLOBAL tProlog.%103
GLOBAL tProlog.prog
CALL 2
!   prog('  subject(T), use_rule(stmt, T, P), cost(P, C).');
CONST 48
GLOBAL tProlog.%104
GLOBAL tProlog.prog
CALL 2
!   prog('min(N, P) :- min1(N, 0, P).');
CONST 28
GLOBAL tProlog.%105
GLOBAL tProlog.prog
CALL 2
!   prog('min1(N, N, P) :- call(P), !.');
CONST 29
GLOBAL tProlog.%106
GLOBAL tProlog.prog
CALL 2
!   prog('min1(N, N0, P) :- plus(N0, 1, N1), min1(N, N1, P).');
CONST 51
GLOBAL tProlog.%107
GLOBAL tProlog.prog
CALL 2
!   prog('# :- answer(P, C).');
CONST 19
GLOBAL tProlog.%108
GLOBAL tProlog.prog
CALL 2
!   Initialize();
GLOBAL tProlog.Initialize
CALL 0
!   ReadFile()
GLOBAL tProlog.ReadFile
CALL 0
RETURN
END

! Global variables
GLOVAR tProlog.run 1
GLOVAR tProlog.dflag 1
GLOVAR tProlog.charptr 4
GLOVAR tProlog.charbuf 2048
GLOVAR tProlog.lsp 4
GLOVAR tProlog.gsp 4
GLOVAR tProlog.hp 4
GLOVAR tProlog.hmark 4
GLOVAR tProlog.mem 100004
GLOVAR tProlog.infile 3000
GLOVAR tProlog.pin 4
GLOVAR tProlog.pout 4
GLOVAR tProlog.pbchar 1
GLOVAR tProlog.lineno 4
GLOVAR tProlog.current 4
GLOVAR tProlog.call 4
GLOVAR tProlog.goalframe 4
GLOVAR tProlog.choice 4
GLOVAR tProlog.base 4
GLOVAR tProlog.prok 4
GLOVAR tProlog.nsymbols 4
GLOVAR tProlog.symtab 8192
GLOVAR tProlog.cons 4
GLOVAR tProlog.eqsym 4
GLOVAR tProlog.cutsym 4
GLOVAR tProlog.nilsym 4
GLOVAR tProlog.notsym 4
GLOVAR tProlog.node 4
GLOVAR tProlog.refnode 256
GLOVAR tProlog.token 4
GLOVAR tProlog.tokval 4
GLOVAR tProlog.tokival 4
GLOVAR tProlog.toksval 128
GLOVAR tProlog.errflag 1
GLOVAR tProlog.errcount 4
GLOVAR tProlog.nvars 4
GLOVAR tProlog.vartable 256
GLOVAR tProlog.trhead 4
GLOVAR tProlog.ok 1
GLOVAR tProlog.av 256
GLOVAR tProlog.callbody 4

! String "Panic: "
DEFINE tProlog.%1
STRING 50616E69633A2000

! String "out of string space"
DEFINE tProlog.%2
STRING 6F7574206F6620737472696E6720737061636500

! String "out of stack space"
DEFINE tProlog.%3
STRING 6F7574206F6620737461636B20737061636500

! String "out of heap space"
DEFINE tProlog.%4
STRING 6F7574206F66206865617020737061636500

! String "Deref"
DEFINE tProlog.%5
STRING 446572656600

! String "out of symbol space"
DEFINE tProlog.%6
STRING 6F7574206F662073796D626F6C20737061636500

! String "nil"
DEFINE tProlog.%7
STRING 6E696C00

! String "not"
DEFINE tProlog.%8
STRING 6E6F7400

! String "node"
DEFINE tProlog.%9
STRING 6E6F646500

! String "call"
DEFINE tProlog.%10
STRING 63616C6C00

! String "plus"
DEFINE tProlog.%11
STRING 706C757300

! String "times"
DEFINE tProlog.%12
STRING 74696D657300

! String "integer"
DEFINE tProlog.%13
STRING 696E746567657200

! String "char"
DEFINE tProlog.%14
STRING 6368617200

! String "false"
DEFINE tProlog.%15
STRING 66616C736500

! String "print"
DEFINE tProlog.%16
STRING 7072696E7400

! String "nl"
DEFINE tProlog.%17
STRING 6E6C00

! String "Error: "
DEFINE tProlog.%18
STRING 4572726F723A2000

! String "cannot add clauses to built-in relation "
DEFINE tProlog.%19
STRING 63616E6E6F742061646420636C617573657320746F206275696C742D696E2072
STRING 656C6174696F6E2000

! String " = "
DEFINE tProlog.%20
STRING 203D2000

! String "not "
DEFINE tProlog.%21
STRING 6E6F742000

! String ", "
DEFINE tProlog.%22
STRING 2C2000

! String "*null-term*"
DEFINE tProlog.%23
STRING 2A6E756C6C2D7465726D2A00

! String "*unknown-term(tag="
DEFINE tProlog.%24
STRING 2A756E6B6E6F776E2D7465726D287461673D00

! String ")*"
DEFINE tProlog.%25
STRING 292A00

! String "*null-clause*"
DEFINE tProlog.%26
STRING 2A6E756C6C2D636C617573652A00

! String ":- "
DEFINE tProlog.%27
STRING 3A2D2000

! String "Line "
DEFINE tProlog.%28
STRING 4C696E652000

! String "Syntax error - "
DEFINE tProlog.%29
STRING 53796E746178206572726F72202D2000

! String "Too many errors: I am giving up"
DEFINE tProlog.%30
STRING 546F6F206D616E79206572726F72733A204920616D20676976696E6720757000

! String "identifier too long"
DEFINE tProlog.%31
STRING 6964656E74696669657220746F6F206C6F6E6700

! String "bad token /"
DEFINE tProlog.%32
STRING 62616420746F6B656E202F00

! String "end of file in comment"
DEFINE tProlog.%33
STRING 656E64206F662066696C6520696E20636F6D6D656E7400

! String "missing quote"
DEFINE tProlog.%34
STRING 6D697373696E672071756F746500

! String "unterminated string"
DEFINE tProlog.%35
STRING 756E7465726D696E6174656420737472696E6700

! String "illegal character"
DEFINE tProlog.%36
STRING 696C6C6567616C2063686172616374657200

! String "identifier "
DEFINE tProlog.%37
STRING 6964656E7469666965722000

! String "variable "
DEFINE tProlog.%38
STRING 7661726961626C652000

! String "number"
DEFINE tProlog.%39
STRING 6E756D62657200

! String "char constant"
DEFINE tProlog.%40
STRING 6368617220636F6E7374616E7400

! String ":-"
DEFINE tProlog.%41
STRING 3A2D00

! String "string constant"
DEFINE tProlog.%42
STRING 737472696E6720636F6E7374616E7400

! String "unknown token"
DEFINE tProlog.%43
STRING 756E6B6E6F776E20746F6B656E00

! String "too many variables"
DEFINE tProlog.%44
STRING 746F6F206D616E79207661726961626C657300

! String "yes"
DEFINE tProlog.%45
STRING 79657300

! String "expected "
DEFINE tProlog.%46
STRING 65787065637465642000

! String ", found "
DEFINE tProlog.%47
STRING 2C20666F756E642000

! String "wrong number of args"
DEFINE tProlog.%48
STRING 77726F6E67206E756D626572206F66206172677300

! String "expected a term"
DEFINE tProlog.%49
STRING 65787065637465642061207465726D00

! String "literal must be a compound term"
DEFINE tProlog.%50
STRING 6C69746572616C206D757374206265206120636F6D706F756E64207465726D00

! String "bad tag"
DEFINE tProlog.%51
STRING 6261642074616700

! String "Key"
DEFINE tProlog.%52
STRING 4B657900

! String "(TRO)"
DEFINE tProlog.%53
STRING 2854524F2900

! String "Exit"
DEFINE tProlog.%54
STRING 4578697400

! String ": "
DEFINE tProlog.%55
STRING 3A2000

! String "Redo"
DEFINE tProlog.%56
STRING 5265646F00

! String "Call"
DEFINE tProlog.%57
STRING 43616C6C00

! String "call to undefined relation "
DEFINE tProlog.%58
STRING 63616C6C20746F20756E646566696E65642072656C6174696F6E2000

! String "no"
DEFINE tProlog.%59
STRING 6E6F00

! String "bad argument to call/1"
DEFINE tProlog.%60
STRING 62616420617267756D656E7420746F2063616C6C2F3100

! String "plus/3 needs at least two integers"
DEFINE tProlog.%61
STRING 706C75732F33206E65656473206174206C656173742074776F20696E74656765
STRING 727300

! String "times/3 needs at least two integers"
DEFINE tProlog.%62
STRING 74696D65732F33206E65656473206174206C656173742074776F20696E746567
STRING 65727300

! String "subject("
DEFINE tProlog.%63
STRING 7375626A6563742800

! String "  <store,"
DEFINE tProlog.%64
STRING 20203C73746F72652C00

! String "    <load,"
DEFINE tProlog.%65
STRING 202020203C6C6F61642C00

! String "      <plusa,"
DEFINE tProlog.%66
STRING 2020202020203C706C7573612C00

! String "        <global(a)>,"
DEFINE tProlog.%67
STRING 20202020202020203C676C6F62616C2861293E2C00

! String "        <lsl, <load, <local(16)>>, <const(2)>>>>,"
DEFINE tProlog.%68
STRING 20202020202020203C6C736C2C203C6C6F61642C203C6C6F63616C283136293E
STRING 3E2C203C636F6E73742832293E3E3E3E2C00

! String "    <local(20)>>"
DEFINE tProlog.%69
STRING 202020203C6C6F63616C283230293E3E00

! String ") :- ."
DEFINE tProlog.%70
STRING 29203A2D202E00

! String "rule(\"*str\", stmt, <store, reg, addr>) :- ."
DEFINE tProlog.%71
STRING 72756C6528222A737472222C2073746D742C203C73746F72652C207265672C20
STRING 616464723E29203A2D202E00

! String "rule(\"*ldr\", reg,  <load, addr>) :- ."
DEFINE tProlog.%72
STRING 72756C6528222A6C6472222C207265672C20203C6C6F61642C20616464723E29
STRING 203A2D202E00

! String "rule(\"*addfp\", reg, <local(N)>) :- ."
DEFINE tProlog.%73
STRING 72756C6528222A6164646670222C207265672C203C6C6F63616C284E293E2920
STRING 3A2D202E00

! String "rule(\"local\", addr, <local(N)>) :- ."
DEFINE tProlog.%74
STRING 72756C6528226C6F63616C222C20616464722C203C6C6F63616C284E293E2920
STRING 3A2D202E00

! String "rule(\"*add\", reg, <plusa, reg, rand>) :- ."
DEFINE tProlog.%75
STRING 72756C6528222A616464222C207265672C203C706C7573612C207265672C2072
STRING 616E643E29203A2D202E00

! String "rule(\"index\", addr, <plusa, reg, reg>) :- ."
DEFINE tProlog.%76
STRING 72756C652822696E646578222C20616464722C203C706C7573612C207265672C
STRING 207265673E29203A2D202E00

! String "rule(\"scale\", addr,"
DEFINE tProlog.%77
STRING 72756C6528227363616C65222C20616464722C00

! String "       <plusa, reg, <lsl, reg, <const(N)>>>) :- ."
DEFINE tProlog.%78
STRING 202020202020203C706C7573612C207265672C203C6C736C2C207265672C203C
STRING 636F6E7374284E293E3E3E29203A2D202E00

! String "rule(\"*global\", reg, <global(X)>) :- ."
DEFINE tProlog.%79
STRING 72756C6528222A676C6F62616C222C207265672C203C676C6F62616C2858293E
STRING 29203A2D202E00

! String "rule(\"*lsl\", reg, <lsl, reg, rand>) :- ."
DEFINE tProlog.%80
STRING 72756C6528222A6C736C222C207265672C203C6C736C2C207265672C2072616E
STRING 643E29203A2D202E00

! String "rule(\"lshiftc\", rand, <lsl, reg, <const(N)>>) :- ."
DEFINE tProlog.%81
STRING 72756C6528226C736869667463222C2072616E642C203C6C736C2C207265672C
STRING 203C636F6E7374284E293E3E29203A2D202E00

! String "rule(\"lshiftr\", rand, <lsl, reg, reg>) :- ."
DEFINE tProlog.%82
STRING 72756C6528226C736869667472222C2072616E642C203C6C736C2C207265672C
STRING 207265673E29203A2D202E00

! String "rule(\"*mov\", reg, <const(N)>) :- ."
DEFINE tProlog.%83
STRING 72756C6528222A6D6F76222C207265672C203C636F6E7374284E293E29203A2D
STRING 202E00

! String "rule(\"const\", rand, <const(N)>) :- ."
DEFINE tProlog.%84
STRING 72756C652822636F6E7374222C2072616E642C203C636F6E7374284E293E2920
STRING 3A2D202E00

! String "rule(\"reg\", rand, reg) :- ."
DEFINE tProlog.%85
STRING 72756C652822726567222C2072616E642C2072656729203A2D202E00

! String "rule(\"indir\", addr, reg) :- ."
DEFINE tProlog.%86
STRING 72756C652822696E646972222C20616464722C2072656729203A2D202E00

! String "use_rule(NT, Tree, node(Name, Kids)) :-"
DEFINE tProlog.%87
STRING 7573655F72756C65284E542C20547265652C206E6F6465284E616D652C204B69
STRING 64732929203A2D00

! String "  rule(Name, NT, RHS), match(RHS, Tree, Kids, nil)."
DEFINE tProlog.%88
STRING 202072756C65284E616D652C204E542C20524853292C206D6174636828524853
STRING 2C20547265652C204B6964732C206E696C292E00

! String "match(NT, Tree, Parse:Kids0, Kids0) :-"
DEFINE tProlog.%89
STRING 6D61746368284E542C20547265652C2050617273653A4B696473302C204B6964
STRING 733029203A2D00

! String "  use_rule(NT, Tree, Parse)."
DEFINE tProlog.%90
STRING 20207573655F72756C65284E542C20547265652C205061727365292E00

! String "match(node(W, PS), node(W, TS), Kids, Kids0) :-"
DEFINE tProlog.%91
STRING 6D61746368286E6F646528572C205053292C206E6F646528572C205453292C20
STRING 4B6964732C204B6964733029203A2D00

! String "  matchall(PS, TS, Kids, Kids0)."
DEFINE tProlog.%92
STRING 20206D61746368616C6C2850532C2054532C204B6964732C204B69647330292E
STRING 00

! String "matchall(nil, nil, Kids0, Kids0) :- ."
DEFINE tProlog.%93
STRING 6D61746368616C6C286E696C2C206E696C2C204B696473302C204B6964733029
STRING 203A2D202E00

! String "matchall(P:PS, T:TS, Kids, Kids0) :-"
DEFINE tProlog.%94
STRING 6D61746368616C6C28503A50532C20543A54532C204B6964732C204B69647330
STRING 29203A2D00

! String "  match(P, T, Kids, Kids1), matchall(PS, TS, Kids1, Kids0)."
DEFINE tProlog.%95
STRING 20206D6174636828502C20542C204B6964732C204B69647331292C206D617463
STRING 68616C6C2850532C2054532C204B696473312C204B69647330292E00

! String "cost(node(X, TS), C) :-"
DEFINE tProlog.%96
STRING 636F7374286E6F646528582C205453292C204329203A2D00

! String "  opcost(X, A), allcosts(TS, B), plus(A, B, C)."
DEFINE tProlog.%97
STRING 20206F70636F737428582C2041292C20616C6C636F7374732854532C2042292C
STRING 20706C757328412C20422C2043292E00

! String "allcosts(nil, 0) :- ."
DEFINE tProlog.%98
STRING 616C6C636F737473286E696C2C203029203A2D202E00

! String "allcosts(T:TS, C) :-"
DEFINE tProlog.%99
STRING 616C6C636F73747328543A54532C204329203A2D00

! String "  cost(T, A), allcosts(TS, B), plus(A, B, C)."
DEFINE tProlog.%100
STRING 2020636F737428542C2041292C20616C6C636F7374732854532C2042292C2070
STRING 6C757328412C20422C2043292E00

! String "opcost('*':_, 1) :- !."
DEFINE tProlog.%101
STRING 6F70636F737428272A273A5F2C203129203A2D20212E00

! String "opcost(_, 0) :- ."
DEFINE tProlog.%102
STRING 6F70636F7374285F2C203029203A2D202E00

! String "answer(P, C) :-"
DEFINE tProlog.%103
STRING 616E7377657228502C204329203A2D00

! String "  subject(T), use_rule(stmt, T, P), cost(P, C)."
DEFINE tProlog.%104
STRING 20207375626A6563742854292C207573655F72756C652873746D742C20542C20
STRING 50292C20636F737428502C2043292E00

! String "min(N, P) :- min1(N, 0, P)."
DEFINE tProlog.%105
STRING 6D696E284E2C205029203A2D206D696E31284E2C20302C2050292E00

! String "min1(N, N, P) :- call(P), !."
DEFINE tProlog.%106
STRING 6D696E31284E2C204E2C205029203A2D2063616C6C2850292C20212E00

! String "min1(N, N0, P) :- plus(N0, 1, N1), min1(N, N1, P)."
DEFINE tProlog.%107
STRING 6D696E31284E2C204E302C205029203A2D20706C7573284E302C20312C204E31
STRING 292C206D696E31284E2C204E312C2050292E00

! String "# :- answer(P, C)."
DEFINE tProlog.%108
STRING 23203A2D20616E7377657228502C2043292E00

! String ":"
DEFINE tProlog.%110
STRING 3A00

! String "!"
DEFINE tProlog.%111
STRING 2100

! String "="
DEFINE tProlog.%112
STRING 3D00

! String "("
DEFINE tProlog.%113
STRING 2800

! String ")"
DEFINE tProlog.%114
STRING 2900

! String ","
DEFINE tProlog.%115
STRING 2C00

! String "."
DEFINE tProlog.%116
STRING 2E00

! String "<"
DEFINE tProlog.%117
STRING 3C00

! String ">"
DEFINE tProlog.%118
STRING 3E00

! String "#"
DEFINE tProlog.%119
STRING 2300

! Descriptor for *anon*
DEFINE tProlog.%109
WORD 0
WORD 0
WORD tProlog.%109.%anc

DEFINE tProlog.%109.%anc
WORD tProlog.%109

! End of file
]]*)
