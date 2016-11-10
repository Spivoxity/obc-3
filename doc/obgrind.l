/* pp/obgrind/obgrind.l */
/* $Id: obgrind.l,v 1.3 2004-10-12 16:48:16 mike Exp $ */

%{
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdarg.h>

#define HASHSIZE 211
#define MAXIDS 1000

void set_mode(int mode);
int tablen(char *s);
void start_line(void);
void end_line(void);
void start_comment(void);
void end_comment(void);
void emit_ident(void);
void emit_number(void);
void emit_hexconst(void);
void emit(int mode, char *fmt, ...);
void translit(int mode, char flag, char *text, char *trans);

int level = 0;
int space = 0;
int new = 1;
int indent = 0;
int para = 0;
int comment = 0;
int commin;
int states[10];			/* stack of start symbols */
int sp = 0;			/* stack pointer */

int eflag = 0;

#define push(st) states[sp++] = YY_START, BEGIN st
#define pop()    BEGIN states[--sp]

#define TEXT 1
#define MATH 2
#define EITHER 3
#define FWD 4

int state = TEXT;

#undef ECHO
#define ECHO printf("?%s?\n", yytext)

#define COPY printf("%s", yytext)

int yywrap(void) { return 1; }
%}

%s				COMMENT PROG QUOTE VERB BOX
%x				MATRIX
%%

<PROG,COMMENT>\n		end_line();
<QUOTE>\n			space = 1;
<PROG>^[ \t]+			indent = tablen(yytext);
<PROG,QUOTE,COMMENT>[ \t]+	space = 1;

<MATRIX>"\\verb/"[^/]*"/"	{ COPY; }
<MATRIX>"\\|" 			{ COPY; }
<MATRIX>^"\\obprog".*\n		{ COPY; new = 1; level = 0; indent = 0;
				  push(PROG); }
<MATRIX>^("\\program"|"\\verbatim"|"\\interact"|"\\wideinter").*\n {
				  COPY; push(VERB); }
<PROG>^"\\endob".*\n		{ COPY; pop(); }
<MATRIX>"|" 			{ new = 0; level = 0; push(QUOTE); }
<VERB>^("\\endprog"|"\\endverb"|"\\endinter").*\n { 
				  COPY; pop(); }
<MATRIX,VERB>([^\\|\n]*\n?)|.	{ COPY; }
<MATRIX>"%".*\n			{ COPY; }

<PROG,QUOTE>[A-Za-z][A-Za-z0-9_]*  emit_ident();
<PROG,QUOTE>[A-Za-z][A-Za-z0-9_]*[*\-]/[ :;=(]  emit_ident();

<PROG,QUOTE>[0-9]+("."[0-9]+)?("E"[0-9]+)?	emit_number();
<PROG,QUOTE>[0-9][0-9A-F]*[HX]	emit_hexconst();
<PROG,QUOTE>'[^'\n]*'|\"[^"\n]*\" translit(MATH, 'S', yytext, NULL);
<PROG,QUOTE>[=+\-*,./]		emit(MATH, yytext);
<PROG,QUOTE>":"			emit(MATH, "{:}\\;");
<PROG,QUOTE>"&"			emit(MATH, "\\obAND ");
<PROG,QUOTE>[(\[]		level++; emit(MATH, yytext);
<PROG,QUOTE>[)\]]		level--; emit(MATH, yytext);
<PROG,QUOTE>"()"		emit(MATH, "(\\,)");
<PROG,QUOTE>":="		emit(MATH, yytext);
<PROG,QUOTE>"<"			emit(MATH, "\\lt ");
<PROG,QUOTE>">"			emit(MATH, "\\gt ");
<PROG,QUOTE>"#"			emit(MATH, "\\neq ");
<PROG,QUOTE>"<="		emit(MATH, "\\leq ");
<PROG,QUOTE>">="		emit(MATH, "\\geq ");
<PROG,QUOTE>".."		emit(MATH, "\\dotdot ");
<PROG,QUOTE>"^"			emit(MATH, "\\obPTR ");
<PROG,QUOTE>";"			{ if (level == 0) emit(TEXT, ";");
				  else emit(MATH, ";\\;"); }
<PROG>"|"			emit(TEXT, "\\VB ");

<PROG,QUOTE>"\\hbox{"	        { push(BOX); emit(MATH, yytext); 
				  state = TEXT; }
<PROG,QUOTE>\\[A-Za-z]+		|
<PROG,QUOTE>\\.			emit(MATH, yytext);
<PROG,QUOTE>~			emit(MATH, "\\obNOT ");
<PROG>"(*"			{ push(COMMENT); start_comment(); }

<COMMENT>[^ *|\t\n]+		emit(TEXT, yytext);
<COMMENT>"|"			push(QUOTE);
<COMMENT>"*"			emit(TEXT, yytext);
<COMMENT>"*)"			{ pop(); end_comment(); }
<QUOTE>"|"			{ set_mode(TEXT); pop(); }

<BOX>"|"			push(QUOTE);
<BOX>"}"			{ emit(TEXT, yytext); pop(); state = MATH; }
<BOX>[^|}]+			{ emit(TEXT, yytext); }

%%

void set_mode(int mode)
{
     for (;;) {
	  if (state == TEXT || mode == MATH) {
	       if (space > 0)
		    printf(" ");
	       else if (space < 0)
		    printf("\n");
	       space = 0;
	  }
	  if (state == mode) break;
	  printf("$");
	  state = mode;
     }
}

void start_line(void)
{
     if (! para) {
	  printf("\\LI");
	  if (comment) {
	       if (commin > 0) printf("\\IN{%d}", commin);
	       printf("\\CQ ");
	  }
	  else if (indent > 0)
	       printf("\\IN{%d}", indent);
     }
     new = 0;
}

void end_line(void)
{
     if (new && para)
	  printf("\\par");
     else if (para)
	  space = -1;
     else if (new) 
	  printf("\\BL\n");
     else 
	  emit(TEXT, "\\NL\n"); 
     new = 1; indent = 0;
}

void start_comment(void)
{
     if (! new)
	  emit(TEXT, "\\RC");
     else if (indent > 0) {
	  commin = indent;
	  emit(TEXT, "\\BC "); 
     }
     else {
	  para = 1;
	  emit(TEXT, "\\BP ");
     }
     comment = 1;
}

void end_comment(void)
{
     comment = 0;
     if (! para)
	  emit(TEXT, "\\EC");
     else {
	  emit(TEXT, "\\EP");
	  para = 0;
     }
}

int all_upper(char *s)
{
     for (; *s != '\0'; s++)
	  if (islower(*s)) return 0;
     return 1;
}

typedef struct key *key;

struct key {
     char *k_name;
     int k_kind;
     char *k_trans;
} keywords[] = {
     { "ARRAY", MATH, "\\K{ARRAY}\\;" },
     { "BEGIN", TEXT, NULL },
     { "BY", MATH, "\\P{BY}" },
     { "CASE", TEXT, NULL },
     { "CONST", TEXT, NULL },
     { "DIV", MATH, "\\O{DIV}" },
     { "DO", TEXT, NULL },
     { "ELSE", TEXT, NULL },
     { "ELSIF", TEXT, NULL },
     { "END", TEXT, NULL },
     { "EXIT", TEXT, NULL },
     { "FOR", TEXT, NULL },
     { "IF", TEXT, NULL },
     { "IMPORT", TEXT, NULL },
     { "IS", MATH, "\\P{IS}" },
     { "LOOP", TEXT, NULL },
     { "MOD", MATH, "\\O{MOD}" },
     { "MODULE", TEXT, NULL },
     { "NIL", MATH, "\\K{NIL}" },
     { "OF", MATH, "\\P{OF}" },
     { "OR", MATH, "\\P{OR}" },
     { "POINTER", MATH, "\\K{POINTER}\\;" },
     { "PROCEDURE", TEXT, NULL },
     { "RECORD", MATH, "\\K{RECORD}\\;" },
     { "REPEAT", TEXT, NULL },
     { "RETURN", TEXT, NULL },
     { "THEN", TEXT, NULL },
     { "TO", MATH, "\\P{TO}" },
     { "TYPE", TEXT, NULL },
     { "UNTIL", TEXT, NULL },
     { "VAR", EITHER, "\\K{VAR}\\;" },
     { "WHILE", TEXT, NULL },
     { "WITH", TEXT, NULL },
     { "GOTO", TEXT, NULL },
     { NULL, 0, NULL }
};

key lookup(char *name)
{
     key k;

     for (k = keywords; k->k_name != NULL; k++)
	  if (strcmp(name, k->k_name) == 0)
	       return k;
     return NULL;
}

void translit(int mode, char flag, char *text, char *trans)
{
     char *buf = alloca(2*strlen(text)+16);
     char *bp = buf;

     *bp++ = '\\'; *bp++ = flag; *bp++ = '{';
     for (; *text != '\0'; text++) {
	  if (strchr(" _$&#{}~", *text)) *bp++ = '\\';
	  *bp++ = *text;
     }
     *bp++ = '}';
     *bp = '\0';

     emit(mode, (mode == MATH && trans != NULL ? trans : "%s"), buf);
}

void emit_ident(void)
{
     int len = strlen(yytext);
     char ch;

     if (len == 1)
	  emit(MATH, yytext);
     else if (len >= 3
	      && yytext[len-2] == '_'
	      && isdigit(yytext[len-1])) {
	  yytext[len-2] = '\0';
	  emit_ident();
	  emit(MATH, "_%c", yytext[len-1]);
     }
     else if (yytext[len-1] == '*' || yytext[len-1] == '-') {
	  ch = yytext[len-1]; yytext[len-1] = '\0';
	  emit_ident();
	  emit(MATH, "{%c}", ch);
     }
     else {
	  key s = lookup(yytext);
	  if (s == NULL) 
	       translit(MATH, (all_upper(yytext) ? 'D' : 'I'), 
			yytext, (char *) NULL);
	  else {
	       switch (s->k_kind) {
	       case TEXT:
	       case MATH:
		    translit(s->k_kind, 'K', yytext, s->k_trans);
		    break;

	       case EITHER:
		    translit((level > 0 ? MATH : TEXT),
			     'K', yytext, s->k_trans);
		    break;
	       }
	  }
     }
}

void emit_number()
{
     char buf[32];
     char *bp = buf, *s = yytext;

     for (; *s != '\0'; s++)
	  if (*s == 'E')
	       bp = stpcpy(bp, "{\\sci E}");
	  else
	       *bp++ = *s;

     emit(MATH, yytext);
}

void emit_hexconst()
{
     int len = strlen(yytext);

     emit(MATH, "\\HX{%.*s}{%c}", len-1, yytext, yytext[len-1]);
}

int tablen(char *s)
{
     int n = 0;

     for (; *s != '\0'; s++)
	  if (*s == '\t')
	       n = (n/8+1)*8;
	  else
	       n++;

     return n/2;
}

void emit(int mode, char *fmt, ...)
{
     va_list ap;

     if (new) start_line();
     set_mode(mode);
     va_start(ap, fmt);
     vprintf(fmt, ap);
     va_end(ap);
}

void usage(void)
{
     fprintf(stderr, "Usage: obgrind file\n");
     exit(2);
}

int main(int argc, char **argv)
{
     char *s;

     while (--argc > 0 && (*++argv)[0] == '-') {
	  for (s = *argv+1; *s != '\0'; s++) {
	       switch (*s) {
	       case 'e':
		    eflag++;
		    break;
	       default:
		    usage();
	       }
	  }
     }

     if (argc > 1) usage();

     if (argc == 0)
	  yyin = stdin;
     else if ((yyin = fopen(argv[0], "r")) == NULL) {
	  fprintf(stderr, "pgrind: can't read %s\n", argv[0]);
	  return 1;
     }

     BEGIN (eflag ? MATRIX : PROG);
     yylex();
     return 0;
}
