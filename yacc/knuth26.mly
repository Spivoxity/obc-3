/* Knuth's example 26: mutilated expressions */

%start expr

%type<unit> expr

%token A B

%%

expr : A { () } | left right { () } ;

left : A { () } | left middle B { () } ;

right : A { () } | B middle right { () } ;

middle : A { () } | B middle middle B { () } ;
