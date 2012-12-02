%token LP RP A B RA

%start<unit> s

%%

s : a {()} | b {()} ;
a : A {()} | LP a RP {()} ;
b : B {()} | LP b RA  {()} ;
