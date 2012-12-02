%token A B C D

%start<int> s

%%

s : a { 1 } ;

a : B b { 2 }
  | A   { 3 } ;

b : C c { 3 } ;

c : D a { 4 } ;
