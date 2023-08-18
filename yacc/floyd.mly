%token LPAR RPAR KET X Y

%start<unit> s

%%

s : a {()} | b {()} ;

a : LPAR a RPAR {()} | LPAR b RPAR {()} | X {()} ;

b : LPAR a KET {()} | LPAR b KET {()} | Y {()} ;
