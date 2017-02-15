
%token <string> CHAR
%token IN
%token LPAREN RPAREN
%token EOF

%start main
%type <string> main
%%
main:
| IN LPAREN name EOF { String.concat "" $3 }

name:
| CHAR RPAREN { [$1] }
| CHAR name { $1 :: $2 }
