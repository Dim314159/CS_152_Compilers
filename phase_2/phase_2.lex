digit [0-9]
letter [a-zA-Z]
identifier ({letter}({letter}|{digit}|_)*({letter}|{digit}))|{letter}
identifier_error_digit ({digit}|_)+{identifier}
identifier_error_un_score {identifier}_+
comment ##.*

%{
    #include "y.tab.h"
    int line = 1;
    int position = 0;
    char* token_text;
%}
%%
function {position += yyleng; return FUNCTION;}
beginparams {position += yyleng; return BEGIN_PARAMS;}
endparams {position += yyleng; return END_PARAMS;}
beginlocals {position += yyleng; return BEGIN_LOCALS;}
endlocals {position += yyleng; return END_LOCALS;}
beginbody {position += yyleng; return BEGIN_BODY;}
endbody {position += yyleng; return END_BODY;}
integer {position += yyleng; return INTEGER;}
array {position += yyleng; return ARRAY;}
of {position += yyleng; return OF;}
if {position += yyleng; return IF;}
then {position += yyleng; return THEN;}
endif {position += yyleng; return ENDIF;}
else {position += yyleng; return ELSE;}
while {position += yyleng; return WHILE;}
do {position += yyleng; return DO;}
for {position += yyleng; return FOR;}
beginloop {position += yyleng; return BEGINLOOP;}
endloop {position += yyleng; return ENDLOOP;}
continue {position += yyleng; return CONTINUE;}
read {position += yyleng; return READ;}
write {position += yyleng; return WRITE;}
and {position += yyleng; return AND;}
or {position += yyleng; return OR;}
not {position += yyleng; return NOT;}
true {position += yyleng; return TRUE;}
false {position += yyleng; return FALSE;}
return {position += yyleng; return RETURN;}


"+" {position += yyleng; return ADD;}
"-" {position += yyleng; return SUB;}
"*" {position += yyleng; return MULT;}
"/" {position += yyleng; return DIV;}
"%" {position += yyleng; return MOD;}

"==" {position += yyleng; return EQ;}
"<>" {position += yyleng; return NEQ;}
"<" {position += yyleng; return LT;}
">" {position += yyleng; return GT;}
"<=" {position += yyleng; return LTE;}
">=" {position += yyleng; return GTE;}

{identifier} {position += yyleng; return IDENT;}
{digit}+ {position += yyleng; return NUMBER;}

"(" {position += yyleng; return L_PAREN;}
")" {position += yyleng; return R_PAREN;}
";" {position += yyleng; return SEMICOLON;}
":" {position += yyleng; return COLON;}
"," {position += yyleng; return COMMA;}
"[" {position += yyleng; return L_SQUARE_BRACKET;}
"]" {position += yyleng; return R_SQUARE_BRACKET;}
":=" {position += yyleng; return ASSIGN;}

{comment} {}
"\t"|" " {position += yyleng;}

{identifier_error_digit} {printf("Error at line %d, column %d: identifier \"%s\" must begin with a letter\n", line, position, yytext);}
{identifier_error_un_score} {printf("Error at line %d, column %d: identifier \"%s\" cannot end with an underscore\n", line, position, yytext);}
. {token_text = yytext;}

"\n" {position = 0; line += 1;}


%%

