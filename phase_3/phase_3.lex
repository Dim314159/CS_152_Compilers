digit [0-9]
letter [a-zA-Z]
identifier ({letter}({letter}|{digit}|_)*({letter}|{digit}))|{letter}
identifier_error_digit ({digit}|_)+{identifier}
identifier_error_un_score {identifier}_+
comment ##.*

%{
    #include <iostream>
    #define YY_DECL yy::parser::symbol_type yylex()
    #include "parser.tab.hh"
    
    static yy::location loc;
    
    int line = 1;
    int position = 0;
    char* token_text;
%}

%option noyywrap 

%{
#define YY_USER_ACTION loc.columns(yyleng);
%}

%%

%{
loc.step(); 
%}

function {position += yyleng; return yy::parser::make_FUNCTION(loc);}
beginparams {position += yyleng; return yy::parser::make_BEGIN_PARAMS(loc);}
endparams {position += yyleng; return yy::parser::make_END_PARAMS(loc);}
beginlocals {position += yyleng; return yy::parser::make_BEGIN_LOCALS(loc);}
endlocals {position += yyleng; return yy::parser::make_END_LOCALS(loc);}
beginbody {position += yyleng; return yy::parser::make_BEGIN_BODY(loc);}
endbody {position += yyleng; return yy::parser::make_END_BODY(loc);}
integer {position += yyleng; return yy::parser::make_INTEGER(loc);}
array {position += yyleng; return yy::parser::make_ARRAY(loc);}
of {position += yyleng; return yy::parser::make_OF(loc);}
if {position += yyleng; return yy::parser::make_IF(loc);}
then {position += yyleng; return yy::parser::make_THEN(loc);}
endif {position += yyleng; return yy::parser::make_ENDIF(loc);}
else {position += yyleng; return yy::parser::make_ELSE(loc);}
while {position += yyleng; return yy::parser::make_WHILE(loc);}
do {position += yyleng; return yy::parser::make_DO(loc);}
for {position += yyleng; return yy::parser::make_FOR(loc);}
beginloop {position += yyleng; return yy::parser::make_BEGINLOOP(loc);}
endloop {position += yyleng; return yy::parser::make_ENDLOOP(loc);}
continue {position += yyleng; return yy::parser::make_CONTINUE(loc);}
read {position += yyleng; return yy::parser::make_READ(loc);}
write {position += yyleng; return yy::parser::make_WRITE(loc);}
and {position += yyleng; return yy::parser::make_AND(loc);}
or {position += yyleng; return yy::parser::make_OR(loc);}
not {position += yyleng; return yy::parser::make_NOT(loc);}
true {position += yyleng; return yy::parser::make_TRUE(loc);}
false {position += yyleng; return yy::parser::make_FALSE(loc);}
return {position += yyleng; return yy::parser::make_RETURN(loc);}


"+" {position += yyleng; return yy::parser::make_ADD(loc);}
"-" {position += yyleng; return yy::parser::make_SUB(loc);}
"*" {position += yyleng; return yy::parser::make_MULT(loc);}
"/" {position += yyleng; return yy::parser::make_DIV(loc);}
"%" {position += yyleng; return yy::parser::make_MOD(loc);}

"==" {position += yyleng; return yy::parser::make_EQ(loc);}
"<>" {position += yyleng; return yy::parser::make_NEQ(loc);}
"<" {position += yyleng; return yy::parser::make_LT(loc);}
">" {position += yyleng; return yy::parser::make_GT(loc);}
"<=" {position += yyleng; return yy::parser::make_LTE(loc);}
">=" {position += yyleng; return yy::parser::make_GTE(loc);}

{identifier} {position += yyleng; return yy::parser::make_IDENT(yytext, loc);}
{digit}+ {position += yyleng; return yy::parser::make_NUMBER(atoi(yytext), loc);}

"(" {position += yyleng; return yy::parser::make_L_PAREN(loc);}
")" {position += yyleng; return yy::parser::make_R_PAREN(loc);}
";" {position += yyleng; return yy::parser::make_SEMICOLON(loc);}
":" {position += yyleng; return yy::parser::make_COLON(loc);}
"," {position += yyleng; return yy::parser::make_COMMA(loc);}
"[" {position += yyleng; return yy::parser::make_L_SQUARE_BRACKET(loc);}
"]" {position += yyleng; return yy::parser::make_R_SQUARE_BRACKET(loc);}
":=" {position += yyleng; return yy::parser::make_ASSIGN(loc);}

{comment} {}
"\t"|" " {position += yyleng;}

{identifier_error_digit} {printf("Error at line %d, column %d: identifier \"%s\" must begin with a letter\n", line, position, yytext);}
{identifier_error_un_score} {printf("Error at line %d, column %d: identifier \"%s\" cannot end with an underscore\n", line, position, yytext);}
. {token_text = yytext;}

"\n" {position = 0; line += 1;}

<<EOF>> {return yy::parser::make_END(loc);}

%%

