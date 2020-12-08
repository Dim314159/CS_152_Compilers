%{
#include "stdlib.h"
#include "stdio.h"
#include "string.h"
#include "y.tab.h"

extern int line;
extern int position;
extern const char* yytext;
extern const int yyleng;
extern const char* token_text;
int my_flag = 1;

void yyerror(const char *msg);
int yylex();
%}

%union{
    char* cval;
    int ival;
}

%define parse.error verbose

%token IDENT NUMBER FUNCTION BEGIN_PARAMS END_PARAMS BEGIN_LOCALS END_LOCALS BEGIN_BODY END_BODY INTEGER ARRAY OF IF THEN ENDIF ELSE WHILE DO FOR BEGINLOOP ENDLOOP CONTINUE READ WRITE COMMA SEMICOLON COLON TRUE FALSE RETURN

%type<cval> IDENT
%type<ival> NUMBER

// 9
%right ASSIGN
// 8
%left OR
// 7
%left AND
// 6
%right NOT
// 5
%left EQ NEQ LT GT LTE GTE
// 4
%left ADD SUB
// 3
%left MULT DIV MOD
// 2
%right UMINUS
// 1
%left L_SQUARE_BRACKET R_SQUARE_BRACKET
// 0
%left L_PAREN R_PAREN

%start program

%%

program: {printf("program -> epsilon\n");}
       | program function {printf("program -> program function\n");}
       | error {my_flag = 0; printf("ERROR: in line %d, position %d: unexpected item %s\n", line, position - yyleng, yytext);}
       ;
// function
function: FUNCTION identifier SEMICOLON BEGIN_PARAMS declaration_semicolon END_PARAMS BEGIN_LOCALS declaration_semicolon END_LOCALS BEGIN_BODY statement_semicolon END_BODY {printf("function -> FUNCTION identifier SEMICOLON BEGIN_PARAMS declaration_semicolon END_PARAMS BEGIN_LOCALS declaration_semicolon END_LOCALS BEGIN_BODY statement_semicolon END_BODY\n");}
        ;

declaration_semicolon: {printf("declaration_semicolon -> epsilon\n");}
                     | declaration_semicolon declaration SEMICOLON {printf("declaration_semicolon -> declaration_semicolon declaration SEMICOLON\n");}
                     ;
// declaration
declaration: identifier comma_identifier COLON declaration_loop_1 INTEGER 
           {printf("declaration -> identifier comma_identifier COLON declaration_loop_1 INTEGER\n");}
           ;

comma_identifier: {printf("comma_identifier -> epsilon\n");}
                | comma_identifier COMMA identifier {printf("comma_identifier -> comma_identifier COMMA identifier\n");}
                ;

declaration_loop_1: {printf("declaration_loop_1 -> epsilon\n");}
                  | ARRAY L_SQUARE_BRACKET number R_SQUARE_BRACKET declaration_loop_2 OF {printf("declaration_loop_1 -> ARRAY L_SQUARE_BRACKET number R_SQUARE_BRACKET declaration_loop_2 OF\n");}
                  ;

declaration_loop_2: {printf("declaration_loop_2 -> epsilon\n");}
                  | L_SQUARE_BRACKET number R_SQUARE_BRACKET {printf("declaration_loop_2 -> L_SQUARE_BRACKET number R_SQUARE_BRACKET\n");}
                  ;

statement_semicolon: statement SEMICOLON {printf("statement_semicolon -> statement SEMICOLON\n");}
                   | statement_semicolon statement SEMICOLON {printf("statement_semicolon -> statement_semicolon statement SEMICOLON\n");}
                   ;
// statement
statement: var ASSIGN expression {printf("statement -> var ASSIGN expression\n");}
         | var {my_flag = 0; printf("ERROR: in line %d, position %d: unexpected item %s do you mean :=?\n", line, position - yyleng, token_text);} error expression
         | IF bool_expr THEN statement_semicolon else_loop ENDIF {printf("statement -> IF bool_expr THEN statement_semicolon else_loop ENDIF\n");}
         | WHILE bool_expr BEGINLOOP statement_semicolon ENDLOOP {printf("statement -> WHILE bool_expr BEGINLOOP statement_semicolon ENDLOOP\n");}
         | DO BEGINLOOP statement_semicolon ENDLOOP WHILE bool_expr {printf("statement -> DO BEGINLOOP statement_semicolon ENDLOOP WHILE bool_expr\n");}
         | FOR var ASSIGN number SEMICOLON bool_expr SEMICOLON var ASSIGN expression BEGINLOOP statement_semicolon ENDLOOP {printf("statement -> FOR war ASSIGN number SEMICOLON bool_expr SEMICOLON var ASSIGN expression BEGINLOOP statement_semicolon ENDLOOP\n");}
         | READ var comma_var {printf("statement -> READ var comma_var\n");}
         | WRITE var comma_var {printf("statement -> WRITE var comma_var\n");}
         | CONTINUE {printf("statement -> CONTINUE\n");}
         | RETURN expression {printf("statement -> RETURN expression\n");}
         ;

else_loop: {printf("else_loop -> epsilon\n");}
         | ELSE statement_semicolon {printf("else_loop -> ELSE statement_semicolon\n");}
         ;

comma_var: {printf("comma_var -> epsilon\n");}
         | comma_var COMMA var {printf("comma_var -> comma_var COMMA var\n");}
         ;

// bool_expr
bool_expr: relation_and_expr bool_expr_loop {printf("bool_expr -> relation_and_expr bool_expr_loop\n");}
         ;

bool_expr_loop: {printf("bool_expr_loop -> epsilon\n");}
              | bool_expr_loop OR relation_and_expr {printf("bool_expr_loop -> bool_expr_loop OR relation_and_expr\n");}
              ;

// relation_and_expr
relation_and_expr: relation_expr relation_and_expr_loop 
                 {printf("relation_and_expr -> relation_expr relation_and_expr_loop\n");}
                 ;

relation_and_expr_loop: {printf("relation_and_expr_loop -> epsilon\n");}
                      | relation_and_expr_loop AND relation_expr {printf("relation_and_expr_loop -> relation_and_expr_loop AND relation_expr\n");}
                      ;

// relation_expr
relation_expr: NOT relation_expr_fork {printf("relation_expr -> NOT relation_expr_fork\n");}
             | relation_expr_fork {printf("relation_expr -> relation_expr_fork\n");}
             ;

relation_expr_fork: expression comp expression {printf("relation_expr_fork -> expression comp expression\n");}
                  | TRUE {printf("relation_expr_fork -> TRUE\n");}
                  | FALSE {printf("relation_expr_fork -> FALSE\n");}
                  | L_PAREN bool_expr R_PAREN {printf("relation_expr_fork -> L_PAREN bool_expr R_PAREN\n");}
                  ;

// comp
comp: EQ {printf("comp -> EQ\n");}
    | NEQ {printf("comp -> NEQ\n");}
    | LT {printf("comp -> LT\n");}
    | GT {printf("comp -> GT\n");}
    | LTE {printf("comp -> LTE\n");}
    | GTE {printf("comp -> GTE\n");}
    ;

// var
var: identifier {printf("var -> identifier\n");}
   | identifier L_SQUARE_BRACKET expression R_SQUARE_BRACKET var_loop {printf("var -> identifier L_SQUARE_BRACKET expression R_SQUARE_BRACKET var_loop\n");}
   ;

var_loop: {printf("var_loop -> epsilon\n");}
        | L_SQUARE_BRACKET expression R_SQUARE_BRACKET {printf("var_loop -> L_SQUARE_BRACKET expression R_SQUARE_BRACKET\n");}
        ;

// expression
expression: multiplicative_expr expression_loop {printf("expression -> multiplicative_expr expression_loop\n");}
          ;

expression_loop: {printf("expression_loop -> epsilon\n");}
               | expression_loop SUB multiplicative_expr {printf("expression_loop -> expression_loop SUB multiplicative_expr\n");}
               | expression_loop ADD multiplicative_expr {printf("expression_loop -> expression_loop ADD multiplicative_expr\n");}
               ;

// multiplicative_expr
multiplicative_expr: term multiplicative_expr_loop {printf("multiplicative_expr -> term multiplicative_expr_loop\n");}
                   ;

multiplicative_expr_loop: {printf("multiplicative_expr_loop -> epsilon\n");}
                        | multiplicative_expr_loop MOD term {printf("multiplicative_expr_loop -> multiplicative_expr_loop MOD term \n");}
                        | multiplicative_expr_loop DIV term {printf("multiplicative_expr_loop -> multiplicative_expr_loop DIV term\n");}
                        | multiplicative_expr_loop MULT term {printf("multiplicative_expr_loop -> multiplicative_expr_loop MULT term\n");}
                        ;

// term
term: term_fork {printf("term -> term_fork\n");}
    | SUB term_fork %prec UMINUS {printf("term -> SUB term_fork\n");}
    | identifier L_PAREN term_loop_1 R_PAREN {printf("term -> identifier L_PAREN term_loop\n");}
    ;

term_fork: var {printf("term_fork -> var\n");}
         | number {printf("term_fork -> number\n");}
         | L_PAREN expression R_PAREN {printf("term_fork -> L_PAREN expression R_PAREN\n");}
         ;

term_loop_1: {printf("term_loop_1 -> epsilon\n");}
           | expression term_loop_1_1 {printf("term_loop_1 -> expression term_loop_1_1\n");}
           ;

term_loop_1_1: {printf("term_loop_1_1 -> epsilon\n");}
             | term_loop_1_1 COMMA expression {printf("term_loop_1_1 -> term_loop_1_1 COMMA expression\n");}
             ;



identifier: IDENT {printf("identifier -> IDENT %s\n", yytext);}
          ;
number: NUMBER {printf("number -> NUMBER %s\n", yytext);}
      ;

%%

int main(int argc, char** argv)
{
    yyparse();
    return 0;
}
void yyerror(const char* msg)
{
    if (my_flag == 1) {
        printf("Error: in line %d, position %d: %s\n", line, position - yyleng, msg);
    }
    else {
        my_flag = 1;
    }
}
