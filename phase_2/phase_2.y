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
       //| error {my_flag = 0; printf("ERROR: in line %d, position %d: unexpected item %s\n", line, position - yyleng, yytext);}
       ;
// function
function: FUNCTION identifier semicolon BEGIN_PARAMS declaration_semicolon END_PARAMS BEGIN_LOCALS declaration_semicolon END_LOCALS BEGIN_BODY statement_semicolon END_BODY {printf("function -> FUNCTION identifier semicolon BEGIN_PARAMS declaration_semicolon END_PARAMS BEGIN_LOCALS declaration_semicolon END_LOCALS BEGIN_BODY statement_semicolon END_BODY\n");}
        ;

declaration_semicolon: {printf("declaration_semicolon -> epsilon\n");}
                     | declaration_semicolon declaration semicolon {printf("declaration_semicolon -> declaration_semicolon declaration semicolon\n");}
                     ;
// declaration
declaration: comma_identifier COLON declaration_loop_1 INTEGER 
           {printf("declaration -> identifier comma_identifier COLON declaration_loop_1 INTEGER\n");}
           ;

comma_identifier: identifier {printf("comma_identifier -> identifier\n");}
                | comma_identifier COMMA identifier {printf("comma_identifier -> comma_identifier COMMA identifier\n");}
                | comma_identifier {my_flag = 0; printf("ERROR: in line %d, position %d: unexpected item %s do you mean ,?\n", line, position - yyleng, token_text);} error identifier
                ;

declaration_loop_1: {printf("declaration_loop_1 -> epsilon\n");}
                  | ARRAY L_SQUARE_BRACKET number R_SQUARE_BRACKET declaration_loop_2 OF {printf("declaration_loop_1 -> ARRAY L_SQUARE_BRACKET number R_SQUARE_BRACKET declaration_loop_2 OF\n");}
                  ;

declaration_loop_2: {printf("declaration_loop_2 -> epsilon\n");}
                  | L_SQUARE_BRACKET number R_SQUARE_BRACKET {printf("declaration_loop_2 -> L_SQUARE_BRACKET number R_SQUARE_BRACKET\n");}
                  ;

statement_semicolon: statement semicolon {printf("statement_semicolon -> statement semicolon\n");}
                   | statement_semicolon statement semicolon {printf("statement_semicolon -> statement_semicolon statement semicolon\n");}
                   ;
// statement
statement: var assign expression {printf("statement -> var assign expression\n");}
         | IF bool_expr THEN statement_semicolon else_loop ENDIF {printf("statement -> IF bool_expr THEN statement_semicolon else_loop ENDIF\n");}
         | WHILE bool_expr BEGINLOOP statement_semicolon ENDLOOP {printf("statement -> WHILE bool_expr BEGINLOOP statement_semicolon ENDLOOP\n");}
         | DO BEGINLOOP statement_semicolon ENDLOOP WHILE bool_expr {printf("statement -> DO BEGINLOOP statement_semicolon ENDLOOP WHILE bool_expr\n");}
         | FOR var assign number semicolon bool_expr semicolon var assign expression BEGINLOOP statement_semicolon ENDLOOP {printf("statement -> FOR war assign number semicolon bool_expr semicolon var assign expression BEGINLOOP statement_semicolon ENDLOOP\n");}
         | READ comma_var {printf("statement -> READ var comma_var\n");}
         | WRITE comma_var {printf("statement -> WRITE var comma_var\n");}
         | CONTINUE {printf("statement -> CONTINUE\n");}
         | RETURN expression {printf("statement -> RETURN expression\n");}
         ;

else_loop: {printf("else_loop -> epsilon\n");}
         | ELSE statement_semicolon {printf("else_loop -> ELSE statement_semicolon\n");}
         ;

comma_var: var {printf("comma_var -> var\n");}
         | comma_var COMMA var {printf("comma_var -> comma_var COMMA var\n");}
         | comma_var var {my_flag = 0; printf("ERROR: in line %d, position %d: unexpected item %s do you mean ,?\n", line, position - yyleng, token_text);}
         ;

// bool_expr
bool_expr: relation_and_expr {printf("bool_expr -> relation_and_expr\n");}
         | bool_expr OR relation_and_expr {printf("bool_expr -> bool_expr OR relation_and_expr\n");}
         ;

// relation_and_expr
relation_and_expr: relation_expr {printf("relation_and_expr -> relation_expr\n");}
                 | relation_and_expr AND relation_expr {printf("relation_and_expr -> relation_and_expr AND relation_expr\n");}
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
expression: multiplicative_expr {printf("expression -> multiplicative_expr\n");}
          | expression SUB multiplicative_expr {printf("expression -> expression SUB multiplicative_expr\n");}
          | expression ADD multiplicative_expr {printf("expression -> expression ADD multiplicative_expr\n");}
          ;

// multiplicative_expr
multiplicative_expr: term {printf("multiplicative_expr -> term\n");}
                   | multiplicative_expr MULT term {printf("multiplicative_expr -> multiplicative_expr MULT term\n");}
                   | multiplicative_expr DIV term {printf("multiplicative_expr -> multiplicative_expr DIV term\n");}
                   | multiplicative_expr MOD term {printf("multiplicative_expr -> multiplicative_expr MOD term\n");}
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
           | comma_expression {printf("term_loop_1 -> comma_expression\n");}
           ;

comma_expression: expression {printf("comma_expression -> expression\n");}
             | comma_expression COMMA expression {printf("comma_expression -> comma_expression COMMA expression\n");}
             | comma_expression {my_flag = 0; printf("ERROR: in line %d, position %d: unexpected item %s do you mean ,?\n", line, position - yyleng, token_text);} error expression
             ;

// semicolon
semicolon: SEMICOLON {printf("semicolon -> SEMICOLON\n");}
         | {my_flag = 0; printf("ERROR: in line %d, position %d: unexpected item %s do you mean ;?\n", line, position - yyleng, token_text);} error
         ;

// assign
assign: ASSIGN {printf("semicolon -> ASSIGN\n");}
      | {my_flag = 0; printf("ERROR: in line %d, position %d: unexpected item %s do you mean :=?\n", line, position - yyleng, token_text);} error
      ;

// comma
//comma: COMMA {printf("comma -> COMMA\n");}
//     | {my_flag = 0; printf("ERROR: in line %d, position %d: unexpected item %s do you mean ,?\n", line, position - yyleng, token_text);} error
//     ;


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
