%{
    extern int line;
    extern int position;
    extern const char* yytext;
    extern const int yyleng;
    extern const char* token_text;
%}


%skeleton "lalr1.cc"
%require "3.0.4"
%defines
%define api.token.constructor
%define api.value.type variant
%define parse.error verbose
%locations

%code requires
{
    #include <list>
    #include <string>
    #include <functional>
    #include <vector>
    #include <iostream>
    #include <stdlib.h>
    #include <stdio.h>
    using namespace std;
    
    struct non_term_type
    {
        string code;
        string id;
        list<string> ids;
        vector<string> code_list;
        string value;
        string ind;
        bool array;
    };
}

%code
{
    #include "parser.tab.hh"
    

        /* you may need these header files 
        * add more header file if you need more
        */
    #include <sstream>
    #include <map>
    #include <regex>
    #include <set>
    #include <iostream>
    #include <string>
    #include <algorithm>
    #include <climits>
    #include <unordered_set>
    #include <stack>
    
    struct tests
    {
       string name;
       yy::location loc;
    };
    
    struct ident_type
    {
       bool array;
       int dim;
    };
    
    map<string, string> ID_DIM;
    
    set<string> MY_FUNC;
    set<string> RESERVED = {"function", "func", "endfunc", "beginparams", "endparams", "beginlocals", "endlocals", "beginbody", "endbody", "integer", "array", "of", "if", "then", "endif", "else", "while", "do", "for", "beginloop", "endloop", "continue", "read", "write", "and", "or", "not", "true", "false", "return"};
    map<string, int> MY_IDS;
    bool FLAG_ERROR = false;
    
    
    stack<string> MY_LOOP;
    
    yy::parser::symbol_type yylex();
    
    string create_temp_var();
    string create_temp_label();
    
    //extern int yylex();
    //extern int yylex(void);
    
    //void yyerror(const char *msg);    /*declaration given by TA*/

        /* define your symbol table, global variables,
        * list of keywords or any function you may need here */

        /* end of your code */
}

%token END 0 "end of file";

%token FUNCTION BEGIN_PARAMS END_PARAMS BEGIN_LOCALS END_LOCALS BEGIN_BODY END_BODY INTEGER ARRAY OF IF THEN ENDIF ELSE WHILE DO FOR BEGINLOOP ENDLOOP CONTINUE READ WRITE COMMA SEMICOLON COLON TRUE FALSE RETURN

%token <int> NUMBER
%token <string> IDENT

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


%type <string> program function identifier
%type <string> statement statement_semicolon else_loop all_loops comp
%type <list<string>> comma_identifier
%type <non_term_type> declaration_semicolon declaration
%type <non_term_type> expression multiplicative_expr term term_fork var
%type <non_term_type> bool_expr relation_and_expr relation_expr relation_expr_fork
%type <string> term_loop_1 comma_expression
%type <list<non_term_type>> comma_var
%type <int> number


%start program_start

%%

program_start: program 
             {
                if (MY_FUNC.find("main") == MY_FUNC.end()) {
                   FLAG_ERROR = true;
                   cout << "ERROR: main function not declared\n";
                }
                if (!FLAG_ERROR) {
                   cout << $1 << endl;
                }
             }
             ;

program: {$$ = "";}
       | program {MY_IDS.clear();} function 
       {
          $$ = $1 + $3;
       }
       ;

// function
function: FUNCTION identifier {
           if (RESERVED.find($2) != RESERVED.end()) {
              FLAG_ERROR = true;
              cout << "ERROR at " << @1 << " can't use reserved words for declaration\n";
           }
           else if (MY_FUNC.find($2) != MY_FUNC.end()) {
              FLAG_ERROR = true;
              cout << "ERROR at " << @1 << " function name redeclaration\n";
           }
           else {
              MY_FUNC.insert($2);
           }
        } semicolon BEGIN_PARAMS declaration_semicolon END_PARAMS BEGIN_LOCALS declaration_semicolon END_LOCALS BEGIN_BODY statement_semicolon END_BODY 
        {
            $$ = "func " + $2 + "\n";
            $$ += $6.code;
            int i = 0;
            
            for (list<string>::iterator it = $6.ids.begin(); it != $6.ids.end(); it++) {
                $$ += "= " + *it + ", $" + to_string(i) + "\n";
                i++;
            }
            $$ += $9.code;
            $$ += $12;
            $$ += "endfunc\n";
        }
        ;

declaration_semicolon: {$$.code = ""; $$.ids = list<string>();}
                     | declaration_semicolon declaration semicolon 
                     {
                        $$.code = $1.code + $2.code;
                        $$.ids = $1.ids;
                        for (list<string>::iterator it = $2.ids.begin(); it != $2.ids.end(); it++) {
                            $$.ids.push_back(*it);
                        }
                     }
                     ;
// declaration
declaration: comma_identifier COLON INTEGER 
           {
                for (list<string>::iterator it = $1.begin(); it != $1.end(); it++) {
                    if (RESERVED.find(*it) != RESERVED.end()) {
                       FLAG_ERROR = true;
                       cout << "ERROR at " << @1 << " can't use reserved words for declaration\n";
                    }
                    else if (MY_FUNC.find(*it) != MY_FUNC.end()) {
                       FLAG_ERROR = true;
                       cout << "ERROR at " << @1 << " id used for function's name\n";
                    }
                    else if (MY_IDS.find(*it) != MY_IDS.end()) {
                       FLAG_ERROR = true;
                       cout << "ERROR at " << @1 << " variable name redeclaration\n";
                    }
                    else {
                       MY_IDS[*it] = 0;
                    }
                    $$.code += ". " + *it + "\n";
                    $$.ids.push_back(*it);
                }
           }
           | comma_identifier COLON ARRAY L_SQUARE_BRACKET number R_SQUARE_BRACKET OF INTEGER
           {
                for (list<string>::iterator it = $1.begin(); it != $1.end(); it++) {
                    if (RESERVED.find(*it) != RESERVED.end()) {
                       FLAG_ERROR = true;
                       cout << "ERROR at " << @1 << " can't use reserved words for declaration\n";
                    }
                    else if (MY_FUNC.find(*it) != MY_FUNC.end()) {
                       FLAG_ERROR = true;
                       cout << "ERROR at " << @1 << " id used for function's name\n";
                       
                    }
                    else if (MY_IDS.find(*it) != MY_IDS.end()) {
                       FLAG_ERROR = true;
                       cout << "ERROR at " << @1 << " variable name redeclaration\n";
                    }
                    else {
                       MY_IDS[*it] = 1;
                    }
                    if ($5 <= 0) {
                       FLAG_ERROR = true;
                       cout << "ERROR at " << @1 << " array size can not be negative\n";
                    }
                    $$.code += ".[] " + *it + ", " + to_string($5) + "\n";
                    $$.ids.push_back(*it);
                }
           }
           | comma_identifier COLON ARRAY L_SQUARE_BRACKET number R_SQUARE_BRACKET L_SQUARE_BRACKET number R_SQUARE_BRACKET OF INTEGER
           {
                for (list<string>::iterator it = $1.begin(); it != $1.end(); it++) {
                    if (RESERVED.find(*it) != RESERVED.end()) {
                       FLAG_ERROR = true;
                       cout << "ERROR at " << @1 << " can't use reserved words for declaration\n";
                    }
                    else if (MY_FUNC.find(*it) != MY_FUNC.end()) {
                       FLAG_ERROR = true;
                       cout << "ERROR at " << @1 << " id used for function's name\n";
                    }
                    else if (MY_IDS.find(*it) != MY_IDS.end()) {
                       FLAG_ERROR = true;
                       cout << "ERROR at " << @1 << " variable name redeclaration\n";
                    }
                    else {
                       MY_IDS[*it] = 2;
                    }
                    if ($5 <= 0 || $8 <= 0) {
                       FLAG_ERROR = true;
                       cout << "ERROR at " << @1 << " array size can not be negative\n";
                    }
                    $$.code += ".[] " + *it + to_string($5 * $8) + "\n";
                    $$.ids.push_back(*it);
                    ID_DIM[*it] = to_string($8);
                }
           }
           ;

comma_identifier: identifier {$$.push_back($1);}
                | comma_identifier COMMA identifier
                {
                    $$ = $1;
                    $$.push_back($3);
                }
                ;

statement_semicolon: statement semicolon {$$ = $1;}
                   | statement_semicolon statement semicolon
                   {
                        $$ = $1 + $2;
                   }
                   ;
// statement
statement: var assign expression 
         {
            $$ = $1.code + $3.code;
            if ($1.array) {
               $$ += "[]= " + $1.id + ", " + $1.ind + ", " + $3.id + "\n";
            }
            else {
               $$ += "= " + $1.id + ", " + $3.id + "\n";
            }
         }
         | IF bool_expr THEN statement_semicolon else_loop ENDIF 
         {
            $$ = $2.code;
            string lab_1 = create_temp_label();
            string lab_2 = create_temp_label();
            string tempo = create_temp_var();
            $$ += ". " + tempo + "\n";
            $$ += "! " + tempo + ", " + $2.id + "\n";
            $$ += "?:= " + lab_1 + ", " + tempo + "\n";
            $$ += $4;
            $$ += ":= " + lab_2 + "\n";
            $$ += ": " + lab_1 + "\n";
            $$ += $5;
            $$ += ": " + lab_2 + "\n";
         }
         | {MY_LOOP.push(create_temp_label());} all_loops {MY_LOOP.pop();}
            {$$ = $2;}
         | READ comma_var 
         {
            for (list<non_term_type>::iterator it = $2.begin(); it != $2.end(); it++) {
                if (it->array) {
                    $$ += ".[]< " + it->id + ", " + it->ind + "\n"; 
                }
                else {
                    $$ += ".< " + it->id + "\n";
                }
            }
         }
         | WRITE comma_var 
         {
            for (list<non_term_type>::iterator it = $2.begin(); it != $2.end(); it++) {
                if (it->array) {
                    $$ += ".[]> " + it->id + ", " + it->ind + "\n"; 
                }
                else {
                    $$ += ".> " + it->id + "\n";
                }
            }
         }
         | CONTINUE 
         {
            if (MY_LOOP.empty()){
               FLAG_ERROR = true;
               cout << "ERROR at " << @1 << " no loop for this command\n";
            }
            else {
               $$ = ":= " + MY_LOOP.top() + "\n";
            }
         }
         | RETURN expression 
         {
            $$ = $2.code;
            $$ += "ret " + $2.id + "\n";
         }
         ;

else_loop: {$$ = "";}
         | ELSE statement_semicolon {$$ = $2;}
         ;

all_loops: WHILE bool_expr BEGINLOOP statement_semicolon ENDLOOP    
         {
            string lab_begin = MY_LOOP.top();
            string lab_end = create_temp_label();
            $$ = ": " + lab_begin + "\n";
            $$ += $2.code;
            string tempo = create_temp_var();
            $$ += ". " + tempo + "\n";
            $$ += "! " + tempo + ", " + $2.id + "\n";
            $$ += "?:= " + lab_end + ", " + tempo + "\n";
            $$ += $4;
            $$ += ":= " + lab_begin + "\n";
            $$ += ": " + lab_end + "\n";
         }
         | DO BEGINLOOP statement_semicolon ENDLOOP WHILE bool_expr
         {
            string lab_begin = MY_LOOP.top();
            $$ = ": " + lab_begin + "\n";
            $$ += $3;
            $$ += $6.code;
            $$ += "?:= " + lab_begin + ", " + $6.id + "\n";
         }
         | FOR var assign number semicolon bool_expr semicolon var assign expression BEGINLOOP statement_semicolon ENDLOOP
         {
            $$ = $2.code;
            if ($2.array) {
               $$ += "[]= " + $2.id + ", " + $2.ind + ", " + to_string($4) + "\n";
            }
            else {
               $$ += "= " + $2.id + ", " + to_string($4) + "\n";
            }
            string lab_begin = MY_LOOP.top();
            string lab_end = create_temp_label();
            $$ = ": " + lab_begin + "\n";
            $$ += $6.code;
            string tempo = create_temp_var();
            $$ += ". " + tempo + "\n";
            $$ += "! " + tempo + ", " + $6.id + "\n";
            $$ += "?:= " + lab_end + ", " + tempo + "\n";
            $$ += $12;
            $$ += $8.code + $10.code;
            if ($8.array) {
               $$ += "[]= " + $8.id + ", " + $8.ind + ", " + $10.id + "\n";
            }
            else {
               $$ += "= " + $8.id + ", " + $10.id + "\n";
            }
            $$ += ":= " + lab_begin + "\n";
            $$ += ": " + lab_end + "\n";
         }

comma_var: var {$$.push_back($1);}
         | comma_var COMMA var 
         {
            $$ = $1;
            $$.push_back($3);
         }
         ;

// bool_expr
bool_expr: relation_and_expr {$$.id = $1.id; $$.code = $1.code;}
         | bool_expr OR relation_and_expr
         {
            $$.code = $1.code + $3.code;
            string tempo = create_temp_var();
            $$.code += ". " + tempo + "\n";
            $$.code += "|| " + tempo + ", " + $1.id + ", " + $3.id + "\n";
            $$.id = tempo;
         }
         ;

// relation_and_expr
relation_and_expr: relation_expr {$$.id = $1.id; $$.code = $1.code;}
                 | relation_and_expr AND relation_expr 
                 {
                    $$.code = $1.code + $3.code;
                    string tempo = create_temp_var();
                    $$.code += ". " + tempo + "\n";
                    $$.code += "&& " + tempo + ", " + $1.id + ", " + $3.id + "\n";
                    $$.id = tempo;
                 }
                 ;

// relation_expr
relation_expr: NOT relation_expr_fork 
             {
                $$.code = $2.code;
                string tempo = create_temp_var();
                $$.code += ". " + tempo + "\n";
                $$.code += "! " + tempo + ", " + $2.id + "\n";
                $$.id = tempo;
             }
             | relation_expr_fork {$$.id = $1.id; $$.code = $1.code;}
             ;

relation_expr_fork: expression comp expression 
                  {
                     $$.code = $1.code + $3.code;
                     string tempo = create_temp_var();
                     $$.code += ". " + tempo + "\n";
                     $$.code += $2 + tempo + ", " + $1.id + ", " + $3.id + "\n";
                     $$.id = tempo;
                  }
                  | TRUE {$$.id = "1"; $$.code = "";}
                  | FALSE {$$.id = "0"; $$.code = "";}
                  | L_PAREN bool_expr R_PAREN 
                  {
                     $$.code = $2.code;
                     $$.id = $2.id;
                  }
                  ;

// comp
comp: EQ {$$ = "== ";}
    | NEQ {$$ = "!= ";}
    | LT {$$ = "< ";}
    | GT {$$ = "> ";}
    | LTE {$$ = "<= ";}
    | GTE {$$ = ">= ";}
    ;

// expression
expression: multiplicative_expr {$$.id = $1.id; $$.code = $1.code;}
          | expression SUB multiplicative_expr 
          {
             $$.code = $1.code + $3.code;
             string tempo = create_temp_var();
             $$.code += ". " + tempo + "\n";
             $$.code += "- " + tempo + ", " + $1.id + ", " + $3.id + "\n";
             $$.id = tempo;
          }
          | expression ADD multiplicative_expr
          {
             $$.code = $1.code + $3.code;
             string tempo = create_temp_var();
             $$.code += ". " + tempo + "\n";
             $$.code += "+ " + tempo + ", " + $1.id + ", " + $3.id + "\n";
             $$.id = tempo;
          }
          ;

// multiplicative_expr
multiplicative_expr: term {$$.id = $1.id; $$.code = $1.code;}
                   | multiplicative_expr MULT term 
                   {
                      $$.code = $1.code + $3.code;
                      string tempo = create_temp_var();
                      $$.code += ". " + tempo + "\n";
                      $$.code += "* " + tempo + ", " + $1.id + ", " + $3.id + "\n";
                      $$.id = tempo;
                   }
                   | multiplicative_expr DIV term 
                   {
                      $$.code = $1.code + $3.code;
                      string tempo = create_temp_var();
                      $$.code += ". " + tempo + "\n";
                      $$.code += "/ " + tempo + ", " + $1.id + ", " + $3.id + "\n";
                      $$.id = tempo;
                   }
                   | multiplicative_expr MOD term 
                   {
                      $$.code = $1.code + $3.code;
                      string tempo = create_temp_var();
                      $$.code += ". " + tempo + "\n";
                      $$.code += "% " + tempo + ", " + $1.id + ", " + $3.id + "\n";
                      $$.id = tempo;
                   }
                   ;


// term
term: term_fork {$$.code = $1.code; $$.id = $1.id;}
    | SUB term_fork %prec UMINUS 
    {
       $$.code = $2.code;
       string tempo = create_temp_var();
       $$.code += ". " + tempo + "\n";
       $$.code += "- " + tempo + ", 0, " + $2.id + "\n";
       $$.id = tempo;
    }
    | identifier L_PAREN term_loop_1 R_PAREN 
    {
       if (MY_FUNC.find($1) == MY_FUNC.end()) {
          FLAG_ERROR = true;
          cout << "ERROR at " << @1 << " function name undeclared\n";
       }
       $$.code = $3;
       string tempo = create_temp_var();
       $$.code += ". " + tempo + "\n";
       $$.code += "call " + $1 + ", " + tempo + "\n";
       $$.id = tempo;
    }
    ;

term_fork: var 
         {
            $$.code = $1.code;
            if ($1.array) {
               $$.id = $1.value;
            }
            else {
               $$.id = $1.id;
            }
         }
         | number {$$.code = ""; $$.id = to_string($1);}
         | L_PAREN expression R_PAREN {$$.code = $2.code; $$.id = $2.id;}
         ;

term_loop_1: {$$ = "";}
           | comma_expression {$$ = $1;}
           ;

comma_expression: expression {$$ = $1.code + "param " + $1.id + "\n";}
             | comma_expression COMMA expression 
             {
                $$ = $1 + $3.code + "param " + $3.id + "\n";
             }
             ;

// var
var: identifier 
   {
      if (MY_IDS.find($1) == MY_IDS.end()) {
         FLAG_ERROR = true;
         cout << "ERROR at " << @1 << " variable name undeclared\n";
      }
      else if (MY_IDS[$1] == 1) {
         FLAG_ERROR = true;
         cout << "ERROR at" << @1 << " should be 1d array\n";
      }
      else if (MY_IDS[$1] == 2) {
         FLAG_ERROR = true;
         cout << "ERROR at" << @1 << " should be 2d array\n";
      }
      $$.id = $1;
      $$.code = "";
      $$.array = false;
   }
   | identifier L_SQUARE_BRACKET expression R_SQUARE_BRACKET
   {
      if (MY_IDS.find($1) == MY_IDS.end()) {
         FLAG_ERROR = true;
         cout << "ERROR at " << @1 << " variable name undeclared\n";
      }
      else if (MY_IDS[$1] == 0) {
         FLAG_ERROR = true;
         cout << "ERROR at " << @1 << " should not be an array\n";
      }
      else if (MY_IDS[$1] == 2) {
         FLAG_ERROR = true;
         cout << "ERROR at" << @1 << " should be 2d array\n";
      }
      $$.id = $1;
      $$.code = $3.code;
      $$.array = true;
      $$.ind = $3.id;
      string tempo = create_temp_var();
      $$.code += ". " + tempo + "\n";
      $$.code += "=[] " + tempo + ", " + $$.id + ", " + $$.ind + "\n";
      $$.value = tempo;
   }
   | identifier L_SQUARE_BRACKET expression R_SQUARE_BRACKET L_SQUARE_BRACKET expression R_SQUARE_BRACKET 
   {
      if (MY_IDS.find($1) == MY_IDS.end()) {
         FLAG_ERROR = true;
         cout << "ERROR at " << @1 << " variable name undeclared\n";
      }
      else if (MY_IDS[$1] == 0) {
         FLAG_ERROR = true;
         cout << "ERROR at " << @1 << " should not be an array\n";
      }
      else if (MY_IDS[$1] == 1) {
         FLAG_ERROR = true;
         cout << "ERROR at" << @1 << " should be 1d array\n";
      }
      $$.id = $1;
      $$.code = $3.code + $6.code;
      $$.array = true;
      string tempo_1 = create_temp_var();
      $$.code += ". " + tempo_1 + "\n";
      $$.code += "* " + tempo_1 + ", " + $3.id + ", " + ID_DIM[$1] + "\n";
      string tempo_2 = create_temp_var();
      $$.code += ". " + tempo_2 + "\n";
      $$.code += "+ " + tempo_2 + ", " + tempo_1 + ", " + $6.id + "\n";
      $$.ind = tempo_2;
      string tempo_3 = create_temp_var();
      $$.code += ". " + tempo_3 + "\n";
      $$.code += "=[] " + tempo_3 + ", " + $$.id + ", " + $$.ind + "\n";
      $$.value = tempo_3;
   }
   ;

// semicolon
semicolon: SEMICOLON {}
         ;

// assign
assign: ASSIGN {}
      ;

// comma
//comma: COMMA {printf("comma -> COMMA\n");}
//     | {my_flag = 0; printf("ERROR: in line %d, position %d: unexpected item %s do you mean ,?\n", line, position - yyleng, token_text);} error
//     ;


identifier: IDENT {$$ = $1;}
          ;

number: NUMBER {$$ = $1;}
      ;

%%

int main(int argc, char** argv)
{
    yy::parser p;
    return p.parse();
}

void yy::parser::error(const yy::location& l, const std::string& m)
{
    std::cerr << l << ": " << m << std::endl;
}

string create_temp_var()
{
   static int i = 0;
   return "temp_" + to_string(i++);
}

string create_temp_label()
{
   static int i = 0;
   return "label_" + to_string(i++);
}
