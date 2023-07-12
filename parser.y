%{
#include <stdio.h>
#include <stdlib.h>

#include <iostream>

#include <string>
using namespace std;



#include "lexer.hpp"




#include "ast.hpp"


%}





%token T_and "and"
%token T_char "char"
%token T_div "div"
%token T_do "do"
%token T_else "else"
%token T_fun "fun"
%token T_if "if"
%token T_int "int"
%token T_mod "mod"
%token T_not "not"
%token T_nothing "nothing"
%token T_or "or"
%token T_ref "ref"
%token T_return "return"
%token T_then "then"
%token T_var "var"
%token T_while "while"
%token T_smallerequal "<="
%token T_largerequal ">="
%token T_assigment "<-"



%token<var> T_id
%token<var> T_const
%token<num> T_const_num
%token<character> T_const_char
%token<var> T_const_str



%left<var> '*' "div" "mod"
%left<var> '+' '-'
%nonassoc<var> '<' "<=" '>' ">=" '=' '#'
%nonassoc<var> "not"
%left<var> "and"
%left<var> "or"

%expect 2 /* ??? wrong ?*/

%union {


  Cond *cond;
  Expr *expr;
  Lvalue *lval;
  FuncCall *funccall;
  ExprList *expr_list;
  StmtList *stmt_list;
  Stmt *stmt;
  VarDec *vardec;
  FparType *fpar_type;
  ArraySize *array_size;
  FparDef *fpar_def;
  IdList *id_list;
  Ref *ref;
  FunctionHeader *func_header;
  FparDefList *fpar_def_list;
  FunctionDef *func_def;

  TypeDef *type_def;


  bool boolean;
  Type type;
  LocalDefList *local_def_list;

  std::string *var;
  char character;
  int num;


}



%type<cond> cond
%type<expr> expr expr_optional
%type<lval> l-value
%type<funccall> func-call
%type<expr_list> expr_list expr_list_optional
%type<stmt_list> stmt_list block
%type<stmt> stmt local-def /*maybe smth better for local-def*/
%type<vardec> var-def
%type<fpar_type> fpar-type
%type<boolean> array-size-empty
%type<type> ret-type data-type

%type<type_def> type

%type<array_size> array-size
%type<fpar_def> fpar-def
%type<id_list> id_list
%type<ref> ref_optional
%type<func_header> header func-decl
%type<fpar_def_list> fpar-def_list fpar-def_helper
%type<func_def> func-def
%type<local_def_list> local-def_list

%type<var> any-compare-operator





%%

program : func-def  { std::cout << "AST: " << *$1 << std::endl; }
;

local-def_list :
/* nothing */ { $$ = new LocalDefList(); }
| local-def_list local-def  { $1->add($2); $$ = $1; }
;

func-def : header local-def_list block  { $$ = new FunctionDef($1,$2,$3);}
;

fpar-def_helper :
/*nothing */ { $$ = new FparDefList(); }
| fpar-def_helper ';' fpar-def { $1->add($3); $$ = $1; }
;


fpar-def_list :
/*nothing */ { $$ = new FparDefList(); }
|  fpar-def fpar-def_helper { $2->add($1); $$ = $2; }
;



header : "fun" T_id '(' fpar-def_list  ')' ':' ret-type { $$ = new FunctionHeader($2,$4,$7); }
;

ref_optional :
/*nothing */ { $$ = new Ref(false); }
| "ref" { $$ = new Ref(true); }
;

id_list :
/*nothing */ { $$ = new IdList(); }
| id_list ',' T_id { $1->add(new Id($3)); $$ = $1; }
;

//func def
fpar-def : ref_optional T_id id_list ':' fpar-type { $$ = new FparDef($1,$2,$3,$5); }
;

data-type :
 "int" { $$ = TYPE_int;}
| "char" { $$ = TYPE_char;}
;

array-size:
/*nothing*/ { $$ = new ArraySize(); }
| array-size '[' T_const_num ']' { $1->add($3); $$ = $1;}
;


type : data-type array-size { $$ = new TypeDef($1,$2); }
;

ret-type :
  data-type {$$ = $1;}
| "nothing" { $$ = TYPE_nothing; }
;

array-size-empty:
/*nothing*/ { $$ = false; }
| '[' ']' { $$ = true; }
;

fpar-type : data-type array-size-empty array-size { $$ = new FparType($1,$2,$3); }
;

local-def : 
  func-def { $$ = $1; }
| func-decl { $$ = $1; }
| var-def { $$ = $1; }
;

func-decl : header ';' { $$ = $1; } 
;

var-def : "var" T_id id_list ':' type ';' { $3->add(new Id($2)); $$ = new VarDec($3,$5); }
;



expr_optional :
/*nothing */ { $$ = new Expr(); }
| expr { $$ = $1; }
;

stmt :
 ';' { $$ = new Stmt(); }
| l-value "<-" expr ';' { $$ = new Assign($1,$3); }
| block { $$ = $1; }
| func-call ';' { $$ = $1; }
| "if" cond "then" stmt "else" stmt { $$ = new If($2,$4,$6); }
| "if" cond "then" stmt { $$ = new If($2,$4); }
| "while" cond "do" stmt { $$ = new While($2,$4); }
| "return" expr_optional ';' { $$ = new Return($2); }
;

stmt_list :
/*nothing */ { $$ = new StmtList(); }
| stmt_list stmt  { $1->add($2); $$ = $1; }
;

block : '{' stmt_list '}' { $$ = $2; }
;

expr_list:
/*nothing */ { $$ = new ExprList(); }
| expr_list ',' expr { $1->add($3); $$ = $1;  }
;

expr_list_optional :
/*nothing */ { $$ = new ExprList(); }
| expr expr_list { $2->add($1); $$ = $2; }
;


func-call : T_id '(' expr_list_optional ')' { $$ = new FuncCall($1, $3); }
;

l-value :
  T_id { $$ = new IdLval($1); }
| T_const_str { $$ = new StringConst($1); }
| l-value '[' expr ']' { $$ = new ArrayElem($1, $3); }
;


any-operator: '+' | '-' | '*' | "div" | "mod"
;

expr :
  T_const_num { $$ = new IntConst($1); }
| T_const_char { $$ = new CharConst($1); }
| l-value { $$ = $1; }
| '(' expr ')' { $$ = $2; }
| func-call { $$ = $1; }
| '+' expr { $$ = new UnaryOp($2,$1); }
| '-' expr  { $$ = new UnaryOp($2,$1); }
| expr '+' expr { $$ = new BinOp($1,$2,$3); }
| expr '-' expr { $$ = new BinOp($1,$2,$3); }
| expr '*' expr { $$ = new BinOp($1,$2,$3); }
| expr "div" expr { $$ = new BinOp($1,$2,$3); }
| expr "mod" expr { $$ = new BinOp($1,$2,$3); }
;/*order!!*/



any-compare-operator : '=' | '#' | '<' | '>' | "<=" | ">="
;


cond :
'(' cond ')' { $$ = $2; }
| "not" cond { $$ = new UnaryOpCond($2,$1); }
| cond "and" cond {$$ = new BinOpCond($1,$2,$3);}
| cond "or" cond {$$ = new BinOpCond($1,$2,$3);}
| expr any-compare-operator expr { $$ = new CompareOp($1,$2,$3);}
;



%%


void yyerror(const char *msg) {
  printf("Syntax error: %s\n", msg);
  exit(42);
}

int main() {

  // #ifdef YYDEBUG
    yydebug = 0;
  // #endif

  int res = yyparse();
  if (res == 0) printf("Successful parsing\n");
  return res;
}