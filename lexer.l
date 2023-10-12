%{

#include "ast.hpp"

#include <string>
using namespace std;

#include "lexer.hpp"
#include "parser.hpp"


#define T_eof   0

%}

L [a-zA-Z]
D [0-9]
W [ \t\n\r]


HEX {L}|{D}


SPECIAL \\(n|t|r|0|\\|\'|\"|x{HEX}{2})


%option noyywrap
%option yylineno

%%



 /* λέξεις κλειδιά */

"and" {yylval.var = new std::string(yytext); return T_and; }
"char" {return T_char; }
"div" {yylval.var = new std::string(yytext); return T_div; }
"do" {return T_do; }
"else" {return T_else; }
"fun" {return T_fun; }
"if" {return T_if; }
"int" {return T_int; }
"mod" {yylval.var = new std::string(yytext);return T_mod; }
"not" {yylval.var = new std::string(yytext); return T_not; }
"nothing" {return T_nothing; }
"or" {yylval.var = new std::string(yytext); return T_or; }
"ref" {return T_ref; }
"return" {return T_return; }
"then" {return T_then; }
"var" {return T_var; }
"while" {return T_while; }

 /* ονοματα */
{L}({L}|{D}|_)*	{  
  yylval.var = new std::string(yytext);
 return T_id; }


 /* ακέραιες σταθερές */
{D}+	{ 
  yylval.num = atoi(yytext);
  return T_const_num; }

 /* σταθερούς χαρακτήρες */

'.'  { 
  yylval.character = yytext[1];
  return T_const_char; }

'{SPECIAL}'  { 
  yylval.var = new std::string(yytext + 1, strlen(yytext) - 2);
  return T_const_char_special; }


 /* σταθερές συμβολοσειρές 8eoro oti ginete k adeio ""  */
 /* \"({D}|{L}|{SPECIAL})*\" { return T_const_str; } */

 \"([^"]|\\\")*\" { 
  yylval.var = new std::string(yytext + 1, strlen(yytext) - 2);

  return T_const_str; }


[\=\+\-\*\#\<\>]	{ yylval.var = new std::string(yytext); return yytext[0]; }
"<=" { yylval.var = new std::string(yytext); return T_smallerequal; }
">=" {yylval.var = new std::string(yytext); return T_largerequal; }



[\(\)\[\]\{\}\,\;\:]	{ return yytext[0]; }
"<-" {return T_assigment; }



 /*white spaces */
{W}+	{ /* ignore */ }

 /*single line comment */
 "$"("\n"|[^\$"\n"].*) { /* nothing */ }

 /* multi line seems good?*/

"$$"([^$]+|$[^$])*"$$" { /* nothing */ }



.	{ fprintf(stderr, "Illegal character '%c' (ASCII: %d)\n", yytext[0], yytext[0]); exit(42); }

%%

/*
int main() {
  int token;
  do {
    token = yylex();
    printf("token = %d, lexeme = \"%s\"\n", token, yytext);
  } while(token != T_eof);
}
*/