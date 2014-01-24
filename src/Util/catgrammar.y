/*
 * LALR grammar for catalog structure
 * Based on Cafe grammar, but much simplified
 */

%{
#include <math.h>
#include <ooio.h>
#include <io.h>
#include "catalogP.h"
%}

%locations
%pure_parser
%defines
%error-verbose

%debug

%parse-param { ioPo yyInfile }
%parse-param { lxPo *result }
%lex-param { ioPo yyInfile } 

%start topLevel

%union{
  uniChar *op;
  uniChar *str;
  uniChar *id;
  uniChar ch;
  integer i;
  double f;
  sxPo a;
  lxPo s;
 }

// Symbolic tokens
%token COMMA SEMI
%token PRCENT
%token COLON DCOLON ASSIGN ARROW THINARROW EQUAL
%token LPAR RPAR
%token LBRCE RBRCE
%token ANON

 // Keywords
%token CATALOG IS HASH
%token TYPE OR 
%token LET SWITCH DEFAULT IN VALOF VALIS IMPORT
%token VAR LEAVE IS 
%token NOTHING DO WHILE LOOP GOTO CONTINUE IF THEN ELSE SYNC
%token TRY CATCH THROW

 // Compare Operators
%token LESS LEQ GT GEQ NOTEQUAL

 // Arithmetic operators
%token PLUS MINUS TIMES DIVIDE LSHIFT RSHIFT UMINUS BITAND BITOR BITXOR BITNEG

// Number and value tokens
%token <i> DECIMAL LONG
%token <str> STRING
%token <id> ID
%token <f> FLOAT

%right SEMI COMMA ELSE
%nonassoc IN
%nonassoc LPAR
%left PLUS MINUS BITOR BITXOR
%left TIMES DIVIDE PRCENT BITAND 
%nonassoc LSHIFT RSHIFT
%left UMINUS BITNEG

%type <s> topLevel 
%type <a> expression atom name arith ident literal pattern 
%type <a> apply
%type <a> letExp switchExp switchAction valofExp 
%type <a> condition
%type <a> definition switchExpCase switchActCase
%type <a> isDeclaration varDeclaration 
%type <a> action block 
%type <a> typeSpec
%type <a> type typeVar
%type <s> types 

%type <s> definitions switchActCases switchExpCases 
%type <s> actions idents typeSpecs args ariths

%type <str> path

%{
  static void yyerror(YYLTYPE *loc,ioPo yyFile,lxPo *l, char const *errmsg);
  extern int yylex (YYSTYPE * yylval_param,YYLTYPE * yylloc_param, ioPo yyFile);

  static sxPo negative(locationPo loc,sxPo neg);

  #define locOf(yyloc) \
    newLocation(fileName(yyInfile),yyloc.first_line,yyloc.last_line)
  %}
%%

topLevel: catalog { *result = $1; }

catalog: ID IS CATALOG LBRCE catalogContents RBRCE { $$ = buildCatalog($1,$5); }

catalogContents: { $$=nil; }
| catEntry { $$ = mCons($1,nil); }
| catEntry SEMI catalogContents { $$ = mCons($1,$3); }
| catEntry SEMI { $$ = mCons($1,nil); }

catEntry: STRING THIN_ARROW STRING { $$ = catalogEntry($1,$3); }

%%

static void yyerror(YYLTYPE *loc,ioPo yyFile,lxPo *a, char const *errmsg)
{
  LocationRec lc = {.fileName=fileName(yyFile),
		    .firstLine=loc->first_line,
		    .lastLine=loc->last_line};
  reportError(&lc,"syntax error: %s",errmsg);
}

static sxPo negative(locationPo loc,sxPo neg)
{
  if(sxIsInt(neg))
    return mInt(loc,-sxInt(neg));
  else if(sxIsFloat(neg))
    return mFloat(loc,-sxFloat(neg));
  else
    return sxBinary(loc,SubtractOp,mInt(loc,0),neg);
}

    


