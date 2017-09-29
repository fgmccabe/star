/*
 * LALR grammar for Cafe
 * Cafe is a very minimal programming language suitable as the target of a
 * very high level language like Star, April or L-and-O
 */
%{
#include <math.h>
#include <ooio.h>
#include "compiler.h"
#include "meta.h"
#include "dict.h"
#include "arith.h"
#include "locationP.h"
%}

%locations
%pure_parser
%defines
%error-verbose

%debug

%parse-param { ioPo yyInfile }
%parse-param { sxPo *result }
%lex-param { ioPo yyInfile } 

%start topLevel

%union {
  char *str;
  char *id;
  char ch;
  integer i;
  double f;
  sxPo a;
  lxPo s;
 }

// Symbolic tokens
%token COMMA SEMI
%token PRCENT PERIOD
%token EQ COLON ASSIGN RARROW LARROW THINARROW
%token LPAR RPAR
%token LBRCE RBRCE
%token LANGLE RANGLE
%token ANON
%token IF THEN ELSE

 // Keywords
%token FUNCTION PATTERN MEMO IMPORT WHERE
%token TYPE STRUCT ENUM ALL ST
%token LET SWITCH DEFAULT IN 
%token VAR DEF IS
%token MATCHES OR NOT AND

// Number and value tokens
%token <i> DECIMAL LONG
%token <str> STRING
%token <id> ID
%token <f> FLOAT

%right SEMI COMMA ELSE
%nonassoc IN
%nonassoc LPAR
%nonassoc NOT
%left AND
%left OR
%right LARROW RARROW
%left ST

%type <a> topLevel package
%type <a> expression atom name arith ident literal pattern 
%type <a> apply field fieldExp
%type <a> letExp switchExp conditionalExp 
%type <a> condition sequence
%type <a> definition switchExpCase
%type <a> isDeclaration varDeclaration importSpec
%type <a> type typeVar
%type <s> types 

%type <s> definitions switchExpCases 
%type <s> names idents args ariths fields expressions

%type <str> path

%{
  static void yyerror(YYLTYPE *loc,ioPo yyFile,sxPo *l, char const *errmsg);
  extern int yylex (YYSTYPE * yylval_param,YYLTYPE * yylloc_param, ioPo yyFile);

  #define locOf(yyloc) \
    newLocation(fileName(yyInfile),yyloc.first_line,yyloc.last_line)
  %}
%%

topLevel: package { *result = $1;}

package: path LBRCE definitions RBRCE { $$ = sxPackage(locOf(@$),$1,$3); }

definitions: {$$ = nil; }
| definition { $$ = mCons($1,nil); }
| definition SEMI definitions { $$ = mCons($1,$3); }
;

definition: 
  isDeclaration { $$=$1; }
| varDeclaration { $$=$1; }
| FUNCTION ID args COLON type IS expression { $$ = sxFunction(locOf(@$),$2,$5,$3,$7); }
| PATTERN ID args COLON type MATCHES pattern { $$ = sxPattern(locOf(@$),$2,$3,$5,$7,Null); }
| PATTERN ID args COLON type MATCHES pattern WHERE condition { $$ = sxPattern(locOf(@$),$2,$3,$5,$7,$9); }
| MEMO ID COLON type IS expression { $$ = sxMemo(locOf(@$),$2,$4,$6); }
| importSpec { $$=$1; }
| TYPE type { $$ = sxTypeDef(locOf(@$),$2); }
| ENUM name COLON type { $$ = sxEnumDef(locOf(@$),$2,sxTupleType(locOf(@$),nil),$4); }
| ENUM name LPAR idents RPAR COLON type { $$ = sxEnumDef(locOf(@$),$2,sxTupleType(locOf(@$),$4),$7); }
| STRUCT name LBRCE idents RBRCE COLON type { $$ = sxStructDef(locOf(@$),$2,sxRecordType(locOf(@$),$4),$7); }
;

path: STRING;

importSpec: IMPORT path { $$ = sxImport(locOf(@$),$2); }

varDeclaration: 
VAR pattern ASSIGN expression { $$ = sxVarDeclaration(locOf(@$),$2,$4); }

isDeclaration: 
DEF pattern IS expression { $$ = sxIsDeclaration(locOf(@$),$2,$4); }

args: LPAR idents RPAR { $$ = $2; }

expression: arith
| apply
| fieldExp
| letExp
| switchExp
| conditionalExp
| sequence

expressions: { $$ = nil; }
| expression { $$ = mCons($1,nil); }
| expression COMMA expressions { $$ = mCons($1,$3); }

atom: name
| literal
| LPAR expression COLON type RPAR { $$ = sxTypedExp(locOf(@$),$2,$4); }

arith: atom 

ariths: { $$ = nil; }
| arith { $$ = mCons($1,nil); }
| arith COMMA ariths { $$ = mCons($1,$3); }

literal: STRING  {$$ = mStr(locOf(@$),$1); }
| DECIMAL { $$ = mInt(locOf(@$),$1); }
| LONG { $$ = mLong(locOf(@$),$1); }
| FLOAT { $$ = mFloat(locOf(@$),$1);}

pattern: ident 
| ID { $$ = mId(locOf(@$),$1); }
| ID LBRCE idents RBRCE { $$ = sxConstructor(locOf(@$),$1,$3); }
| literal;

fields: { $$ = nil; }
| field { $$ = mCons($1,nil); }
| field COMMA fields { $$ = mCons($1,$3); }
;

field: name EQ arith { $$ = sxField(locOf(@$),$1,$3); }
;

fieldExp: arith PERIOD ID { $$ = sxField(locOf(@$),$1, mId(locOf(@$),$3));}

apply: ID LPAR ariths RPAR { $$ = sxCall(locOf(@$),$1,$3); };
| ID LBRCE fields RBRCE { $$ = sxConstructor(locOf(@$),$1,$3); }

letExp: LET LBRCE definitions RBRCE IN expression 
{ $$ = sxLet(locOf(@$),$3,$6); }

switchExp: SWITCH arith IN LBRCE switchExpCases RBRCE
{ $$ = sxSwitch(locOf(@$),$2,$5); }

switchExpCases: { $$ = nil; }
| switchExpCase { $$ = mCons($1,nil); }
| switchExpCase SEMI switchExpCases { $$=mCons($1,$3); }

switchExpCase: pattern THINARROW expression { $$=sxCaseRule(locOf(@$),$1,$3);}
| DEFAULT expression { $$=sxDefaultRule(locOf(@$),$2); }

conditionalExp: IF condition THEN expression ELSE expression { $$ = sxConditional(locOf(@$),$2,$4,$6); }

condition: expression MATCHES pattern { $$ = sxMatches(locOf(@$),$1,$3); }
| expression { $$ = sxIsTrue(locOf(@$),$1); }
| condition OR condition { $$ = sxOr(locOf(@$),$1,$3); }
| condition AND condition { $$ = sxAnd(locOf(@$),$1,$3); }
| NOT condition { $$ = sxNot(locOf(@$),$2); }

sequence: LBRCE expressions RBRCE { $$ = sxSequence(locOf(@$),$2); }

idents: { $$ = nil; }
| ident { $$ = mCons($1,nil); }
| ident COMMA idents { $$ = mCons($1,$3); } 

ident: ID COLON type { $$ = sxIdent(locOf(@$),$1,$3); }
| ANON COLON type { $$ = sxIdent(locOf(@$),ANONYMOUS,$3); }

name: ID { $$ = mId(locOf(@$),$1); }

names: { $$ = nil; }
| name { $$ = mCons($1,nil); }
| name COMMA names { $$ = mCons($1,$3); } 

type: typeVar
| name { $$ = $1; }
| name LANGLE types RANGLE { $$ = sxTypeExp(locOf(@$),$1,sxTupleType(locOf(@$),$3)); }
| ALL names ST type { $$ = sxAllType(locOf(@$),$2,$4); }
| LPAR types RPAR { $$ = sxTupleType(locOf(@$),$2); }
| type RARROW type { $$ = sxArrowType(locOf(@$),$1,$3); }
| type LARROW type  { $$ = sxPttrnType(locOf(@$),$3,$1); }
| LBRCE idents RBRCE { $$ = sxRecordType(locOf(@$),$2); }
;

typeVar: PRCENT ID { $$ = sxTypeVar(locOf(@$),$2); }

types: { $$ = nil; }
| type { $$ = mCons($1,nil); }
| type COMMA types { $$ = mCons($1,$3); }

%%

static void yyerror(YYLTYPE *loc,ioPo yyFile,sxPo *a, char const *errmsg)
{
  LocationRec lc = {.fileName=fileName(yyFile),
		    .firstLine=loc->first_line,
		    .lastLine=loc->last_line};
  reportError(&lc,"syntax error: %s",errmsg);
}
