/*
 * LALR grammar for Cafe
 * Cafe is a very minimal programming language suitable as the target of a
 * very high level language like Star, April or Go!
 */
%{
#include <math.h>
#include <ooio.h>
#include <io.h>
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
%token COLON ASSIGN ARROW THINARROW EQUAL
%token LPAR RPAR
%token LBRCE RBRCE
%token ANON

 // Keywords
%token PROCEDURE FUNCTION PATTERN MEMO 
%token TYPE OR 
%token LET SWITCH DEFAULT IN VALOF VALIS IMPORT
%token VAR IS 
%token NOTHING DO WHILE IF THEN ELSE 
%token TRY CATCH THROW
%token MATCHES
%token ASSERT

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
%right ARROW
%left LEQ
%left UMINUS BITNEG

%type <s> topLevel 
%type <a> expression atom name arith ident literal pattern 
%type <a> apply
%type <a> letExp switchExp switchAction valofExp 
%type <a> condition
%type <a> definition switchExpCase switchActCase
%type <a> isDeclaration varDeclaration 
%type <a> action block 
%type <a> conSpec
%type <a> type typeVar
%type <s> types 

%type <s> definitions switchActCases switchExpCases 
%type <s> actions idents conSpecs args ariths

%type <str> path

%{
  static void yyerror(YYLTYPE *loc,ioPo yyFile,lxPo *l, char const *errmsg);
  extern int yylex (YYSTYPE * yylval_param,YYLTYPE * yylloc_param, ioPo yyFile);

  static sxPo negative(locationPo loc,sxPo neg);

  #define locOf(yyloc) \
    newLocation(fileName(yyInfile),yyloc.first_line,yyloc.last_line)
  %}
%%

topLevel: definitions { *result = $1; }

definitions: {$$ = nil; }
| definition { $$ = mCons($1,nil); }
| definition SEMI definitions { $$ = mCons($1,$3); }
;

definition: 
  isDeclaration { $$=$1; }
| FUNCTION ID args COLON type ARROW expression { $$ = sxFunction(locOf(@$),$2,$5,$3,$7); }
| PROCEDURE ID args action { $$ = sxProcedure(locOf(@$),$2,$3,$4); }
| PATTERN ID args COLON type MATCHES pattern { $$ = sxPattern(locOf(@$),$2,$3,$5,$7,Null); }
| PATTERN ID args COLON type MATCHES pattern IF condition { $$ = sxPattern(locOf(@$),$2,$3,$5,$7,$9); }
| MEMO ID COLON type IS expression { $$ = sxMemo(locOf(@$),$2,$4,$6); }
| IMPORT path { $$ = sxImport(locOf(@$),$2); }
| TYPE type IS conSpecs { $$ = sxTypeDef(locOf(@$),$2,$4); }
;

path: STRING;

varDeclaration: 
VAR pattern ASSIGN expression { $$ = sxVarDeclaration(locOf(@$),$2,$4); }

isDeclaration: 
VAR pattern IS expression { $$ = sxIsDeclaration(locOf(@$),$2,$4); }

args: LPAR idents RPAR { $$ = $2; }

expression: arith
| apply
| letExp
| switchExp
| valofExp

atom: name
| literal
| LPAR expression COLON type RPAR { $$ = sxTypedExp(locOf(@$),$2,$4); }

arith: atom 
| arith PLUS arith { $$ = sxBinary(locOf(@$),AddOp,$1,$3); }
| arith MINUS arith { $$ = sxBinary(locOf(@$),SubtractOp,$1,$3); }
| arith TIMES arith { $$ = sxBinary(locOf(@$),TimesOp,$1,$3); }
| arith DIVIDE arith { $$ = sxBinary(locOf(@$),DivideOp,$1,$3); }
| arith PRCENT arith { $$ = sxBinary(locOf(@$),RemainderOp,$1,$3); }
| arith LSHIFT arith { $$ = sxBinary(locOf(@$),LshiftOp,$1,$3); }
| arith RSHIFT arith { $$ = sxBinary(locOf(@$),RshiftOp,$1,$3); }
| arith BITAND arith { $$ = sxBinary(locOf(@$),BitAndOp,$1,$3); }
| arith BITOR arith { $$ = sxBinary(locOf(@$),BitOrOp,$1,$3); }
| arith BITXOR arith { $$ = sxBinary(locOf(@$),BitXorOp,$1,$3); }
| BITNEG arith { $$ = sxUnary(locOf(@$),BitNegOp,$2); } %prec UMINUS
| MINUS arith { $$ = negative(locOf(@$),$2); } %prec UMINUS

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

apply: ID LPAR ariths RPAR { $$ = sxCall(locOf(@$),$1,$3); };
| ID LBRCE ariths RBRCE { $$ = sxConstructor(locOf(@$),$1,$3); }

letExp: LET LBRCE definitions RBRCE IN expression 
{ $$ = sxLet(locOf(@$),$3,$6); }

switchExp: SWITCH arith IN LBRCE switchExpCases RBRCE
{ $$ = sxSwitch(locOf(@$),$2,$5); }

switchExpCases: { $$ = nil; }
| switchExpCase { $$ = mCons($1,nil); }
| switchExpCase SEMI switchExpCases { $$=mCons($1,$3); }

switchExpCase: pattern THINARROW expression { $$=sxCaseRule(locOf(@$),$1,$3);}
| DEFAULT expression { $$=sxDefaultRule(locOf(@$),$2); }

switchAction: SWITCH arith IN LBRCE switchActCases RBRCE 
{ $$ = sxSwitch(locOf(@$),$2,$5); }

switchActCases: { $$ = nil; }
| switchActCase { $$ = mCons($1,nil); }
| switchActCase SEMI switchActCases { $$=mCons($1,$3); }

switchActCase: pattern THINARROW action { $$ = sxCaseRule(locOf(@$),$1,$3); }
| DEFAULT action { $$ = sxDefaultRule(locOf(@$),$2); }

valofExp: VALOF block { $$ = sxValof(locOf(@$),$2); }

actions: { $$ = nil; }
| action { $$ = mCons($1,nil); }
| action SEMI actions { $$ = mCons($1,$3); }

action: block
| apply
| IF condition THEN action ELSE action { $$=sxConditional(locOf(@$),$2,$4,$6); }
| isDeclaration
| varDeclaration
| name ASSIGN expression { $$ = sxAssignment(locOf(@$),$1,$3); }
| switchAction
| VALIS expression { $$ = sxValis(locOf(@$),$2); }
| WHILE condition DO block { $$ = sxWhileAction(locOf(@$),$2,$4); }
| TRY action CATCH LBRCE switchActCases RBRCE { $$ = sxCatch(locOf(@$),$2,$5); }
| THROW expression { $$ = sxThrowAction(locOf(@$),$2); }
| NOTHING { $$ = sxNothing(locOf(@$)); }
| LET LBRCE definitions RBRCE IN action { $$ = sxLet(locOf(@$),$3,$6); }
| ASSERT condition { $$=sxAssert(locOf(@$),$2); }
;

block: LBRCE actions RBRCE { $$ = sxBlock(locOf(@$),$2); }

condition: atom
| arith EQUAL arith { $$ = sxBinary(locOf(@$),EqualName,$1,$3); }
| arith NOTEQUAL arith { $$ = sxBinary(locOf(@$),NotEqualName,$1,$3); }
| arith LESS arith { $$ = sxBinary(locOf(@$),LessName,$1,$3); }
| arith LEQ arith { $$ = sxBinary(locOf(@$),LessEqualName,$1,$3); }
| arith GT arith { $$ = sxBinary(locOf(@$),GreaterName,$1,$3); }
| arith GEQ arith { $$ = sxBinary(locOf(@$),GreaterEqualName,$3,$1); }

idents: { $$ = nil; }
| ident { $$ = mCons($1,nil); }
| ident COMMA idents { $$ = mCons($1,$3); } 

ident: ID COLON type { $$ = sxIdent(locOf(@$),$1,$3); }
| ANON COLON type { $$ = sxIdent(locOf(@$),ANONYMOUS,$3); }

name: ID { $$ = mId(locOf(@$),$1); }

conSpecs:  { $$ = nil; }
| conSpec { $$ = mCons($1,nil); }
| conSpec OR conSpecs { $$ = mCons($1,$3); } 
;

conSpec: ID LBRCE types RBRCE { $$ = sxConstructor(locOf(@$),$1,$3); }
| ID { $$ = mId(locOf(@$),$1); }

type: typeVar
| ID { $$ = mId(locOf(@$),$1); }
| ID LESS types GT { $$ = sxTypeExp(locOf(@$),$1,$3); }
| LPAR types RPAR ARROW type { $$ = sxArrowType(locOf(@$),$2,$5); }
| type LEQ LPAR types RPAR { $$ = sxPttrnType(locOf(@$),$4,$1); }
;

typeVar: PRCENT ID { $$ = sxTypeVar(locOf(@$),$2); }

types: { $$ = nil; }
| type { $$ = mCons($1,nil); }
| type COMMA types { $$ = mCons($1,$3); }

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

    

