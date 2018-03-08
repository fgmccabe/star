/*
 * LALR grammar for json
 * Event-based parser -- calls out events when parsing. Allows client to avoid constructing structures
 */

%{
#include "unistr.h"
#include "retcode.h"
#include "logical.h"
#include "io.h"
#include "formio.h"
#include "file.h"
#include "jsonEvent.h"

// #define YY DEBUG 1
%}

%locations
%pure-parser
%defines
%error-verbose

%debug

%parse-param { ioPo yyInfile }
%parse-param { jsonCallBackPo cb }
%parse-param { void *client }

%lex-param { ioPo yyInfile }

%start topLevel

%union{
  char *str;
  double num;
 }

// Symbolic tokens
%token LBRCE RBRCE LBRA RBRA COMMA COLON

// Data tokens
%token TRUE FALSE NuLL

// Number and value tokens
%token <str> STRING
%token <num> NUM

%{
  static void yyerror(YYLTYPE *loc,ioPo yyFile,jsonCallBackPo l, void *client, char const *errmsg);
  extern int yylex (YYSTYPE * yylval_param,YYLTYPE * yylloc_param, ioPo yyFile);
%}

%%

topLevel: { cb->startJson(client); } json { cb->endJson(client); }

json: array
 | collection
 | number
 | string
 | trueVal
 | nullVal
 ;

array: LBRA { cb->startArray(client); } arrayEntries RBRA { cb->endArray(client); }

arrayEntries :
  | json
  | json COMMA arrayEntries
  ;

collection: LBRCE { cb->startCollection(client); } collectionEntries RBRCE { cb->endCollection(client); }

collectionEntries:
  | entry
  | entry COMMA collectionEntries
  ;

entry: STRING COLON { cb->startEntry($1,client); } json {cb->endEntry($1,client); } ;

number: NUM { cb->numEntry($1,client); } ;

string: STRING { cb->stringEntry($1,client); } ;

trueVal: TRUE { cb->logicalEntry(True,client); }
 | FALSE { cb->logicalEntry(False,client); }
 ;

nullVal: NuLL { cb->nullEntry(client); }

%%

static void yyerror(YYLTYPE *loc,ioPo yyFile,jsonCallBackPo cb, void *client,char const *errmsg)
{
  cb->errorEntry(errmsg,client);
}





