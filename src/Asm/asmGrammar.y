/*
 * Grammar for Star assembler
 */

%{
#include <math.h>
#include <ooio.h>
#include "asm.h"
#include "errors.h"
%}

/* %define api.prefix {ss} */

%locations
%pure-parser
%defines
%error-verbose

%debug

%parse-param { ioPo asmFile }
%parse-param { pkPo *pkg }
%lex-param { ioPo asmFile }

%define api.prefix {ss}

%start program

%union{
  char *op;
  char *str;
  char *id;
  integer i;
  double f;
  assemInsPo ins;
  lPo lbl;
 }

%{
  static void yyerror(YYLTYPE *loc,ioPo asmFile,pkPo *p, char const *errmsg);
  extern int sslex (YYSTYPE * asmlval_param,YYLTYPE * asmlloc_param, ioPo asmFile);

  #define locOf(asmloc)							\
    newLocation(fileName(asmInfile),asmloc.first_line,asmloc.last_line)

  static mtdPo currMtd;

  #define YYLEX_PARAM asmFile
  #define yylex sslex
%}

// Assembler mnemonics
%token PUBLIC PKG IMPORT GLOBAL

%token HALT ABORT
%token CALL OCALL TAIL OTAIL ESCAPE
%token RET JMP CASE
%token DROP DUP PULL ROT RST BF BT CLBL CMP

%token LD ST T

%token A L
%token ALLOC

%token FRAME LINE DCALL DOCALL DTAIL DOTAIL DRET LOCAL
%token END

%token COLON SLASH AT DCOLON LBRA RBRA HASH
%token NL

// Number and value tokens
%token <i> DECIMAL
%token <str> STRING
%token <id> ID
%token <f> FLOAT

%type <lbl> label;
%type <str> libName;
%type <str> signature;
%type <i> literal local;

%%

   program: nls package imports defs trailer;

   package: PKG ID HASH ID DCOLON signature nls { *pkg = newPkg($2,$4,$6); }
     | PKG ID DCOLON signature nls { *pkg = newPkg($2,defltPkgVersion(),$4); }

   imports: imports import | ;

   import: IMPORT ID HASH ID nls { addImport(*pkg,$2,$4,False); }
   | IMPORT ID nls { addImport(*pkg,$2,"*",False); }
   | PUBLIC IMPORT ID HASH ID nls { addImport(*pkg,$3,$5,True); }
   | PUBLIC IMPORT ID nls { addImport(*pkg,$3,"*",True); }

   defs: defs function | ;

 function: header instructions trailer ;

 header: ID SLASH DECIMAL DCOLON signature COLON DECIMAL nls { currMtd = defineMethod(*pkg,$1,$3,$7,$5); }

 instructions: instructions instruction nls
     | instructions error nls
     | instruction nls
     ;

trailer: END nls { endFunction(currMtd); }

 nls: nls NL | NL;

 instruction: halt
     | abort
     | call
     | load
     | store
     | caseins
     | heap
     | directive
     ;

 halt: HALT { AHalt(currMtd); };

 abort: ABORT { AAbort(currMtd); };

 call: CALL literal { ACall(currMtd,$2); }
   | OCALL DECIMAL { AOCall(currMtd,$2); }
   | ESCAPE libName { AEscape(currMtd,$2); }
   | TAIL literal { ATail(currMtd,$2); }
   | OTAIL DECIMAL { AOTail(currMtd,42); }
   | RET { ARet(currMtd); }
   | JMP label { AJmp(currMtd,$2); }
   | BF label { ABf(currMtd,$2); }
   | BT label {ABt(currMtd,$2); }
   | CMP label {ACmp(currMtd,$2);}
   | CLBL label { ACLbl(currMtd,$2); }
   ;

 literal: FLOAT { $$=newFloatConstant(currMtd,$1); }
   | STRING { $$=newStringConstant(currMtd,$1); }
   | DECIMAL { $$ = newIntegerConstant(currMtd,$1); }
   | ID COLON DECIMAL { $$=newStrctConstant(currMtd,$1,$3); }
   ;

 load: LD literal { ALdC(currMtd,$2); }
   | LD A LBRA DECIMAL RBRA { ALdA(currMtd,$4); }
   | LD L LBRA DECIMAL RBRA { ALdL(currMtd,$4); }
   | LD ID { ALdG(currMtd,$2); }
   | DROP { ADrop(currMtd); }
   | DUP { ADup(currMtd); }
   | RST DECIMAL { ARst(currMtd,$2); }
   | PULL DECIMAL { APull(currMtd,$2); }
   | ROT DECIMAL { ARot(currMtd,$2); }
   | LD LBRA DECIMAL RBRA { ANth(currMtd,$3); }
   ;

 store: ST L LBRA local RBRA { AStL(currMtd,$4); }
   | ST LBRA local RBRA { AStNth(currMtd,$3); }
   | ST ID { AStG(currMtd,$2); }
   | T L LBRA local RBRA { ATL(currMtd,$4); }
   ;

 local: ID {
    $$=findLocal(currMtd,$1);
    if($1<0){
      yyerror(&yylloc,asmFile,pkg,"local var not defined");
    }
  };

 caseins: CASE DECIMAL { ACase(currMtd,$2); } ;

 heap: ALLOC literal { AAlloc(currMtd,$2); }
   ;

 directive: label COLON { defineLbl(currMtd,$1); }
   | FRAME signature { AFrame(currMtd,newStringConstant(currMtd,$2)); }
   | LINE ID AT DECIMAL SLASH DECIMAL COLON DECIMAL
          { AdLine(currMtd,defineLocation(currMtd,$2,$4,$6,$8)); }
   | DCALL literal { AdCall(currMtd,$2); }
   | DOCALL DECIMAL { AdOCall(currMtd,$2); }
   | DTAIL literal { AdTail(currMtd,$2); }
   | DOTAIL DECIMAL { AdOTail(currMtd,$2); }
   | DRET { AdRet(currMtd); }
   | LOCAL ID signature label label { defineLocal(currMtd,$2,$3,$4,$5); }
   ;

 label: ID { $$ = newLbl(currMtd,$1); };

 libName: STRING { $$ = $1; }
   | ID { $$ = $1; }

 signature: STRING { $$ = $1; if(!validSignature($1)){
  yyerror(&yylloc,asmFile,pkg,"invalid signature");
 }
 }

%%

static void yyerror(YYLTYPE *loc,ioPo asmFile,pkPo *p, char const *errmsg)
{
  reportError(loc->first_line,"%s\n",errmsg);
}
