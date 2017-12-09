/*
 * Grammar for Cafe assembler
 */

%{
#include <math.h>
#include <ooio.h>
#include "assem.h"
#include "errors.h"
%}

%locations
%pure-parser
%defines
%error-verbose

%debug

%parse-param { ioPo asmFile }
%parse-param { pkgPo pkg }
%lex-param { ioPo asmFile }

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
  static void yyerror(YYLTYPE *loc,ioPo asmFile,pkgPo p, char const *errmsg);
  extern int sslex (YYSTYPE * asmlval_param,YYLTYPE * asmlloc_param, ioPo asmFile);

  #define locOf(asmloc)							\
    newLocation(fileName(asmInfile),asmloc.first_line,asmloc.last_line)

  static mtdPo currMtd;

  #define YYLEX_PARAM asmFile
  #define yylex sslex
%}

// Assembler mnemonics
%token PUBLIC PKG IMPORT

%token HALT
%token CALL TAIL ENTER ESCAPE
%token RET JMP CASE
%token DROP DUP PULL ROT CAS

%token LD ST

%token A L
%token ALLOC

%token I2F F2I

%token ADDI ADDF LADD INCI
%token SUBI SUBF LSUB DECI
%token MULI MULF LMUL
%token DIVI DIVF LDIV
%token REMI REMF LREM

%token LFT LLFT
%token RGT ASR

%token CMPI LCMP CMPF

%token BZ BNZ BF BNF BLE BLT BGE BGT
%token FRAME LOCAL
%token END

%token COLON SLASH DCOLON LBRA RBRA HASH
%token NL

// Number and value tokens
%token <i> DECIMAL
%token <str> STRING
%token <id> ID
%token <f> FLOAT

%type <lbl> label, funLbl;
%type <i> libName;
%type <str> signature;
%type <i> literal;

%%

   program: package defs trailer;

   package: ;

   defs: defs function | ;

 function: header instructions trailer ;

 header: funLbl DCOLON signature nls { currMtd = defineMethod(pkg,False,$1,$3,$5); }
     | PUBLIC funLbl DCOLON signature nls { currMtd = defineMethod(pkg,True,$2,$4,$6); }

 instructions: instructions instruction nls
     | instructions error nls
     | instruction nls
     ;

trailer: END nls { endFunction(currMtd); }

 nls: nls NL | NL;

 instruction: halt
     | call
     | load
     | store
     | caseins
     | heap
     | convert
     | arith
     | cond
     | directive
     ;
 halt: HALT { AHalt(currMtd); };

 call: CALL { ACall(currMtd); }
   | ESCAPE libName { AEscape(currMtd,$2); }
   | TAIL { ATail(currMtd); }
   | ENTER DECIMAL { AEnter(currMtd,$2); }
   | RET DECIMAL { ARet(currMtd,$2); }
   | JMP label { AJmp(currMtd,$2); }
   ;

 literal: FLOAT { newFloatConstant(currMtd,$1); }
   | STRING { newStringConstant(currMtd,$1); }
   | ID COLON DECIMAL { newStrctConstant(currMtd,$1,$3); }
   | ID SLASH DECIMAL { newPrgConstant(currMtd,$1,$3); }
   ;

 load: LD DECIMAL { ALdI(currMtd,$2); }
   | LD literal { ALdC(currMtd,$2); }
   | LD A LBRA DECIMAL RBRA { ALdA(currMtd,$4); }
   | LD L LBRA DECIMAL RBRA { ALdL(currMtd,$4); }
   | DROP { ADrop(currMtd); }
   | DUP { ADup(currMtd); }
   | PULL DECIMAL { APull(currMtd,$2); }
   | ROT DECIMAL { ARot(currMtd,$2); }
   | LD LBRA DECIMAL RBRA { ANth(currMtd,$3); }
   ;

 store: ST L LBRA DECIMAL RBRA { AStL(currMtd, $4); }
   | ST LBRA DECIMAL RBRA { AStNth(currMtd,$3); }
   | CAS label { ACas(currMtd,$2); }
   ;

 caseins: CASE DECIMAL { ACase(currMtd,$2); };

 heap: ALLOC ID { AAlloc(currMtd,findMethod(currMtd,$2)); }
   ;

 convert: F2I { AF2i(currMtd); }
   | I2F { AI2f(currMtd); }
   ;

 arith: ADDI { AAddI(currMtd); }
   | ADDF { AAddF(currMtd); }
   | LADD { ALAdd(currMtd); }
   | SUBI { ASubI(currMtd); }
   | LSUB { ALSub(currMtd); }
   | SUBF { ASubF(currMtd); }
   | INCI { AIncI(currMtd); }
   | DECI { ADecI(currMtd); }
   | MULI { AMulI(currMtd); }
   | LMUL { ALMul(currMtd); }
   | MULF { AMulF(currMtd); }
   | DIVI { ADivI(currMtd); }
   | LDIV { ALDiv(currMtd); }
   | DIVF { ADivF(currMtd); }
   | REMI { ARemI(currMtd); }
   | LREM { ALRem(currMtd); }
   | REMF { ARemF(currMtd); }
   | LFT { ALft(currMtd); }
   | LLFT { ALLft(currMtd); }
   | ASR { AAsr(currMtd); }
   | RGT { ARgt(currMtd); }
   | CMPI { ACmpI(currMtd); }
   | LCMP { ALCmp(currMtd); }
   | CMPF { ACmpF(currMtd); }
   ;

 cond: BZ label { ABz(currMtd,$2); }
   | BNZ label { ABnz(currMtd,$2); }
   | BF label { ABf(currMtd,$2); }
   | BNF label { ABnf(currMtd,$2); }
   | BLT label { ABlt(currMtd,$2); }
   | BLE label { ABle(currMtd,$2); }
   | BGE label { ABge(currMtd,$2); }
   | BGT label { ABgt(currMtd,$2); }
   ;

 directive: label COLON { defineLbl(currMtd,$1); }
   | FRAME signature { defineFrame(currMtd,$2); }
   | ID LOCAL DECIMAL signature label label { defineLocal(currMtd,$1,$4,$3,$5,$6); }
   ;

 funLbl : ID SLASH DECIMAL { $$ = newProgLbl(currMtd$1,$3); }

 label: ID { $$ = newLbl(currMtd,$1); };

 libName: STRING { $$ = newEscapeConstant(currMtd,$1); }

 signature: STRING { $$ = $1; if(!validSignature($1)){
  yyerror(&yyloc,asmFile,pkg,"invalid signature");
 }
 }

%%

static void yyerror(YYLTYPE *loc,ioPo asmFile,pkgPo p, char const *errmsg)
{
  reportError(loc->first_line,"%s\n",errmsg);
}
