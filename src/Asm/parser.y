/*
 * Grammar for Cafe assembler
 */

%code top{
#include <math.h>
#include <ooio.h>
#include "assem.h"
#include "errors.h"
}

%locations
%pure-parser
// %glr-parser
%defines
%error-verbose

%debug

%parse-param { ioPo yyInfile }
%parse-param { pkgPo pkg }
%lex-param { ioPo yyInfile } 

%start program

%union{
  uniChar ch;
  uniChar *op;
  uniChar *str;
  uniChar *id;
  integer i;
  double f;
  assemInsPo ins;
  lPo lbl;
 }

%code {
  static void sserror(YYLTYPE *loc,ioPo asmFile,pkgPo p, char const *errmsg);
  extern int sslex (YYSTYPE * asmlval_param,YYLTYPE * asmlloc_param, ioPo asmFile);

  #define locOf(asmloc)							\
    newLocation(fileName(asmInfile),asmloc.first_line,asmloc.last_line)

  static mtdPo currMtd;
}

// Assembler mnemonics
%token HALT
%token CALL TAIL ENTER ESCAPE
%token RET JMP
%token CASE
%token LD ST POP SWAP DUP CAS
%token A L E
%token ALLOC 
%token I2F F2I
%token ADD ADDF SUB SUBF MUL MULF DIV DIVF REM INC DEC
%token LEFT RIGHT ASR
%token CMP CMPF 
%token BZ BNZ BLE BLT BGE BGT
%token FRAME LOCAL
%token END

%token COLON DCOLON LBRA RBRA
%token NL

// Number and value tokens
%token <ch> CHAR
%token <i> DECIMAL
%token <str> STRING
%token <id> ID
%token <f> FLOAT

%type <lbl> label;
%type <i> libName;
%type <str> signature;

%%

   program: program optnls function | ;

 function: header instructions trailer ;

 header: ID DCOLON signature COLON signature nls { currMtd = defineMethod(pkg,$1,$3,$5); }

 instructions: instructions instruction nls
     | instructions error nls
     | instruction nls
     ;

trailer: END nls { endFunction(currMtd); }

 nls: nls NL | NL;

 optnls: nls | ;

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
   | RET { ARet(currMtd); }
   | JMP label { AJmp(currMtd,$2); }
   ;

 load: LD DECIMAL { ALdInt(currMtd,$2); }
   | LD FLOAT { ALdConst(currMtd,newFloatConstant(currMtd,$2)); }
   | LD STRING { ALdConst(currMtd,newStringConstant(currMtd,$2)); }
   | LD A LBRA DECIMAL RBRA { ALdArg(currMtd,$4); }
   | LD L LBRA DECIMAL RBRA { ALdLocal(currMtd,$4); }
   | LD E LBRA DECIMAL RBRA { ALdEnv(currMtd,$4); }
   | POP { APop(currMtd); }
   | DUP { ADup(currMtd); }
   | SWAP { ASwap(currMtd); }
   | LD LBRA DECIMAL RBRA { ANth(currMtd,$3); }
   ;

 store: ST L LBRA DECIMAL RBRA { AStLocal(currMtd, $4); }
   | ST E LBRA DECIMAL RBRA { AStEnv(currMtd, $4); }
   | ST LBRA DECIMAL RBRA { AStNth(currMtd,$3); }
   | CAS label { ACas(currMtd,$2); }
   ;
 
 caseins: CASE DECIMAL { ACayse(currMtd,$2); };

 heap: ALLOC ID { AAlloc(currMtd,findMethod(currMtd,$2)); }
   ;

 convert: F2I { AF2i(currMtd); }
   | I2F { AI2f(currMtd); }
   ;

 arith: ADD label { AAdd(currMtd, $2); }
   | ADDF label { AAddf(currMtd, $2); }
   | SUB label { ASub(currMtd, $2); }
   | SUBF label { ASubf(currMtd, $2); }
   | INC label { AInc(currMtd, $2); }
   | DEC label { ADec(currMtd, $2); }
   | MUL label { AMul(currMtd, $2); }
   | MULF label { AMulf(currMtd, $2); }
   | DIV label { ADiv(currMtd, $2); }
   | DIVF label { ADivf(currMtd, $2); }
   | REM label { ARem(currMtd, $2); }
   | LEFT { ALeft(currMtd); }
   | ASR { AAsr(currMtd); }
   | RIGHT { ARight(currMtd); }
   | CMP { ACmp(currMtd); }
   | CMPF { ACmpf(currMtd); }
   ;
   
 cond: BZ label { ABz(currMtd,$2); }
   | BNZ label { ABnz(currMtd,$2); }
   | BLT label { ABlt(currMtd,$2); }
   | BLE label { ABle(currMtd,$2); }
   | BGE label { ABge(currMtd,$2); }
   | BGT label { ABgt(currMtd,$2); }
   ;
   
 directive: label COLON { defineLbl(currMtd,$1); }
   | FRAME signature { defineFrame(currMtd,$2); }
   | ID LOCAL DECIMAL signature label label { defineLocal(currMtd,$1,$4,$3,$5,$6); }
   ;

 label: ID { $$ = newLbl(currMtd,$1); };
   
 libName: STRING { $$ = newEscapeConstant(currMtd,$1); }

 signature: STRING { $$ = $1; if(!validSignature($1)){
  sserror(&yyloc,yyInfile,pkg,"invalid signature");
  YYERROR;
 }
 }

%%

static void sserror(YYLTYPE *loc,ioPo asmFile,pkgPo p, char const *errmsg)
{
  reportError(loc->first_line,"%s\n",errmsg);
}

