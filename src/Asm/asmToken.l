/* Lexical scanner for the Cafe assembler */

%option yylineno
%option bison-bridge bison-locations 
%option noyywrap nounput
%option nodefault

%x comment
%x string
%x quoted
%option prefix="ss"

%{
#include <config.h>
#include <ooio.h>
#include <io.h>
#include <math.h>
#include <unicode.h>
#include "assem.h"
#include "asmGrammar.h"

#define MAX_STR_CONST 2048

#define YY_DECL int sslex(union YYSTYPE *yylval_param, YYLTYPE *yylloc_param, ioPo yyFile_param)
extern int sslex(union YYSTYPE *yylval_param, YYLTYPE *yylloc_param, ioPo yyFile);

static ioPo yyFile;

#define YY_USER_INIT yyFile = yyFile_param;

#define YY_INPUT(buf,result,max) {\
    long act;\
    retCode ret = inBytes(yyFile,(unsigned char*)buf,max,&act);	\
    if(ret==Eof)\
      result = YY_NULL;				\
    else\
      result = act;				\
  }

#define YY_NO_INPUT

static void parseError(char *msg,int lineNo);
static long octtoi(char *text);
static long hextoi(char *text);

#define RT(K) { yylloc->first_line = yylloc->last_line = yylineno; return K;}

static char *newStr(char *text,int len){
  char buff[len+1];
  return uniIntern(_uni((unsigned char*)text,buff,NumberOf(buff)));
}

static long parseLong(char *text)
{
  long len = strlen(text)+1;
  char buff[len];
  _uni((unsigned char*)text,buff,len);
  return parseInteger(buff,len-1);
}

static double parseFloat(char *text)
{
  long len = strlen(text)+1;
  char buff[len];
  _uni((unsigned char*)text,buff,len);
  return parseNumber(buff,len-1);
}

%}

DECDIGIT [0-9]

HEXDIGIT [0-9a-fA-F]

OCTDIGIT [0-7]

IDLETTER [a-zA-Z_.$@#]
IDLETTERDIGIT [a-zA-Z_.$@0-9#]

ID ([a-zA-Z_.$@#][a-zA-Z_.$@0-9#]*)|"'"[^']*"'"

%%

\n           RT(NL);

[ \t]+					/* ignore */

"--"[ \t].*\n RT(NL);			/* line comment */

"/*"                            BEGIN(comment);
<comment>\n                     
<comment>[^*\n]*                
<comment>"*"+[^*/\n]*           
<comment>"*"+"/"                BEGIN(INITIAL);

"a"           RT(A);
"l"           RT(L);
"e"           RT(E);

"halt"        RT(HALT);

"call"        RT(CALL);
"enter"       RT(ENTER);
"escape"      RT(ESCAPE);
"tail"        RT(TAIL);

"ret"         RT(RET);
"jmp"         RT(JMP);
"case"        RT(CASE);

"drop"        RT(DROP)
"dup"         RT(DUP)
"pull"        RT(PULL)
"rot"         RT(ROT)

"ld"          RT(LD);
"st"          RT(ST);

"alloc"       RT(ALLOC);

"f2i"         RT(F2I);
"i2f"         RT(I2F);

"addi"        RT(ADDI);
"addf"        RT(ADDF);
"ladd"        RT(LADD);

"subi"        RT(SUBI);
"subf"        RT(SUBF);
"lsub"        RT(LSUB);

"muli"        RT(MULI);
"mulf"        RT(MULF);
"lmul"        RT(LMUL);

"divi"        RT(DIVI);
"divf"        RT(DIVF);
"ldiv"        RT(LDIV);

"remi"        RT(REMI);
"remf"        RT(REMF);
"lrem"        RT(LREM);

"dec"         RT(DECI);
"inc"         RT(INCI);

"left"        RT(LFT);
"right"       RT(RGT);
"asr"         RT(ASR);

"cmp"         RT(CMPI);
"lcmp"        RT(LCMP);
"cmpf"        RT(CMPF);

"bz"          RT(BZ);
"bnz"         RT(BNZ);
"bf"          RT(BF);
"bnf"         RT(BNF);
"bge"         RT(BGE);
"ble"         RT(BLE);
"blt"         RT(BLT);
"bgt"         RT(BGT);

"cas"         RT(CAS);

"frame"       RT(FRAME);
"local"       RT(LOCAL);
"end"         RT(END);

"package"     RT(PKG);
"public"      RT(PUBLIC);
"import"      RT(IMPORT);

"["           RT(LBRA);
"]"           RT(RBRA);
"::"          RT(DCOLON);
":"           RT(COLON);
"#"           RT(HASH);
"/"           RT(SLASH);

-?[1-9]{DECDIGIT}* { 
  yylval->i = parseLong(yytext);
  RT(DECIMAL);
		   }

"0"[xX]{HEXDIGIT}+ { yylval->i = hextoi(yytext); RT(DECIMAL); }

"0"{OCTDIGIT}*     { yylval->i = octtoi(yytext); RT(DECIMAL); }

{DECDIGIT}+("."{DECDIGIT}+)?([eE][-+]?{DECDIGIT}+)? { 
  yylval->f = parseFloat(yytext); RT(FLOAT);
	       }

{ID}          { yylval->str = newStr(yytext,strlen(yytext)); RT(ID); }


%{
  char buff[MAX_STR_CONST];
  char *text = &buff[0];
  long size = NumberOf(buff);
  long pos = 0;

#define putChar(ch) {				\
    if(pos>=size){				\
      char *nBuff = (char*)malloc((size+size/2)*sizeof(char));\
      for(int ix=0;ix<pos;ix++)\
	nBuff[ix]=text[ix];    \
      if(text!=buff)	       \
	free(text);	       \
      text = nBuff;	       \
    }			       \
    text[pos++] = (ch);	       \
	      }

#define restart() { text = &buff[0]; pos=0; size=NumberOf(buff); }
#define clear() { if(text!=buff){ free(text); restart(); } }
%}
  
\'                             { restart(); BEGIN(quoted); }

<quoted>{
  \'                        { 
    BEGIN(INITIAL);
    putChar('\0');
    yylval->str = uniIntern(text);
    clear();
    RT(ID);
  }
  \n                        { parseError("unterminated quoted name",yylineno); }
  \\[0-7]{1,3} {
    int octCode = octtoi(yytext);
    putChar(octCode);
  }
  \\+[0-9a-fA-F]*; {
    long hexCode = hextoi(yytext);
    putChar(hexCode);
  }
  \\a                       putChar('\a');
  \\b                       putChar('\b');
  \\f                       putChar('\f');
  \\n                       putChar('\n');
  \\r                       putChar('\r');
  \\t                       putChar('\t');
  \\.                       putChar(yytext[1]);
  [^\\\n\"\']+                {
    char *yptr = yytext;
    while(*yptr)
      putChar(*yptr++);
  }
 }

\"                             { restart(); BEGIN(string); }

<string>{
  \"                        { 
    BEGIN(INITIAL);
    putChar('\0');
    yylval->str = uniIntern(text);
    clear();
    RT(STRING);
  }
  \n                        { parseError("unterminated string",yylineno);
  }
  \\[0-7]{1,3} {
    int octCode = octtoi(yytext);
    putChar(octCode);
  }
  \\+[0-9a-fA-F]*; {
    long hexCode = hextoi(yytext);
    putChar(hexCode);
  }
  \\a                       putChar('\a');
  \\b                       putChar('\b');
  \\f                       putChar('\f');
  \\n                       putChar('\n');
  \\r                       putChar('\r');
  \\t                       putChar('\t');
  \\.                       putChar(yytext[1]);
  [^\\\n\"]+                {
    char *yptr = yytext;
    while(*yptr)
      putChar(*yptr++);
  }
 }

%%

static void parseError(char *msg, int lineno)
{
  outMsg(logFile,"problem %s with token at %U/%l",msg,fileName(yyFile),lineno);
}

static int octValue(char ch)
{
  return ch-'0';
}

static long octtoi(char *text)
{
  long val = 0;
  char *octStr = text;
  while(*octStr!='\0'){
    int Oct = octValue(*octStr++);

    if(0<=Oct && Oct<8)
      val = val*8+Oct;
    else
      outMsg(logFile,"octal number %s not valid at %U/%l",text,fileName(yyFile),yylineno);
  }
  return val;
}

static int hexValue(char ch)
{
  return ch-'0';
}

static long hextoi(char *text)
{
  long val = 0;

  if(strncmp(text,"0x",2)==0){
    char *hexStr = text+strlen("0x");

    while(*hexStr!='\0'){
      int Hex = hexValue(*hexStr++);
      
      if(0<=Hex && Hex<16)
	val = val*16+Hex;
      else
	outMsg(logFile,"hex number %s not valid at %U/%l",text,fileName(yyFile),yylineno);
    }
  }
  else if(strncmp(text,"+",1)==0){
    char *hexStr = text+1;
    while(*hexStr!='\0' && *hexStr!=';'){
      int Hex = hexValue(*hexStr++);
      
      if(0<=Hex && Hex<16)
	val = val*16+Hex;
      else
	outMsg(logFile,"hex number %s not valid at %U/%l",text,fileName(yyFile),yylineno);
    }
  }
  else
    outMsg(logFile,"hex number %s not valid at %U/%l",text,fileName(yyFile),yylineno);
  
  return val;
}
