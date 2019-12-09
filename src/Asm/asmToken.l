/* Lexical scanner for the Star assembler */

%option yylineno
%option bison-bridge bison-locations
%option noyywrap nounput
%option debug
%option prefix="ss"

%x comment
%x string
%x quoted

%{
#include <config.h>
#include <ooio.h>

#include <math.h>
#include <unistr.h>
#include <assemP.h>
#include <assem.h>
#include <code.h>
#include <heap.h>
#include <term.h>

#include <term.h>

#include <assem.h>

#include "assem.h"

#include "asmGrammar.h"

#define MAX_STR_CONST 2048

#define YY_DECL int sslex(union SSSTYPE *yylval_param, SSLTYPE *yylloc_param, ioPo yyFile_param)
extern int sslex(union SSSTYPE *yylval_param, SSLTYPE *yylloc_param, ioPo ssFile);

static ioPo ssFile;

#define YY_USER_INIT ssFile = yyFile_param;

#define YY_INPUT(buf,result,max) {\
    integer act;\
    retCode ret = inBytes(ssFile,(unsigned char*)buf,max,&act);	\
    if(ret==Eof)\
      result = YY_NULL;				\
    else\
      result = act;				\
  }

#define YY_NO_INPUT

#define YYLTYPE SSLTYPE
#define YYSTYPE SSSTYPE

static void parseError(char *msg,int lineNo);
static long octtoi(char *text);
static long hextoi(char *text);

#define RT(K) { yylloc->first_line = yylloc->last_line = yylineno; return K;}

static long parseLong(char *text)
{
  return parseInteger(text,strlen(text));
}

static double parseFloat(char *text)
{
  return parseNumber(text,strlen(text));
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
"t"           RT(T);

"halt"        RT(HALT);

"call"        RT(CALL);
"escape"      RT(ESCAPE);
"tail"        RT(TAIL);
"otail"       RT(OTAIL);
"debug"       RT(DBG);

"ret"         RT(RET);
"jmp"         RT(JMP);
"case"        RT(CASE);

"cmp"         RT(CMP);
"bt"          RT(BT);
"bf"          RT(BF);

"drop"        RT(DROP)
"dup"         RT(DUP)
"pull"        RT(PULL)
"rot"         RT(ROT)
"rst"         RT(RST)

"ld"          RT(LD);
"st"          RT(ST);

"void"        RT(VOID);

"alloc"       RT(ALLOC);

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
"."           RT(DOT);

-?[1-9]{DECDIGIT}* {
  yylval->i = parseLong(yytext);
  RT(DECIMAL);
		   }

"0"[xX]{HEXDIGIT}+ { yylval->i = hextoi(yytext); RT(DECIMAL); }

"0"{OCTDIGIT}*     { yylval->i = octtoi(yytext); RT(DECIMAL); }

{DECDIGIT}+("."{DECDIGIT}+)?([eE][-+]?{DECDIGIT}+)? {
  yylval->f = parseFloat(yytext); RT(FLOAT);
	       }

{ID}          { yylval->str = uniDuplicate(yytext); RT(ID); }


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
    yylval->str = uniDuplicate(text);
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
    yylval->str = uniDuplicate(text);
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
  outMsg(logFile,"problem %s with token at %U/%l",msg,fileName(ssFile),lineno);
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
      outMsg(logFile,"octal number %s not valid at %U/%l",text,fileName(ssFile),yylineno);
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
	outMsg(logFile,"hex number %s not valid at %U/%l",text,fileName(ssFile),yylineno);
    }
  }
  else if(strncmp(text,"+",1)==0){
    char *hexStr = text+1;
    while(*hexStr!='\0' && *hexStr!=';'){
      int Hex = hexValue(*hexStr++);

      if(0<=Hex && Hex<16)
	val = val*16+Hex;
      else
	outMsg(logFile,"hex number %s not valid at %U/%l",text,fileName(ssFile),yylineno);
    }
  }
  else
    outMsg(logFile,"hex number %s not valid at %U/%l",text,fileName(ssFile),yylineno);

  return val;
}
