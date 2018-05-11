/* Lexical scanner for JSON */

%option yylineno
%option bison-bridge bison-locations
%option noyywrap nounput
/* %option debug */

%x comment
%x string
%x quoted

%{

#include <config.h>
#include <io.h>

#include <retcode.h>
#include <integer.h>

#include "file.h"
#include <math.h>
#include <unistr.h>
#include "jsonEvent.h"
#include "jsonParser.h"
#include "formio.h"

#define MAX_STR_CONST 1024

#define YY_DECL int yylex (YYSTYPE * yylval_param,YYLTYPE * yylloc_param, ioPo yyFile_param)
extern int yylex(YYSTYPE *yylval_param, YYLTYPE *yylloc_param, ioPo yyFile);

static ioPo yyFile;

#define YY_USER_INIT yyFile = yyFile_param;

#define YY_INPUT(buf,result,max) {\
    integer act;\
    retCode ret = inBytes(yyFile,(unsigned char*)buf,max,&act);	\
    if(ret==Eof)\
      result = YY_NULL;				\
    else\
      result = act;				\
  }

static void parseError(char *msg,int lineNo);

#define RTN(K) { yylloc->first_line = yylloc->last_line = yylineno; return K;}


static double parseFloat(char *text)
{
  long len = strlen(text);
  return parseNumber(text,len);
}

static long octtoi(char *text);
static long hextoi(char *text);

char buff[MAX_STR_CONST];
char *text = &buff[0];
long size = NumberOf(buff);
long pos = 0;

static void putChar(char ch) {
  if(pos>=size){
    long nSize = size+size/2;
    char *nBuff = (char*)malloc(nSize*sizeof(char));
    size = nSize;
    for(int ix=0;ix<pos;ix++)
      nBuff[ix]=text[ix];
    if(text!=&buff[0])
      free(text);
    text = nBuff;
  }
  text[pos++] = ch;
}

%}

DECDIGIT [0-9]

HEXDIGIT [0-9a-fA-F]

OCTDIGIT [0-7]

IDLETTER [a-zA-Z_.$@#]
IDLETTERDIGIT [a-zA-Z_.$@0-9#]

ID ([a-zA-Z_.$@#][a-zA-Z_.$@0-9#]*)|"'"[^']*"'"

%%

\n
[ \t]+					/* ignore */
"--"[ \t].*\n				/* line comment */
"/*"                            BEGIN(comment);
<comment>\n                     
<comment>[^*\n]*                
<comment>"*"+[^*/\n]*           
<comment>"*"+"/"                BEGIN(INITIAL);

"["           RTN(LBRA)
"]"           RTN(RBRA)
":"           RTN(COLON)
","           RTN(COMMA)
"{"           RTN(LBRCE)
"}"           RTN(RBRCE)

"true"        RTN(TRUE)
"false"       RTN(FALSE)
"null"        RTN(NuLL)

[-+]?{DECDIGIT}+("."{DECDIGIT}+)?([eE][-+]?{DECDIGIT}+)? {
  yylval->num = parseFloat(yytext); RTN(NUM);
}

%{


#define restart() { text = &buff[0]; pos=0; size=NumberOf(buff); }
#define clear() { if(text!=buff){ free(text); }  restart(); }
%}
  


\"                             { restart(); BEGIN(string); }

<string>{
  \"                        { 
    BEGIN(INITIAL);
    putChar('\0');
    yylval->str = text;
   // clear();
    RTN(STRING)
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
