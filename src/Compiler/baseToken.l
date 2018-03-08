/* Lexical scanner for the Cafe language */

%option yylineno
%option bison-bridge bison-locations 
%option noyywrap nounput

%x comment
%x string
%x quoted

%{

#include <config.h>
#include <ooio.h>
#include <iochnl.h>
#include <math.h>
#include "meta.h"
#include "compiler.h"
#include "grammar.h"
#include <unistr.h>

#define MAX_STR_CONST 1024

#define YY_DECL int yylex(YYSTYPE *yylval_param, YYLTYPE *yylloc_param, ioPo yyFile_param)
extern int yylex(YYSTYPE *yylval_param, YYLTYPE *yylloc_param, ioPo yyFile);

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

static void parseError(char *msg,int lineNo);
static long octtoi(char *text);
static long hextoi(char *text);

#define RTN(K) { yylloc->first_line = yylloc->last_line = yylineno; return K;}

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

ID ({IDLETTER}{IDLETTERDIGIT}*)

%%

\n
[ \t]+					/* ignore */
"--"[ \t].*\n				/* line comment */
"/*"                            BEGIN(comment);
<comment>\n                     
<comment>[^*\n]*                
<comment>"*"+[^*/\n]*           
<comment>"*"+"/"                BEGIN(INITIAL);

"function"    RTN(FUNCTION);
"pattern"     RTN(PATTERN);
"memo"        RTN(MEMO);
"import"      RTN(IMPORT);
"type"        RTN(TYPE);
"struct"      RTN(STRUCT);
"enum"        RTN(ENUM);
"all"         RTN(ALL);

"switch"      RTN(SWITCH);
"default"     RTN(DEFAULT);
"let"         RTN(LET);
"in"          RTN(IN);
"is"          RTN(IS);
"def"         RTN(DEF);
"var"         RTN(VAR);
"if"          RTN(IF);
"then"        RTN(THEN);
"else"        RTN(ELSE);
"matches"     RTN(MATCHES);
"or"          RTN(OR);
"and"         RTN(AND);
"not"         RTN(NOT);
"where"       RTN(WHERE);

":="          RTN(ASSIGN);
":"           RTN(COLON);
"="           RTN(EQ);

"_"           RTN(ANON);

"("           RTN(LPAR)
")"           RTN(RPAR)
"{"           RTN(LBRCE)
"}"           RTN(RBRCE)
"<"           RTN(LANGLE);
">"           RTN(RANGLE);
","           RTN(COMMA)
";"           RTN(SEMI)
"."           RTN(PERIOD)
"%"           RTN(PRCENT)
"<="          RTN(LARROW);
"=>"          RTN(RARROW);
"->"          RTN(THINARROW);
"~~"          RTN(ST);

[1-9]{DECDIGIT}* { 
  yylval->i = parseLong(yytext);
  RTN(DECIMAL);
		   }

[1-9]{DECDIGIT}*[lL] { 
  yylval->i = parseLong(yytext);
  RTN(LONG); }

"0"[xX]{HEXDIGIT}+ { yylval->i = hextoi(yytext); RTN(DECIMAL); }

"0"{OCTDIGIT}*     { yylval->i = octtoi(yytext); RTN(DECIMAL); }

{DECDIGIT}+("."{DECDIGIT}+)?([eE][-+]?{DECDIGIT}+)? { 
  yylval->f = parseFloat(yytext); RTN(FLOAT);
	       }

{ID}          { yylval->str = newStr(yytext,strlen(yytext)); RTN(ID); }


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
    RTN(ID);
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
    RTN(STRING);
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
