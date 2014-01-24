/*
 * Header for the lexer functionality.
 */

#ifndef _LEX_H_
#define _LEX_H_
#include <ooio.h>
#include "location.h"

typedef enum{
  eofToken, idToken, strToken, intToken, fltToken, chrToken,
  lparToken, rparToken, atToken,lbraToken, rbraToken, lbrceToken, rbrceToken,
  qtToken, commaToken, colonToken,
  errorToken, unknownToken
} tokenType;

typedef struct _token_ *tokenPo;
typedef struct _lex_state_ *tkStatePo;

typedef struct _token_ {
  tokenType tt;			/* Token type */
  union {
    integer i;			/* When the tokenizer finds an integer */
    double f;			/* or a float */
    uniChar *text;		/* or some text */
    uniChar ch;                 // For a character literal
  } tk;
  int line_num;			/* which line number */
  int start;			/* character offset to start of token */
  int end;			/* character offset to token end */
} Token;

extern tkStatePo initTokenState(ioPo in);
extern void releaseTokenState(tkStatePo st);

extern tokenType tkType(tokenPo tok);
extern int tkLineNum(tokenPo tok);
extern int tkStart(tokenPo tok);
extern int tkEnd(tokenPo tok);

extern integer tkInteger(tokenPo tok);
extern double tkFloat(tokenPo tok);
extern uniChar *tkText(tokenPo tok);
extern uniChar tkChar(tokenPo tok);

extern tokenType nextoken(tkStatePo st,tokenPo tok);
extern tokenType hedtoken(tkStatePo st,tokenPo tok);
extern void commit_token(tkStatePo st);
extern uniChar *tkStateSrc(tkStatePo st);
extern ioPo tkStateInput(tkStatePo st);
extern long tkStateCurrLine(tkStatePo st);
extern long tkStateCurrChar(tkStatePo st);

extern retCode testLexer(uniChar *name);

extern retCode lexingError(tkStatePo st,char *fmt,...);


#endif
