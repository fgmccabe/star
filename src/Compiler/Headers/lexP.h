#ifndef _LEXP_H_
#define _LEXP_H_

/* Private header file defining the structure of a Cafe token */

#include "lex.h"

typedef struct _lex_state_ {
  long currLineNumber;
  long currCharNumber;
  ioPo in;
  stringPo tkBuffer;			/* buffer for collecting token text */
  char *srcName;
  Token head;
} TokenState;

void init_token();		/* initialize the tokeniser */

extern logical traceParse;	/* true if we are trying to trace the parser */

#endif
