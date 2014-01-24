#ifndef _ESCAPES_H_
#define _ESCAPES_H_

#include "config.h"

typedef uint64 (*libFun)(uint64* tos);

typedef struct {
  uniChar *name;			/* Name of the escape */
  uniChar *sig;				/* Signature of the escape */
  libFun esc;				/* The function itself */
  int32 arity;				/* How many arguments */
} EscapeRec, *escapePo;

void initEscapes();

void installEscape(char *name,escapePo esc);

escapePo findEscape(uniChar *name);

#endif
