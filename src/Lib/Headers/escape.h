#ifndef _ESCAPES_H_
#define _ESCAPES_H_

#include "config.h"
#include "heap.h"

typedef ptrPo (*libFun)(ptrPo tos);

typedef struct {
  char *name;         /* Name of the escape */
  char *sig;          /* Signature of the escape */
  int32 code;         /* What is the code number for the escape? */
  libFun esc;         /* The function itself */
  int32 arity;        /* How many arguments */
} EscapeRec, *escapePo;

void initEscapes();

void installEscape(char *name, escapePo esc);

escapePo findEscape(char *name);

#endif
