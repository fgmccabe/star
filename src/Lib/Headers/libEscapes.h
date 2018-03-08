#ifndef _ESCAPES_H_
#define _ESCAPES_H_

#include "config.h"
#include "heap.h"

typedef retCode (*libFun)(processPo p,ptrPo tos);

typedef struct {
  char *name;         /* Name of the escape */
  char *sig;          /* Signature of the escape */
  libFun fun;         /* The function itself */
  integer arity;      /* How many arguments */
} EscapeRec, *escapePo;

void initEscapes();

void installEscape(char *name, char *sig, libFun fun);

escapePo findEscape(char *name);

escapePo getEscape(int32 escNo);

#endif
