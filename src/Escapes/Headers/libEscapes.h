#ifndef _ESCAPES_H_
#define _ESCAPES_H_

#include "config.h"
#include "heap.h"

typedef ReturnStatus (*libFun)(processPo p, ptrPo tos);

typedef struct {
  char *name;         /* Name of the escape */
  char *sig;          /* Signature of the escape */
  libFun fun;         /* The function itself */
  integer arity;      /* How many arguments */
} EscapeRec, *escapePo;

void installEscapes();

int32 lookupEscape(char *name);

escapePo getEscape(int32 escNo);

extern ReturnStatus rtnStatus(processPo p, retCode ret, char *msg);

#ifdef TRACESTATS
void recordEscape(integer escNo);
void dumpEscapes(ioPo out);
#endif

#endif
