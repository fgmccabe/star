#ifndef _ESCAPES_H_
#define _ESCAPES_H_

#include "config.h"
#include "heap.h"

typedef ReturnStatus (*libFun)(processPo p, heapPo h, ptrPo tos);

typedef struct {
  char *name;         /* Name of the escape */
  char *sig;          /* Signature of the escape */
  libFun fun;         /* The function itself */
  integer arity;      /* How many arguments */
} EscapeRec, *escapePo;

void installEscapes();

uint32 lookupEscape(char *name);

escapePo getEscape(uint32 escNo);

extern ReturnStatus rtnStatus(processPo p, heapPo h, retCode ret, char *msg);

#ifdef TRACESTATS
void recordEscape(integer escNo);
void dumpEscapes(ioPo out);
#endif

#undef escape
#define escape(Nm, Tp, Cmt) Esc##Nm,

typedef enum {
#include "escapes.h"
} EscapeCode;

#undef escape

#endif
