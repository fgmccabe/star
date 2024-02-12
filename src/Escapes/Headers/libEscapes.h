#ifndef LIB_ESCAPES_H_
#define LIB_ESCAPES_H_

#include "config.h"
#include "heap.h"

typedef ReturnStatus (*libFun)(heapPo h);
typedef ReturnStatus (*escFun0)(heapPo h);
typedef ReturnStatus (*escFun1)(heapPo h, termPo arg1);
typedef ReturnStatus (*escFun2)(heapPo h, termPo arg1, termPo arg2);
typedef ReturnStatus (*escFun3)(heapPo h, termPo arg1, termPo arg2, termPo arg3);
typedef ReturnStatus (*escFun4)(heapPo h, termPo arg1, termPo arg2, termPo arg3, termPo arg4);
typedef ReturnStatus (*escFun5)(heapPo h, termPo arg1, termPo arg2, termPo arg3, termPo arg4, termPo arg5);
typedef ReturnStatus (*escFun6)(heapPo h, termPo arg1, termPo arg2, termPo arg3, termPo arg4, termPo arg5, termPo arg6);
typedef ReturnStatus (*escFun7)(heapPo h, termPo arg1, termPo arg2, termPo arg3, termPo arg4, termPo arg5, termPo arg6,
                                termPo arg7);
typedef ReturnStatus (*escFun8)(heapPo h, termPo arg1, termPo arg2, termPo arg3, termPo arg4, termPo arg5, termPo arg6,
                                termPo arg7, termPo arg8);

#define MAX_ESCAPE_ARITY (8)

typedef struct {
  char *name;         /* Name of the escape */
  char *sig;          /* Signature of the escape */
  libFun fun;         /* The function itself */
  integer arity;      /* How many arguments */
} EscapeRec, *escapePo;

void installEscapes();

uint32 lookupEscape(char *name);

escapePo getEscape(uint32 escNo);
char *escapeName(escapePo esc);
integer escapeArity(escapePo esc);

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
