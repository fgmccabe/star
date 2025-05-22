#ifndef LIB_ESCAPES_H_
#define LIB_ESCAPES_H_

#include "config.h"
#include "heap.h"
#include "escape.h"
#include "stack.h"

typedef ReturnStatus (*escFun0)(heapPo h);
typedef ReturnStatus (*escFun1)(heapPo h, termPo arg1);
typedef ReturnStatus (*escFun2)(heapPo h, termPo arg1, termPo arg2);
typedef ReturnStatus (*escFun3)(heapPo h, termPo arg1, termPo arg2, termPo arg3);
typedef ReturnStatus (*escFun4)(heapPo h, termPo arg1, termPo arg2, termPo arg3, termPo arg4);

typedef ReturnStatus (*escFun)(heapPo h, stackPo stk);

#define MAX_ESCAPE_ARITY (8)

typedef struct escape_record_ {
  char *name;         /* Name of the escape */
  char *sig;          /* Signature of the escape */
  libFun fun;         /* The function itself */
  int32 arity;        /* How many arguments */
} EscapeRec;

void installEscapes();

uint32 lookupEscape(char *name);

void recordEscape(integer escNo);
void dumpEscapes(ioPo out);

#undef escape
#define escape(Nm, Tp, Cmt) Esc##Nm,

typedef enum {
#include "escapes.h"
} EscapeCode;

#undef escape

#endif
