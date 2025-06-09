#ifndef LIB_ESCAPES_H_
#define LIB_ESCAPES_H_

#include "config.h"
#include "heap.h"
#include "escape.h"

#define MAX_ESCAPE_ARITY (8)

typedef struct escape_record_ {
  char *name;         /* Name of the escape */
  char *sig;          /* Signature of the escape */
  escFun fun;         /* The function itself */
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
