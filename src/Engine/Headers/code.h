#ifndef _CODE_H_
#define _CODE_H_

#include "config.h"
#include "ooio.h"
#include "term.h"

typedef uint16 insWord, *insPo;

typedef struct _method_ *methodPo;
typedef struct _constant_ *constantPo;

labelPo declareLbl(char *name, integer arity);
void initCode();
void markLabels(heapPo heap);

extern methodPo C_MTD(termPo t);

typedef void (*jitCode)();
typedef integer (*cafeFun)();
typedef cafeFun (*pkgFun)();

#endif
