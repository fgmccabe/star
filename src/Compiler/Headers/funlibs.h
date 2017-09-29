#ifndef _FUN_LIBS_H_
#define _FUN_LIBS_H_

#include "compiler.h"
#include "type.h"
#include "dict.h"

typedef int (*cFunPo)();

extern logical isLibFun(char *name);

extern varInfoPo findLibFun(char *name);

extern void defineLibFun(char *name,sxPo type,cFunPo fun);

extern logical isLibVar(char *name);

extern void defineLibVar(char *name,sxPo type,void *address);

extern varInfoPo findLibVar(char *name);

extern void initIoFuns();

extern void initSystemFuns();

#endif
