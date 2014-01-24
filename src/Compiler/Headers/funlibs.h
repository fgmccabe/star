#ifndef _FUN_LIBS_H_
#define _FUN_LIBS_H_

#include "compiler.h"
#include "type.h"
#include "dict.h"

typedef int (*cFunPo)();

extern logical isLibFun(uniChar *name);

extern varInfoPo findLibFun(uniChar *name);

extern void defineLibFun(char *name,sxPo type,cFunPo fun);

extern logical isLibVar(uniChar *name);

extern void defineLibVar(char *name,sxPo type,void *address);

extern varInfoPo findLibVar(uniChar *name);

extern void initIoFuns();

extern void initSystemFuns();

#endif
