#ifndef _CODE_GEN_H_
#define _CODE_GEN_H_

#include <ooio.h>
#include "lightning.h"
#include <stdarg.h>

#include "dict.h"
#include "type.h"
#include "assem.h"

extern void initCodegen();
extern void initLabels();
extern void initAssem();

extern integer invokeCafe(cafeFun fun,void *heap,integer arg);
extern cafeFun invokePkg(pkgFun fun,void *heap);

#ifdef TRACECODEGEN
#define debug(Op) if(debugCodeGen){ Op; }
#else
#define debug(Op)
#endif

#endif
