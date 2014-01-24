#ifndef _METHOD_H_
#define _METHOD_H_

#include "dict.h"
#include "code.h"
#include "assem.h"

typedef struct _method_context_ *mtdPo;
typedef struct _try_block_ *tryPo;

extern void initMethod();

extern mtdPo newMethod(uniChar *name);
extern assemPo methodCode(mtdPo mtd);
extern cafeFun genMethodCode(mtdPo mtd,lPo entryPoint);

extern tryPo methodCatchBlocks(mtdPo mtd);
extern void addCatchBlock(mtdPo mtd,lPo start,lPo end,lPo recover);

extern tryPo methodCatchBlocks(mtdPo mtd);
extern lPo tryBlockStart(tryPo try);
extern lPo tryBlockEnd(tryPo try);
extern lPo tryBlockRecover(tryPo try);
extern tryPo tryBlockNext(tryPo try);

extern void gcCallSite(mtdPo mtd,dictPo dict);

extern lPo defineLiteralString(mtdPo mtd,uniChar *str);
extern lPo defineLiteralOther(mtdPo mtd,void *data, long size);

extern retCode genClosEvac(mtdPo mtd,lPo entryPoint,lPo evacLbl,lPo scanLbl,
			   lPo scavLbl,lxPo free,dictPo dict,int frSize);

extern retCode genClosScav(mtdPo mtd,lPo scav,lxPo free,dictPo dict);

extern void genLocalScanner(lPo scanTable,dictPo dict,mtdPo mtd);

#endif
