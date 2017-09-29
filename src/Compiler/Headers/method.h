#ifndef _METHOD_H_
#define _METHOD_H_

#include "dict.h"
#include "../../Engine/Headers/code.h"
#include "assem.h"

typedef struct _method_context_ *mtdCxtPo;
typedef struct _try_block_ *tryPo;

extern void initMethod();

extern mtdCxtPo newMethod(char *name);
extern mtdPo methodCode(mtdCxtPo mtd);
extern cafeFun genMethodCode(mtdCxtPo mtd,lPo entryPoint);

extern tryPo methodCatchBlocks(mtdCxtPo mtd);
extern void addCatchBlock(mtdCxtPo mtd,lPo start,lPo end,lPo recover);

extern tryPo methodCatchBlocks(mtdCxtPo mtd);
extern lPo tryBlockStart(tryPo try);
extern lPo tryBlockEnd(tryPo try);
extern lPo tryBlockRecover(tryPo try);
extern tryPo tryBlockNext(tryPo try);

extern void gcCallSite(mtdCxtPo mtd,dictPo dict);

extern lPo defineLiteralString(mtdCxtPo mtd,char *str);
extern lPo defineLiteralOther(mtdCxtPo mtd,void *data, long size);

extern retCode genClosEvac(mtdCxtPo mtd,lPo entryPoint,lPo evacLbl,lPo scanLbl,
			   lPo scavLbl,lxPo free,dictPo dict,int frSize);

extern retCode genClosScav(mtdCxtPo mtd,lPo scav,lxPo free,dictPo dict);

extern void genLocalScanner(lPo scanTable,dictPo dict,mtdCxtPo mtd);

#endif
