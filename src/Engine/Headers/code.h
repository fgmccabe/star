#ifndef CODE_H_
#define CODE_H_

#include "config.h"
#include "ooio.h"
#include "term.h"
#include "labels.h"
#include "ssaOps.h"

typedef struct instruction_ *ssaInsPo;
typedef struct method_ *methodPo;


void initCode();

methodPo C_MTD(termPo t);

int32 stackDelta(methodPo mtd);

int32 mtdArity(methodPo mtd);
labelPo mtdLabel(methodPo mtd);
logical mtdHasName(methodPo mtd,char *name);

int32 codeSize(methodPo mtd);
logical validPC(methodPo mtd, ssaInsPo pc);
int32 codeOffset(methodPo mtd, ssaInsPo pc);

int32 lclCount(methodPo mtd);

#ifndef NOJIT
logical hasJitCode(methodPo mtd);
#endif

#endif
