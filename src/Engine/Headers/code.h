#ifndef CODE_H_
#define CODE_H_

#include "config.h"
#include "ooio.h"
#include "term.h"
#include "normal.h"
#include "opcodes.h"

//typedef uint16 insWord;
typedef struct instruction_ *insPo;

typedef struct method_ *methodPo;

void initCode();

methodPo C_MTD(termPo t);

integer stackDelta(methodPo mtd);

int32 codeArity(methodPo mtd);
labelPo mtdLabel(methodPo mtd);

int32 codeSize(methodPo mtd);
termPo findPcLocation(methodPo mtd, int32 pc);
logical validPC(methodPo mtd, insPo pc);
int32 codeOffset(methodPo mtd, insPo pc);

integer callCount(methodPo mtd);

logical hasJitCode(methodPo mtd);

#endif
