#ifndef CODE_H_
#define CODE_H_

#include "config.h"
#include "ooio.h"
#include "term.h"
#include "normal.h"
#include "opcodes.h"

//typedef uint16 insWord;
typedef struct instruction_ *insPo;
typedef struct block_ *blockPo;

typedef struct method_ *methodPo;

void initCode();

methodPo C_MTD(termPo t);

normalPo codeLits(methodPo mtd);
integer codeLitCount(methodPo mtd);
integer stackDelta(methodPo mtd);

termPo getMtdLit(methodPo mtd, integer litNo);

integer lclCount(methodPo mtd);
integer codeArity(methodPo mtd);

retCode findPcLocation(methodPo mtd, insPo pc, char *buffer, integer buffLen);

integer callCount(methodPo mtd);

logical normalCode(methodPo mtd);
#endif
