#ifndef CODE_H_
#define CODE_H_

#include "config.h"
#include "ooio.h"
#include "term.h"
#include "normal.h"
#include "opcodes.h"

typedef uint16 insWord;
typedef const insWord *insPo;

typedef struct method_ *methodPo;

void initCode();

methodPo C_MTD(termPo t);

normalPo codeLits(methodPo mtd);
integer codeLitCount(methodPo mtd);
integer stackDelta(methodPo mtd);

termPo getMtdLit(methodPo mtd, integer litNo);

integer lclCount(methodPo mtd);
integer codeArity(methodPo mtd);

termPo findPcLocation(methodPo mtd, integer pc);
integer insOffset(methodPo m, insPo pc);
insPo pcAddr(methodPo mtd, integer pcOffset);
integer mtdCodeSize(methodPo mtd);

integer callCount(methodPo mtd);

logical normalCode(methodPo mtd);
#endif
