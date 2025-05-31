//
// Created by Francis McCabe on 3/2/18.
//

#ifndef STAR_GLOBALS_H
#define STAR_GLOBALS_H

#include "term.h"
#include "heap.h"
#include "strings.h"

typedef struct global_rec_ *globalPo;

globalPo C_GLOB(termPo t);

globalPo globalVar(const char *nm);

int32 globalVarNo(const char *nm);

globalPo findGlobalVar(int32 varNo);

termPo getGlobal(globalPo v);
char *globalVarName(globalPo v);
termPo setGlobalVar(globalPo v, termPo e);
logical glbIsSet(globalPo glb);

extern termPo voidEnum;
extern termPo canceledEnum;
extern termPo unitEnum;

void initGlobals();

#endif //STAR_GLOBALS_H
