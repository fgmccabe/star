//
// Created by Francis McCabe on 3/2/18.
//

#ifndef STAR_GLOBALS_H
#define STAR_GLOBALS_H

#include "term.h"
#include "heap.h"
#include "chars.h"

typedef struct global_rec_ *globalPo;

extern clssPo globalClass;

extern globalPo C_GLOB(termPo t);

extern globalPo globalVar(const char *nm);

extern int32 globalVarNo(const char *nm);
extern logical isValidGlobalVarNo(int32 varNo);

extern globalPo findGlobalVar(int32 varNo);
extern globalPo getGlobalVar(char *nm);

extern termPo getGlobal(globalPo v);
extern char *globalVarName(globalPo v);
extern termPo setGlobalVar(globalPo v, termPo e);
extern logical glbIsSet(globalPo glb);

extern termPo voidEnum;
extern termPo okEnum;
extern termPo failEnum;
extern termPo eofEnum;
extern labelPo errorLbl;
extern termPo unitEnum;

extern void initGlobals();

#endif //STAR_GLOBALS_H
