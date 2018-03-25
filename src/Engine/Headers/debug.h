//
// Created by Francis McCabe on 6/20/17.
//

#ifndef CAFE_DEBUG_H
#define CAFE_DEBUG_H

#include "engine.h"

extern logical SymbolDebug;
extern long cmdCounter;

extern void insDebug(integer pcCount, processPo p, heapPo h, methodPo mtd, insPo pc, framePo fp, ptrPo sp);
extern void lineDebug(processPo p, heapPo h, methodPo mtd, termPo ln, insPo pc, framePo fp, ptrPo sp);
extern insPo disass(ioPo out, integer pcCount, processPo p, methodPo mtd, insPo pc, framePo fp, ptrPo sp);
extern void countIns(insWord ins);
extern void dumpInsCount();

#endif //CAFE_DEBUG_H
