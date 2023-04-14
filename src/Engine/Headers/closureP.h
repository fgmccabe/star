//
// Created by Francis McCabe on 4/11/23.
//

#ifndef STAR_CLOSUREP_H
#define STAR_CLOSUREP_H

#include "closure.h"
#include "termP.h"

typedef struct closure_record_ {
  clssPo clss;                  // == closureClass
  labelPo lbl;
  termPo content;               // Contents
} ClosureRecord;

#define ClosureCellCount CellCount(sizeof(ClosureRecord))

void initClosure();

closurePo newClosure(heapPo H, labelPo code,termPo content);

#endif //STAR_CLOSUREP_H
