//
// Created by Francis McCabe on 3/5/18.
//

#ifndef CAFE_CELLP_H
#define CAFE_CELLP_H

#include "cell.h"
#include "code.h"
#include "termP.h"

typedef struct _cell_record_ {
  clssPo clss;                  // == cellClass
  termPo content;               // Contents
} CellRecord;

#define CellCellCount CellCount(sizeof(CellRecord))

void initCell();

cellPo newCell(heapPo H, termPo content);

#endif //CAFE_CELLP_H
