//
// Created by Francis McCabe on 3/5/18.
//

#ifndef STAR_CELLP_H
#define STAR_CELLP_H

#include "cell.h"
#include "termP.h"

typedef struct _cell_record_ {
  clssPo clss;                  // == cellClass
  integer hash;                 // Cells have a stable hash
  termPo content;               // Contents
} CellRecord;

#define CellCellCount CellCount(sizeof(CellRecord))

void initCell();

cellPo newCell(heapPo H, termPo content);

#endif //STAR_CELLP_H
