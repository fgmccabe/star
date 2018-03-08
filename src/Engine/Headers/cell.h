//
// Created by Francis McCabe on 3/5/18.
//

#ifndef CAFE_CELL_H
#define CAFE_CELL_H

#include "term.h"

typedef struct _cell_record_ *cellPo;

extern clssPo cellClass;

extern cellPo C_CELL(termPo t);

extern termPo getCell(cellPo cell);

extern termPo setCell(cellPo cell,termPo e);

#endif //CAFE_CELL_H
