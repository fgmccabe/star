//
// Created by Francis McCabe on 3/5/18.
//

#ifndef STAR_CELL_H
#define STAR_CELL_H

#include "term.h"

typedef struct _cell_record_ *cellPo;

extern clssPo cellClass;

cellPo C_CELL(termPo t);

logical isCell(termPo t);

termPo getCell(cellPo cell);

termPo setCell(cellPo cell, termPo e);

#endif //STAR_CELL_H
