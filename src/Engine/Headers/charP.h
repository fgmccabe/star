//
// Created by Francis McCabe on 10/19/21.
//

#ifndef STAR_CHARP_H
#define STAR_CHARP_H

#include "heap.h"
#include "char.h"
#include "termP.h"

typedef struct character_term *charPo;

extern charPo C_CHAR(termPo t);

typedef struct character_term {
  clssPo clss;                  // == charClass
  codePoint cp;
} CharacterRecord;

#define CharCellCount CellCount(sizeof(CharacterRecord))

termPo allocateCharacter(heapPo H, codePoint cp);

void initChars();
#endif //STAR_CHARP_H
