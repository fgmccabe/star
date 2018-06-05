//
// Created by Francis McCabe on 6/1/18.
//

#ifndef CAFE_RRBP_H
#define CAFE_RRBP_H

#include "rrb.h"
#include "code.h"
#include "termP.h"

#define VectorCellCount CellCount(sizeof(VectorRecord))

#define VECT_ENTRIES ((unsigned)4)
#define VECT_DEPTH ((unsigned)31) // VECT_ENTRIES**VECT_DEPTH==2**(64-PtrSize)
#define VECT_SIZE ((unsigned)2)
#define VECT_MASK ((((unsigned)1)<<VECT_SIZE)-1)

typedef struct _vector_ {
  clssPo clss;                  // == vectorClass
  integer depth;                // == 0 for leaf node
  integer indices[VECT_ENTRIES];
  termPo els[VECT_ENTRIES];
} VectorRecord;

typedef struct _vector_focus {
  integer index;
  integer tos;
  int ixStack[VECT_DEPTH];
} VectorFocusRecord;

extern void initVectors();

extern vectorPo allocVector(heapPo H, integer depth);
extern vectorPo copyVector(heapPo H,vectorPo v);

#endif //CAFE_RRBP_H
