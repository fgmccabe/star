//
// Created by Francis McCabe on 6/1/18.
//

#ifndef STAR_RRBP_H
#define STAR_RRBP_H

#include "rrb.h"
#include "code.h"
#include "termP.h"

#define VectorCellCount CellCount(sizeof(VectRecord))

#define VECT_ENTRIES ((unsigned)4)
#define VECT_DEPTH ((unsigned)31) // VECT_ENTRIES**VECT_DEPTH==2**(64-PtrSize)
#define VECT_SIZE ((unsigned)2)
#define VECT_MASK ((((unsigned)1)<<VECT_SIZE)-1)

typedef struct _vector_ {
  clssPo clss;                  // == vectorClass
  integer depth;                // == 0 for leaf node
  integer indices[VECT_ENTRIES];
  termPo els[VECT_ENTRIES];
} VectRecord;

typedef struct _vector_focus {
  integer index;
  integer tos;
  int ixStack[VECT_DEPTH];
} VectorFocusRecord;

extern void initVectors();

extern vectPo allocVector(heapPo H, integer depth);
extern vectPo copyVector(heapPo H,vectPo v);

#endif //STAR_RRBP_H
