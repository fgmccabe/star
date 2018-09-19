//
// Created by Francis McCabe on 6/1/18.
//

#ifndef STAR_RRB_H
#define STAR_RRB_H

#include "term.h"
#include "heap.h"

typedef struct _vector_ *vectPo;

extern clssPo vectClass;

extern vectPo C_VECT(termPo t);

extern vectPo allocateVector(heapPo H, integer length, logical safeMode);

extern vectPo appendToVector(heapPo H, vectPo vect, termPo el);

extern vectPo prependToVector(heapPo H, vectPo vect, termPo el);

extern vectPo deleteEntry(heapPo H, vectPo vect, integer index);

extern termPo nthEntry(vectPo vect, integer index);

extern vectPo setNthEntry(heapPo H, vectPo vect, integer index, termPo el, logical safeMode);

extern vectPo concatVector(heapPo H, vectPo l1, vectPo l2);

extern vectPo flattenVector(heapPo H, vectPo l);

extern vectPo reverseVector(heapPo H, vectPo l1);

// Use with caution!
typedef retCode (*vectorProc)(termPo el, integer ix, void *cl);
extern retCode processVector(vectPo vect, vectorProc p, void *cl);

extern integer vectorCount(vectPo list);

typedef struct _vector_focus *vFocusPo;

retCode stepForward(vFocusPo f, vectPo vect);
retCode stepBack(vFocusPo f, vectPo vect);
termPo currentFocusElement(vFocusPo f, vectPo vect);

void startVectFocus(vectPo v, vFocusPo f);
void endVectFocus(vectPo v, vFocusPo f);
void indexedVectFocus(vectPo v, integer index, vFocusPo f);

#endif //STAR_RRB_H
