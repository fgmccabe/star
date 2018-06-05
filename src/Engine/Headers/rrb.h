//
// Created by Francis McCabe on 6/1/18.
//

#ifndef CAFE_RRB_H
#define CAFE_RRB_H

#include "term.h"
#include "heap.h"

typedef struct _vector_ *vectorPo;

extern clssPo vectorClass;

extern vectorPo C_VECT(termPo t);

extern vectorPo allocateVector(heapPo H, integer length, logical safeMode);

extern vectorPo appendToVector(heapPo H, vectorPo vect, termPo el);

extern vectorPo prependToVector(heapPo H, vectorPo vect, termPo el);

extern vectorPo deleteEntry(heapPo H, vectorPo vect, integer index);

extern termPo nthEntry(vectorPo vect, integer index);

extern vectorPo setNthEntry(heapPo H, vectorPo vect, integer index, termPo el, logical safeMode);

extern vectorPo concatVector(heapPo H, vectorPo l1, vectorPo l2);

extern vectorPo flattenVector(heapPo H, vectorPo l);

extern vectorPo reverseVector(heapPo H, vectorPo l1);

// Use with caution!
typedef retCode (*vectorProc)(termPo el, integer ix, void *cl);
extern retCode processVector(vectorPo vect, vectorProc p, void *cl);

extern integer vectorCount(vectorPo list);

typedef struct _vector_focus *vFocusPo;

retCode stepForward(vFocusPo f, vectorPo vect);
retCode stepBack(vFocusPo f, vectorPo vect);
termPo currentFocusElement(vFocusPo f, vectorPo vect);

void startVectFocus(vectorPo v, vFocusPo f);
void endVectFocus(vectorPo v, vFocusPo f);
void indexedVectFocus(vectorPo v, integer index, vFocusPo f);

#endif //CAFE_RRB_H
