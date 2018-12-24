//
// Created by Francis McCabe on 7/31/18.
//

#ifndef STAR_VECTOR_H
#define STAR_VECTOR_H

/*
 * Vector: index addressable list with ability to replace elements
 */

#include "object.h"
#include "iterate.h"

typedef struct _vector_record_ *vectorPo;

extern objectPo getVectEl(vectorPo v, integer ix);
extern retCode addVectEl(vectorPo v, integer off, objectPo el);
extern objectPo replaceVectEl(vectorPo v, integer off, objectPo el);
extern retCode appendVectEl(vectorPo v, objectPo el);
extern objectPo removeVectEl(vectorPo v, integer off);
extern integer vectLength(vectorPo v);
extern logical vectIsEmpty(vectorPo v);

extern retCode pushVectEl(vectorPo v,objectPo el);
extern objectPo popVectEl(vectorPo v);

extern vectorPo vector(int count, ...);

extern classPo vectorClass;

#ifdef VERIFY_OBJECT
#define O_VECT(c) ((vectorPo)(checkCast((c),vectorClass)))
#else
#define O_VECT(c) ((vectorPo)(c))
#endif

#endif //STAR_VECTOR_H
