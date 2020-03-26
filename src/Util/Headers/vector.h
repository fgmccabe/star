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

objectPo getVectEl(vectorPo v, integer ix);
retCode addVectEl(vectorPo v, integer off, objectPo el);
objectPo replaceVectEl(vectorPo v, integer off, objectPo el);
retCode appendVectEl(vectorPo v, objectPo el);
objectPo removeVectEl(vectorPo v, integer off);
integer vectLength(vectorPo v);
logical vectIsEmpty(vectorPo v);

retCode pushVectEl(vectorPo v, objectPo el);
objectPo popVectEl(vectorPo v);
objectPo pullVectEl(vectorPo v);

typedef retCode (*vectorProc)(objectPo entry, integer ix, void *cl);
retCode procVector(vectorPo v, vectorProc proc, void *cl);

vectorPo vector(int count, ...);
vectorPo duplicateVector(vectorPo src);

extern classPo vectorClass;

#ifdef VERIFY_OBJECT
#define O_VECT(c) ((vectorPo)(checkCast((c),vectorClass)))
#else
#define O_VECT(c) ((vectorPo)(c))
#endif

#endif //STAR_VECTOR_H
