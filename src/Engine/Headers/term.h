//
// Created by Francis McCabe on 6/18/17.
//

#ifndef STAR_TERM_H
#define STAR_TERM_H

#include "config.h"
#include "ooio.h"
#include "assert.h"
#include "labels.h"

typedef struct builtin_class* builtinClassPo;

typedef struct term_record **ptrPo, *termPo; /* pointer to a structured value */

integer termHash(termPo t);
integer hashTerm(termPo t);

extern termPo falseEnum;
extern termPo trueEnum;
extern int32 trueIndex;
extern int32 falseIndex;

extern builtinClassPo integerClass;
extern int32 integerIndex;
extern builtinClassPo charClass;
extern int32 charIndex;
extern builtinClassPo floatClass;
extern int32 floatIndex;

typedef enum {
  ptrTg = 0,
  intTg = 1,
  chrTg = 2
} PtrTag;

static inline PtrTag pointerTag(termPo t) {
  return (PtrTag)(((uinteger)t) & 3ul);
}

static inline logical isPointer(termPo t) {
  return pointerTag(t) == ptrTg;
}

static inline integer ptrPayload(termPo t) {
  switch (pointerTag(t)){
  case ptrTg:
    return (integer)t;
  case intTg:
  case chrTg:
    return (((integer)t) >> 2l);
  }
}

logical isALabel(termPo t);

builtinClassPo builtinClassOf(termPo t);

static inline termPo checkIndex(termPo obj, int32 index) {
  assert(obj->lblIndex==index);
  return obj;
}

static inline logical hasIndex(termPo t, int32 index) {
  return t->lblIndex == index;
}

retCode dispTerm(ioPo out, termPo t, integer precision, integer depth, logical alt);
retCode showTerm(ioPo f, void* data, long depth, long precision, logical alt);
retCode showIdentifier(ioPo f, void* data, long depth, long precision, logical alt);

logical sameTerm(termPo t1, termPo t2);
comparison compTerm(termPo t1, termPo t2);

#endif //STAR_TERM_H
