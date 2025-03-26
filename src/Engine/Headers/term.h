//
// Created by Francis McCabe on 6/18/17.
//

#ifndef STAR_TERM_H
#define STAR_TERM_H

#include "config.h"
#include "ooio.h"
#include "assert.h"

typedef struct class_record *clssPo;

typedef struct term_record **ptrPo, *termPo;      /* pointer to a structured value */

integer termHash(termPo t);
integer hashTerm(termPo t);

extern termPo falseEnum;
extern termPo trueEnum;

extern clssPo integerClass;
extern clssPo charClass;
extern clssPo floatClass;

typedef enum {
  ptrTg = 0,
  intTg = 1,
  chrTg = 2,
  fltTg = 3
} PtrTag;

static inline PtrTag pointerTag(termPo t) {
  return (PtrTag) (((uinteger) t) & 3ul);
}

static inline logical isPointer(termPo t) {
  return pointerTag(t) == ptrTg;
}

static inline integer ptrPayload(termPo t) {
  switch (pointerTag(t)) {
    case ptrTg:
      return (integer) t;
    case intTg:
    case chrTg:
      return (((integer) t) >> 2l);
    case fltTg:
      return (integer) (((uint64) t) & (((uint64) -1) << 2ul));
  }
}

typedef struct term_record {
  clssPo clss;
} TermRecord;

static inline clssPo classOf(termPo obj) {
  switch (pointerTag(obj)) {
    case ptrTg:
      return obj->clss;
    case intTg:
      return integerClass;
    case chrTg:
      return charClass;
    case fltTg:
      return floatClass;
  }
}

static inline logical hasClass(termPo obj, clssPo clss) {
  return (logical) (obj != Null && classOf(obj) == clss);
}

static inline termPo checkClass(termPo obj, clssPo clss){
  assert(hasClass(obj,clss));
  return obj;
}

retCode dispTerm(ioPo out, termPo t, integer precision, integer depth, logical alt);
retCode showTerm(ioPo f, void *data, long depth, long precision, logical alt);
retCode showIdentifier(ioPo f, void *data, long depth, long precision, logical alt);

logical sameTerm(termPo t1, termPo t2);
comparison compTerm(termPo t1,termPo t2);

#endif //STAR_TERM_H

