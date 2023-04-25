//
// Created by Francis McCabe on 6/18/17.
//

#ifndef STAR_TERM_H
#define STAR_TERM_H

#include "config.h"
#include "ooio.h"

typedef struct special_class *specialClassPo;

typedef struct program_label_ *labelPo;

typedef struct term_record **ptrPo, *termPo;      /* pointer to a structured value */

typedef struct normal_term {
  labelPo lbl;                // Overlays clss - because it is the term's class
  termPo args[ZEROARRAYSIZE];
} Normal, *normalPo;

typedef struct class_record *clssPo;

extern clssPo labelClass;

typedef struct term_record {
  clssPo clss;
} TermRecord;

typedef struct class_record {
  clssPo clss; // == classClass
} ClassRecord;

normalPo C_NORMAL(termPo t);

logical isNormalPo(termPo t);

labelPo termLbl(normalPo t);

logical hasLabel(normalPo n, char *name, integer arity);

integer termHash(termPo t);
integer hashTerm(termPo t);

integer termSize(normalPo t);

integer termArity(normalPo term);

termPo nthArg(normalPo term, integer ix);

static inline termPo nthElem(normalPo term, integer ix) {
  return term->args[ix];
}

void setArg(normalPo term, integer ix, termPo arg);

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

retCode dispTerm(ioPo out, termPo t, integer precision, integer depth, logical alt);
retCode showTerm(ioPo f, void *data, long depth, long precision, logical alt);
retCode showIdentifier(ioPo f, void *data, long depth, long precision, logical alt);

extern logical sameTerm(termPo t1, termPo t2);

#endif //STAR_TERM_H
