//
// Created by Francis McCabe on 6/18/17.
//

#ifndef STAR_TERM_H
#define STAR_TERM_H

#include "config.h"
#include "ooio.h"

typedef struct special_class *specialClassPo;

typedef struct program_label_ *labelPo;

typedef struct normal_term *normalPo;

typedef struct term_record **ptrPo, *termPo;      /* pointer to a structured value */

typedef struct class_record *clssPo;

extern clssPo labelClass, normalClass, classClass;

typedef termPo (*gcWalkPo)(clssPo clss, termPo term, void *cl);

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
integer hashTermLbl(termPo t);

integer termSize(normalPo t);

integer termArity(normalPo term);

termPo nthArg(normalPo term, integer ix);
termPo nthElem(normalPo term, integer ix);
void setArg(normalPo term, integer ix, termPo arg);

extern termPo falseEnum;
extern termPo trueEnum;

static inline clssPo classOf(termPo obj) {
  return obj->clss;
}

static inline logical hasClass(termPo obj, clssPo clss) {
  return (logical) (obj != Null && (clssPo) obj->clss == clss);
}

retCode dispTerm(ioPo out, termPo t, integer precision, integer depth, logical alt);
retCode showTerm(ioPo f, void *data, long depth, long precision, logical alt);

extern logical sameTerm(termPo t1, termPo t2);

#endif //STAR_TERM_H
