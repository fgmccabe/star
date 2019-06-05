//
// Created by Francis McCabe on 6/18/17.
//

#ifndef STAR_TERM_H
#define STAR_TERM_H

#include "config.h"
#include "ooio.h"

typedef struct special_class *specialClassPo;

typedef struct _program_label_ *labelPo;

typedef struct normal_term *normalPo;

typedef struct term_record **ptrPo, *termPo;      /* pointer to a structured value */

typedef struct class_record *clssPo;

extern clssPo labelClass, normalClass, classClass;

typedef termPo (*gcWalkPo)(clssPo clss,termPo term,void *cl);

typedef struct term_record {
  clssPo clss;
} TermRecord;

typedef struct class_record {
  clssPo clss; // == classClass
} ClassRecord;

extern normalPo C_TERM(termPo t);

extern clssPo C_CLSS(termPo t);

extern logical isNormalPo(termPo t);

extern labelPo termLbl(normalPo t);

extern integer termHash(termPo t);
extern integer hashTermLbl(termPo t);

extern integer termSize(normalPo t);

extern integer termArity(normalPo term);
extern ptrPo termArgs(normalPo term);

extern termPo nthArg(normalPo term, integer ix);
extern void setArg(normalPo term, integer ix, termPo arg);

extern termPo getField(normalPo term,labelPo field);
extern retCode setField(normalPo term, labelPo field, termPo val);

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

logical sameTerm(termPo t1, termPo t2);

extern comparison compareTerm(termPo t1, termPo t2);

#endif //STAR_TERM_H
