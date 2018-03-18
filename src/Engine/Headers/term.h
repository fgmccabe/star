//
// Created by Francis McCabe on 6/18/17.
//

#ifndef CAFE_TERM_H
#define CAFE_TERM_H

#include "config.h"
#include "ooio.h"

typedef struct special_class *specialClassPo;

typedef struct _program_label_ *labelPo;

typedef struct normal_term *normalPo;

typedef struct term_record **ptrPo, *termPo;      /* pointer to a structured value */

typedef struct class_record *clssPo;

extern clssPo labelClass, normalClass;

typedef struct term_record {
  clssPo clss;
} TermRecord;

typedef struct class_record {
  clssPo clss;
} ClassRecord;

extern labelPo C_LBL(termPo t);

extern integer labelArity(labelPo lbl);

extern normalPo C_TERM(termPo t);

extern clssPo C_CLSS(termPo t);

extern logical isNormalPo(termPo t);

extern labelPo termLbl(normalPo t);

extern integer termHash(termPo t);

extern size_t termSize(normalPo t);

extern integer termArity(normalPo term);

extern termPo nthArg(normalPo term, int64 nth);

extern void setArg(normalPo term, int64 ix, termPo arg);

extern termPo falseEnum;
extern termPo trueEnum;

static inline clssPo classOf(termPo obj) {
  return obj->clss;
}

static inline logical hasClass(termPo obj, clssPo clss) {
  return (logical) ((clssPo) obj->clss == clss);
}

extern retCode dispTerm(ioPo out, termPo t, long depth, logical alt);

logical sameTerm(termPo t1,termPo t2);

extern comparison compareTerm(termPo t1,termPo t2);

#endif //CAFE_TERM_H
