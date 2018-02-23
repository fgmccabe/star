//
// Created by Francis McCabe on 6/18/17.
//

#ifndef CAFE_TERM_H
#define CAFE_TERM_H

#include <integer.h>
#include "logical.h"
#include "config.h"

typedef struct special_class *specialClassPo;

typedef struct enum_struct *enumPo;

typedef struct _program_label_ *labelPo;

typedef struct normal_term *normalPo;

typedef struct term_record **ptrPo, *termPo;      /* pointer to a structured value */

typedef struct class_record *clssPo;

extern clssPo labelClass, normalClass, enumClass;

typedef struct term_record {
  clssPo clss;
} TermRecord;

extern labelPo C_LBL(termPo t);

extern normalPo C_TERM(termPo t);

extern logical isNormalPo(termPo t);

extern termPo termLbl(normalPo t);

extern enumPo C_ENUM(termPo t);

extern integer termHash(termPo t);

extern int64 termArity(normalPo term);

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

retCode dispTerm(ioPo out, termPo t, long depth, logical alt);

logical sameTerm(termPo t1,termPo t2);

#endif //CAFE_TERM_H
