//
// Created by Francis McCabe on 6/18/17.
//

#ifndef CAFE_TERM_H
#define CAFE_TERM_H

#include "config.h"

typedef struct special_class *specialClassPo;

typedef struct enum_struct *enumPo;

typedef struct _program_label_ *labelPo;

typedef struct normal_term *normalPo;

typedef struct term_record **ptrPo, *termPo;      /* pointer to a structured value */

typedef struct class_record *clssPo;

extern clssPo labelClass, normalClass, enumClass;

extern labelPo C_LBL(termPo t);

extern normalPo C_TERM(termPo t);

extern enumPo C_ENUM(termPo t);


extern termPo nthArg(termPo term, int64 nth);

extern void setArg(termPo term, int64 ix, termPo arg);

extern termPo falseEnum;
extern termPo trueEnum;

#endif //CAFE_TERM_H
