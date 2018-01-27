//
// Created by Francis McCabe on 6/18/17.
//

#ifndef CAFE_TERMP_H
#define CAFE_TERMP_H

#include "term.h"

/*
 * A program label structure
 */

typedef struct _program_label_ {
  clssPo clss;                // == labelClass
  integer arity;              // Arity of label
  methodPo mtd;               // Optimization - is a method defined for this label?
  char *name;                 // Label name
} Label;

typedef struct normal_term {
  clssPo clss;
  termPo args[ZEROARRAYSIZE];
} Normal;

typedef struct enum_term {
  clssPo clss;                  // == enumClass
  integer hash;
  integer length;               // Size of name
  char name[ZEROARRAYSIZE];
} EnumRecord;

// Some typedefs to help with working with classes

typedef long (*classSizeFun)(specialClassPo class, termPo o);

typedef retCode (*specialHelperFun)(termPo arg, void *c);

typedef retCode (*classScanFun)(specialClassPo class, specialHelperFun helper, void *c, termPo o);

typedef termPo (*classCpyFun)(specialClassPo class, termPo dst, termPo src);

typedef struct special_class {
  clssPo clss;                 // == specialClass
  classSizeFun sizeFun;        /* Function to compute size of object */
  classCpyFun copyFun;         /* Function to copy special object */
  classScanFun scanFun;        /* Function to scan object */
} SpecialClass;

extern clssPo specialClass;

extern void initTerm();

#endif //CAFE_TERMP_H
