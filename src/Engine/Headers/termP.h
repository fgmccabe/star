//
// Created by Francis McCabe on 6/18/17.
//

#ifndef CAFE_TERMP_H
#define CAFE_TERMP_H

#include "term.h"
#include "code.h"

/*
 * A program label structure
 */

typedef struct _program_label_ {
  clssPo clss;                // == labelClass
  integer arity;              // Arity of label
  integer hash;               // Hash code for the label
  methodPo mtd;               // Optimization - is a method defined for this label?
  char *name;                 // LblRecord name
} LblRecord;

#define LabelCellCount CellCount(sizeof(LblRecord))

typedef struct normal_term {
  labelPo lbl;                // Overlays clss - because it is the term's class
  termPo args[ZEROARRAYSIZE];
} Normal;

#define NormalCellCount(arity) CellCount(sizeof(Normal)+(arity)*sizeof(termPo))

// Some typedefs to help with working with classes

typedef long (*classSizeFun)(specialClassPo class, termPo o);

typedef retCode (*specialHelperFun)(ptrPo arg, void *c);

typedef termPo (*classScanFun)(specialClassPo class, specialHelperFun helper, void *c, termPo o);

typedef termPo (*classCpyFun)(specialClassPo class, termPo dst, termPo src);

typedef retCode (*classDispFun)(ioPo out, termPo t, long depth, logical alt);

typedef comparison (*classCmpFun)(specialClassPo class, termPo t1, termPo t2);

typedef integer (*classHashFun)(specialClassPo class, termPo t1);

typedef struct special_class {
  clssPo clss;                 // == specialClass
  classSizeFun sizeFun;        /* Function to compute size of object */
  classCpyFun copyFun;         /* Function to copy special object */
  classScanFun scanFun;        /* Function to scan object */
  classCmpFun compFun;          // Compare two specials
  classHashFun hashFun;         // Compute a hash code of the value
  classDispFun dispFun;         // How to display the special object
} SpecialClass;

extern clssPo specialClass;

extern void initTerm();

extern logical isSpecialClass(clssPo p);

#endif //CAFE_TERMP_H
