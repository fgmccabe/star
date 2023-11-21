//
// Created by Francis McCabe on 6/18/17.
//

#ifndef STAR_TERMP_H
#define STAR_TERMP_H

#include "engine.h"
#include "term.h"
#include "code.h"


typedef struct class_record {
  clssPo clss;
} ClassRecord;

// Some typedefs to help with working with classes
typedef struct special_class *specialClassPo;

typedef long (*classSizeFun)(specialClassPo class, termPo o);

typedef retCode (*specialHelperFun)(ptrPo arg, void *c);

typedef termPo (*classScanFun)(specialClassPo class, specialHelperFun helper, void *c, termPo o);

typedef termPo (*classCpyFun)(specialClassPo class, termPo dst, termPo src);

typedef retCode (*classDispFun)(ioPo out, termPo t, integer precision, integer depth, logical alt);

typedef logical (*classSameFun)(specialClassPo class, termPo t1, termPo t2);

typedef integer (*classHashFun)(specialClassPo class, termPo t1);

typedef termPo (*classFinalizerFun)(specialClassPo class, termPo o);

typedef struct special_class {
  clssPo clss;                 // == specialClass
  classSizeFun sizeFun;        /* Function to compute size of object */
  classCpyFun copyFun;         /* Function to copy special object */
  classScanFun scanFun;        /* Function to scan object */
  classFinalizerFun finalizer; // Called when finalizing a dead object
  classSameFun compFun;        // Compare two specials
  classHashFun hashFun;        // Compute a hash code of the value
  classDispFun dispFun;        // How to display the special object
} SpecialClass;

extern clssPo specialClass;

extern integer displayDepth;   // Global limit on how deep to display things

void initTerm();

logical isSpecialClass(clssPo p);

#endif //STAR_TERMP_H
