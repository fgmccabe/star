//
// Created by Francis McCabe on 6/18/17.
//

#ifndef CAFE_TERMP_H
#define CAFE_TERMP_H

#include "term.h"

typedef struct term_record {
  termPo sig;                    // Every value has a signature, which is a 'class' pointer, which is also a term
} Term;

/*
 * A program label structure
 */

typedef struct _program_label_ {
  integer arity;                 // Arity of program
  byte name[ZEROARRAYSIZE];     // Program's print name
} Label, *labelPo;

typedef struct class_record{
  termPo sig;
} Class;

typedef struct enum_struct {
  Class clss;                // The sig of this clss should be equal to ClassClass
  integer hash;
  Label lbl;
} EnumRecord;

typedef struct normal_term {
  Class clss;
  termPo args[ZEROARRAYSIZE];
} Normal, *normalPtr;


// Some typedefs to help with working with classes

typedef long (*classSizeFun)(specialClassPo class, termPo o);

typedef retCode (*specialHelperFun)(termPo arg, void *c);

typedef retCode (*classScanFun)(specialClassPo class, specialHelperFun helper, void *c, termPo o);

typedef termPo (*classCpyFun)(specialClassPo class, termPo dst, termPo src);

typedef struct special_class {
  Class classPart;             // == specialClass
  classSizeFun sizeFun;        /* Function to compute size of object */
  classCpyFun copyFun;         /* Function to copy special object */
  classScanFun scanFun;        /* Function to scan object */
  Label lbl;                    // Label info for this special class
} SpecialClass;

extern clssPo specialClass;

static inline clssPo classOf(termPo obj){
  return (clssPo)obj->sig;
}

static inline logical hasClass(termPo obj,clssPo clss){
  return (logical) ((clssPo)obj->sig == clss);
}

#endif //CAFE_TERMP_H
