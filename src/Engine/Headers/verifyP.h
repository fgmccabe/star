//
// Created by Francis McCabe on 8/1/18.
//

#ifndef CAFE_VERIFYP_H
#define CAFE_VERIFYP_H

#include "verify.h"
#include "objectP.h"
#include "vector.h"

typedef struct {
  logical inited;    //  True if cell has real value
  logical read;      //  Has this cell been read?
} Var, *varPo;

typedef struct _segment_ {
  integer segNo;                    // Segment number
  integer arity;                    //  Arity of the code segment
  varPo args;
  integer lclCount;                 //  number of locals in use
  varPo locals;                     //  entry state for this segment
  integer hpCnt;                    //  how much local heap can we allocate?
  logical checked;                  //  Has this segment been checked?
  methodPo mtd;                     //  Pointer to the code structure itself
  integer pc;                       //  base intruction of this segment
  integer maxPc;                    //  Maximum instruction in this segment
  integer entryPoints;              //  how many entry points are there here?
  integer exits[128];               //  What other segments are referenced by this segment
  int numExits;
} SegObjRecord;

typedef struct _code_segment_ {
  ObjectRec object;                     /* object level of the code segment structure */
  SegObjRecord seg;
} SegmentRecord;

typedef struct {
} SegmentClassPart;

typedef struct _segment_class_ {
  ObjectClassRec objectPart;
  SegmentClassPart segPart;
} SegmentClassRec;

extern SegmentClassRec SegmentClass;

#endif //CAFE_VERIFYP_H
