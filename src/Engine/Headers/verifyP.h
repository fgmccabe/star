//
// Created by Francis McCabe on 8/1/18.
//

#ifndef STAR_VERIFYP_H
#define STAR_VERIFYP_H

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
  logical checked;                  //  Has this segment been checked?
  methodPo mtd;                     //  Pointer to the code structure itself
  integer pc;                       //  base intruction of this segment
  integer maxPc;                    //  Maximum instruction in this segment
  integer stackDepth;               //  What is the current stack depth?
  integer entryPoints;              //  how many unchecked entry points are there here?
  vectorPo entries;                 //  Which other segments enter this segment?
  vectorPo exits;                   //  What other segments does this segment reference?
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

#endif //STAR_VERIFYP_H
