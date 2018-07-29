//
// Created by Francis McCabe on 7/26/18.
//

#include <stdlib.h>
#include "verify.h"

typedef struct {
  logical inited;    //  True if cell has real value
  logical read;      //  Has this cell been read?
} Var, *varPo;

typedef struct _segment_ *segPo;

typedef struct _segment_ {
  varPo args;
  integer lclCount;                 //  number of locals in use
  varPo locals;                     //  entry state for this segment
  integer arity;                    //  Arity of the code segment
  integer hpCnt;                    //  how much local heap can we allocate?
  logical inited;                   //  Has the segment's variables been initialized?
  logical checked;                  //  Has this segment been checked?
  logical allocating;               //  Are we allocating variables?
  methodPo mtd;                     //  Pointer to the code structure itself
  integer pc;                       //  base intruction of this segment
  integer insCount;                 //  No. of instructions in this segment
  segPo prev;                       //  previous physical segment
  segPo next;                       //  next physical segment
  segPo alt;
  unsigned long entryPoints;        //  how many entry points are there here?
} segRecord;

typedef struct {
  segPo *table;        //  vector of ptrs into the list of segments
  unsigned int top;      //  current top of the vector
  unsigned int size;      //  current size of the vector
} *segVectPo;

static poolPo segPool = Null;

static varPo copyVars(varPo vars, integer count);

static segPo initVerify(methodPo mtd) {
  if (segPool == Null) {
    segPool = newPool(sizeof(segRecord), 16);
  }

  segPo first = (segPo) allocPool(segPool);

  first->mtd = mtd;
  first->pc = 0;
  first->insCount = insCount(mtd);
  first->next = first->prev = first->alt = Null;
  first->entryPoints = 1;
  first->checked = first->inited = False;
  first->allocating = False;
  first->hpCnt = 0;
  first->lclCount = 0;

  first->args = (varPo) malloc(sizeof(Var) * mtd->arity);
  first->locals = (varPo) malloc(sizeof(Var) * mtd->lclcnt);

  for (integer ix = 0; ix < mtd->arity; ix++) {
    varPo a = &first->args[ix];
    a->inited = True;
    a->read = False;
  }

  for(integer ix=0;ix<mtd->lclcnt;ix++){
    varPo l = &first->locals[ix];
    l->inited = False;
    l->read = False;
  }
  return first;
}

varPo copyVars(varPo vars, integer count) {
  varPo nv = (varPo) malloc(sizeof(Var) * count);

  for (integer ix = 0; ix < count; ix++)
    nv[ix] = vars[ix];

  return nv;
}

