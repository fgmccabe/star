//
// Created by Francis McCabe on 3/22/20.
//

#include "topSortP.h"

static integer analyseDef(objectPo def, vectorPo defs, vectorPo stack, vectorPo groups, findRefs getRefs, void *cl);

static integer
analyseRef(objectPo ref, vectorPo defs, vectorPo stack, vectorPo groups, findRefs getRefs, void *cl, integer low) {
// Is this reference already in the stack?
  for (integer ix = 0; ix < vectLength(stack); ix++) {
    if (getVectEl(stack, ix) == ref)
      return minimum(low, ix);
  }
// look in definitions
  for (integer ix = 0; ix < vectLength(defs); ix++) {
    objectPo def = getVectEl(defs, ix);
    if (def == ref) {
      removeVectEl(defs, ix);
      return minimum(low, analyseDef(def, defs, stack, groups, getRefs, cl));
    }
  }
  return
    low;
}

integer analyseDef(objectPo def, vectorPo defs, vectorPo stack, vectorPo groups, findRefs getRefs, void *cl) {
  integer pt = vectLength(stack);
  pushVectEl(stack, O_OBJECT(def));

  integer low = pt;
  vectorPo refs = getRefs(def, cl);

  for (integer ix = 0; ix < vectLength(refs); ix++) {
    low = analyseRef(getVectEl(refs, ix), defs, stack, groups, getRefs, cl, low);
  }
  if (low < vectLength(stack)) {
    vectorPo group = vector(0);
    while (low < vectLength(stack)) {
      objectPo d = popVectEl(stack);
      pushVectEl(group, d);
    }
    pushVectEl(groups, O_OBJECT(group));
  }
  return low;
}

vectorPo topSort(vectorPo defs, findRefs getRefs, void *cl) {
  vectorPo groups = vector(0);
  vectorPo stack = vector(0);

  while (!vectIsEmpty(defs))
    analyseDef(popVectEl(defs), defs, stack, groups, getRefs, cl);

  decReference(O_OBJECT(stack));
  return groups;
}
