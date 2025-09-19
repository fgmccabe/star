//
// Created by Francis McCabe on 3/22/20.
//

#include "topSortP.h"

static integer analyseDef(void *def, lifoPo *defs, lifoPo *stack, lifoPo *groups, findRefProc findRef, void *cl);

static integer analyseRef(void *ref, lifoPo *defs, lifoPo *stack, lifoPo *groups, findRefProc findRef, void *cl,
                          integer low) {
  // Is this reference already in the stack?
  for (int32 ix = 0; ix < lifoCount(*stack); ix++) {
    if (peekElement(*stack, ix) == ref)
      return minimum(low, ix);
  }
  // look in definitions
  for (int32 ix = 0; ix < lifoCount(*defs); ix++) {
    void *def = peekElement(*defs, ix);
    if (def == ref) {
      *defs = dropElement(*defs, ix);
      return minimum(low, analyseDef(def, defs, stack, groups, findRef, cl));
    }
  }
  return low;
}

integer analyseDef(void *def, lifoPo *defs, lifoPo *stack, lifoPo *groups, findRefProc findRef, void *cl) {
  integer pt = lifoCount(*stack);
  *stack = pushElement(def, *stack);

  integer low = pt;
  objectPo ref = findRef(def, cl, 0);

  for (integer ix = 0; ref != Null; ix++, ref = findRef(def, cl, ix)) {
    low = analyseRef(ref, defs, stack, groups, findRef, cl, low);
  }

  if (low < lifoCount(*stack)) {
    lifoPo group = Null;
    while (low < lifoCount(*stack)) {
      void *d = Null;
      *stack = popElement(&d, *stack);
      group = pushElement(d, group);
    }
    *groups = pushElement(group, *groups);
  }
  return low;
}

lifoPo topSort(lifoPo defs, findRefProc findRef, void *cl) {
  lifoPo groups = Null;
  lifoPo stack = Null;

  while (defs != Null) {
    void *d = Null;
    defs = popElement(&d, defs);
    analyseDef(d, &defs, &stack, &groups, findRef, cl);
  }

  decReference(O_OBJECT(stack));
  return groups;
}
