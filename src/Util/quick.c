//
// Created by Francis McCabe on 9/17/23.
//

#include "ooio.h"
#include "quick.h"

static integer partition(integer lo, integer hi, compare cmp, swap swp, void *cl);

retCode quick(integer from, integer to, compare cmp, swap swp, void *cl) {
  if (from >= to)
    return Ok;
  else {
    integer mid = partition(from, to, cmp, swp, cl);

    tryRet(quick(from, mid - 1, cmp, swp, cl));
    return quick(mid + 1, to, cmp, swp, cl);
  }
}

integer partition(integer lo, integer hi, compare cmp, swap swp, void *cl) {
  integer pivot = hi;
  integer i = lo - 1;
  for (integer j = lo; j < hi; j++) {
    if (cmp(j, pivot, cl) != bigger) {
      i++;
      swp(i, j, cl);
    }
  }
  i++;
  swp(i, hi, cl);
  return i;
}
