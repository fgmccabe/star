//
// Created by Francis McCabe on 9/17/23.
//

#include "ooio.h"
#include "quick.h"

static int32 partition(int32 lo, int32 hi, compare cmp, swap swp, void* cl);

retCode quick(int32 from, int32 to, compare cmp, swap swp, void *cl) {
  if (from >= to)
    return Ok;
  else {
    int32 mid = partition(from, to, cmp, swp, cl);

    tryRet(quick(from, mid - 1, cmp, swp, cl));
    return quick(mid + 1, to, cmp, swp, cl);
  }
}

int32 partition(int32 lo, int32 hi, compare cmp, swap swp, void* cl) {
  int32 pivot = hi;
  int32 i = lo - 1;
  for (int32 j = lo; j < hi; j++) {
    if (cmp(j, pivot, cl) != bigger) {
      i++;
      swp(i, j, cl);
    }
  }
  i++;
  swp(i, hi, cl);
  return i;
}
