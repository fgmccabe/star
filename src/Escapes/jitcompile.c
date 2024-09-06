//
// Created by Francis McCabe on 9/6/24.
//
// Give direct access to jit compilation

#include "escape.h"
#include "jit.h"
#include "globals.h"

ReturnStatus g__jitcompile(heapPo h, termPo xc, termPo a1) {
  return (ReturnStatus) {.ret=Normal, .result=unitEnum};
}
