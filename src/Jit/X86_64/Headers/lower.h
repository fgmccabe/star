//
// Created by Francis McCabe on 10/5/21.
//

#ifndef STAR_LOWER_H
#define STAR_LOWER_H

#include "config.h"
#include "x86_64.h"
#include "code.h"

typedef struct {
  void *rtn;
  methodPo prog;
  void *fp;
} X86Frame;

#define LONG_COUNT (sizeof(int64))
#define FRAME_SIZE (sizeof(X86Frame))

#endif //STAR_LOWER_H
