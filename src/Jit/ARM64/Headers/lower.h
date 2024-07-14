//
// Created by Francis McCabe on 10/5/21.
//

#ifndef STAR_LOWER_H
#define STAR_LOWER_H

#include "config.h"
#include "arm64.h"
#include "code.h"

typedef struct {
  void *rtn;
  methodPo prog;
  void *fp;
} ArmFrame;

#define LONG_COUNT (sizeof(int64))
#define FRAME_SIZE (sizeof(ArmFrame))

#endif //STAR_LOWER_H
