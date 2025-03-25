//
// Created by Francis McCabe on 10/5/21.
//

#ifndef STAR_LOWER_H
#define STAR_LOWER_H

#include "config.h"
#include "../Assem/Headers/arm64.h"
#include "code.h"

typedef struct {
  void *rtn;
  void *fp;
  normalPo pool;
} ArmFrame;

#define LONG_COUNT (sizeof(int64))
#define FRAME_SIZE (sizeof(ArmFrame))


#endif //STAR_LOWER_H
