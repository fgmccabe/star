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
  void *fp;
  normalPo pool;
} ArmFrame;

#define LONG_COUNT (sizeof(int64))
#define FRAME_SIZE (sizeof(ArmFrame))

typedef integer (*Cfunc1)(integer arg1);
typedef integer (*Cfunc2)(integer a1, integer a2);
typedef integer (*Cfunc3)(integer a1, integer a2,integer a3);

#endif //STAR_LOWER_H
