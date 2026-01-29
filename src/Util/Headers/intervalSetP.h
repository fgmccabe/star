//
// Created by Francis McCabe on 1/27/26.
//

#ifndef STAR_INTERVALSETP_H
#define STAR_INTERVALSETP_H

#include "config.h"
#include "intervalSet.h"

typedef struct interval_ *intervalPo;

typedef struct interval_ {
  int32 from;
  int32 to;
  intervalPo next;
} IntervalRecord;

typedef struct iset_ {
  intervalPo chain;
} SetRecord;

#endif //STAR_INTERVALSETP_H
