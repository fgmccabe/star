//
// Created by Francis McCabe on 1/2/18.
//

#ifndef CAFE_PPP_H
#define CAFE_PPP_H

#include "pp.h"
#include "iochnl.h"

typedef struct _display_ {
  ioPo out;
  int indent;
} PPDisplay;

typedef struct _disp_policy_ {
  int indent;
} DisplayPolicy;

#endif //CAFE_PPP_H
