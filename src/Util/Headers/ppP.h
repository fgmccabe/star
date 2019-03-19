//
// Created by Francis McCabe on 1/2/18.
//

#ifndef STAR_PPP_H
#define STAR_PPP_H

#include "pp.h"
#include "io.h"

typedef struct _display_ {
  ioPo out;
  int indent;
} PPDisplay;

typedef struct _disp_policy_ {
  int indent;
} DisplayPolicy;

#endif //STAR_PPP_H
