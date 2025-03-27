//
// Created by Francis McCabe on 7/19/20.
//

#ifndef STAR_ARRAYP_H
#define STAR_ARRAYP_H

#include "array.h"

typedef retCode (*arrayGrow)(arrayPo ar,integer request);
typedef void (*arrayRelease)(arrayPo ar);

arrayPo fixedArray(int elSize, integer initial, void *data, arrayGrow grow, arrayRelease release);

typedef struct array_ {
  int elSize;           // Size of each element
  void *data;           // Raw data
  integer dataLength;   // Size of overall data
  integer count;        // How many elements in use
  arrayGrow grow;       // Used to request an extension
  arrayRelease free;    // Used to free memory
} ArrayRecord;

#endif //STAR_ARRAYP_H
