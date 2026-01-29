//
// Created by Francis McCabe on 7/19/20.
//

#ifndef STAR_ARRAYP_H
#define STAR_ARRAYP_H

#include "array.h"

typedef retCode (*arrayGrow)(arrayPo ar,int32 request);
typedef void (*arrayRelease)(arrayPo ar);

arrayPo fixedArray(int32 elSize, int32 count, void *data, arrayRelease release);

typedef struct array_ {
  int32 elSize;         // Size of each element
  void *data;           // Raw data
  int32 dataLength;     // Size of overall data
  int32 count;          // How many elements in use
  arrayGrow grow;       // Used to request an extension
  arrayRelease free;    // Used to free memory
} ArrayRecord;

#endif //STAR_ARRAYP_H
