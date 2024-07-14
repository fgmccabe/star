//
// Created by Francis McCabe on 7/19/20.
//

#ifndef STAR_ARRAYP_H
#define STAR_ARRAYP_H

#include "array.h"

typedef struct array_ {
  int elSize;           // Size of each element
  void *data;           // Raw data
  integer dataLength;   // Size of overall data
  integer count;        // How many elements in use
  logical growable;     // Is the array allowed to grow
} ArrayRecord;

#endif //STAR_ARRAYP_H
