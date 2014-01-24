#ifndef _ITERATE_H_
#define _ITERATE_H_

#include "logical.h"

typedef logical (*hasNext)(void *cl);	/* does iterator have a next element? */
typedef void* (*nextFun)(void *cl);	// next element function

typedef struct {
  hasNext hasNext;
  nextFun next;
} Iterator;

#endif
