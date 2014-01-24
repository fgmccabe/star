#ifndef _SORT_H_
#define _SORT_H_

#include "config.h"
#include "logical.h"

typedef logical (*beforeFn)(void *a,void *b);
typedef void (*swapFn)(void *a,void *b);
typedef void *(*getFn)(void *data,int64 ix);

extern void sort(void *data,int64 count,beforeFn before,swapFn swap,getFn get);

#endif
